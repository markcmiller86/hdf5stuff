#ifndef _UDT_DATA_UTILS_H
#define _UDT_DATA_UTILS_H

#include "hdf5.h"

#define MAX_ENTS (1<<20)

/* Useful Macro for defining CL args and CL help */
#define ADD_ARG(TYPE,NAME,DEFVAL,SPEC,HELPSTR)                  \
    TYPE NAME = DEFVAL;                                         \
    {                                                           \
        char tmpstr[128];                                       \
        int len = strlen(#NAME)+1;                              \
        int _help = 0;                                          \
        int i;                                                  \
        if (!strncmp(#NAME, "help", 4))                         \
        {                                                       \
            for (i = 1; i < argc; i++)                          \
            {                                                   \
                if (strcasestr(argv[i],"help"))                 \
                {                                               \
                    _help = 1;                                  \
                    if (i < argc - 1)                           \
                    {                                           \
                        char *tmp = argv[argc-1];               \
                        argv[argc-1] = argv[i];                 \
                        argv[i] = tmp;                          \
                    }                                           \
                    argc--;                                     \
                    break;                                      \
                }                                               \
            }                                                   \
        }                                                       \
        for (i = 1; i < argc; i++)                              \
        {                                                       \
            if (!strncmp(argv[i], #NAME"=", len))               \
            {                                                   \
                if (strchr(#TYPE,'*'))                          \
                    NAME=(TYPE) (argv[i]+len);                  \
                else                                            \
                    sscanf(argv[i]+len,SPEC,&NAME);             \
                if (i < argc - 1)                               \
                {                                               \
                    char *tmp = argv[argc-1];                   \
                    argv[argc-1] = argv[i];                     \
                    argv[i] = tmp;                              \
                }                                               \
                argc--;                                         \
                break;                                          \
            }                                                   \
        }                                                       \
        len = snprintf(tmpstr, sizeof(tmpstr), "%s="SPEC, #NAME, NAME);\
        printf("    %s%*s\n",tmpstr,80-len,HELPSTR);            \
        if (!strncmp(#NAME, "help", 4))                         \
        {                                                       \
            if (_help) return 1;                                \
            if (argc > 1)                                       \
            {                                                   \
                printf("unrecognized arguments...\n");          \
                for (i = 1; i < argc; i++)                      \
                    printf("    \"%s\"\n", argv[i]);            \
                return 1;                                       \
            }                                                   \
        }                                                       \
    }

#ifdef __cplusplus
extern "C" {
#endif

/* Define some simple User Defined Types (UDTs).
   out of which we can create a somewhat interesting
   graph/tree */
typedef struct _ListNode_t {
     int                 offsets[3];
     float               a;
     float               b;
     struct _ListNode_t *next;
} ListNode_t;

typedef struct _TreeNode_t {
    int                 memberA;
    int                 memberB;
    double              coords[3];
    struct _TreeNode_t *left;
    struct _TreeNode_t *right;
    struct _ListNode_t *list;
} TreeNode_t;

typedef struct _TreeListNode_t {
    char                    name[32];
    int                     val1;
    double                  val2;
    float                   val3;
    struct _TreeListNode_t *next;
    struct _TreeListNode_t *prev;
    struct _TreeNode_t *tree;
} TreeListNode_t;

extern hid_t ln_p, tn_p, tln_p;
extern TreeListNode_t **tln_map;
extern TreeNode_t **tn_map;
extern ListNode_t **ln_map;

/* Data creation routines */
extern ListNode_t *CreateListNodeInstance();
extern TreeNode_t *CreateTreeNodeInstance();
extern TreeListNode_t *CreateTreeListNodeInstance();
extern TreeListNode_t *CreateUDTData();
extern TreeListNode_t *CreateUDTDataRandom(int nentities);
extern void FreeUDTData();

/* print/debug routines */
extern void PrintListNode(int indent, ListNode_t const *node);
extern void PrintTreeNode(int indent, TreeNode_t const *node);
extern void PrintTreeListNode(TreeListNode_t const *node);
extern void PrintUDTData(TreeListNode_t const *root);

/* HDF5 run-time type conversion routine */
extern herr_t UDTPointerToInt(hid_t srctyp, hid_t dsttyp,
    H5T_cdata_t *cdata, size_t nelmts, size_t buf_stride,
    size_t bkg_stride, void *buf, void *bkg, hid_t plist);

#ifdef __cplusplus
}
#endif

#endif
