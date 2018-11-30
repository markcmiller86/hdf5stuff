#ifndef _UDT_DATA_UTILS_H
#define _UDT_DATA_UTILS_H

#include "hdf5.h"

#define MAX_ENTS (1<<20)

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
