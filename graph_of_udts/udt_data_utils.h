#ifndef _UDT_DATA_UTILS_H
#define _UDT_DATA_UTILS_H

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

/* Data creation routines */
extern ListNode_t *CreateListNodeInstance();
extern TreeNode_t *CreateTreeNodeInstance();
extern TreeListNode_t *CreateTreeListNodeInstance();
extern TreeListNode_t *CreateUDTData();

/* print/debug routines */
extern void PrintListNode(int indent, ListNode_t const *node);
extern void PrintTreeNode(int indent, TreeNode_t const *node);
extern void PrintTreeListNode(TreeListNode_t const *node);
extern void PrintUDTData(TreeListNode_t const *root);

#ifdef __cplusplus
}
#endif

#endif
