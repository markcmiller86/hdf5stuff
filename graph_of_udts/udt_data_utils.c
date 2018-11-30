#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hdf5.h"

#include "udt_data_utils.h"

/* Stuff to help with pointer->int conversions */
hid_t ln_p, tn_p, tln_p;
TreeListNode_t **tln_map;
TreeNode_t **tn_map;
ListNode_t **ln_map;

/* Data creation routines just to create some useable
   data that I can easily understand with h5ls */
ListNode_t *CreateListNodeInstance()
{
    static int count = 0;
    ListNode_t *retval = (ListNode_t *) calloc(1, sizeof(ListNode_t));
    retval->offsets[0] = count;
    retval->offsets[1] = 2 * count;
    retval->offsets[2] = 3 * count;
    retval->a = sqrt(count);
    retval->b = 2 * retval->a;
    count++;
    return retval;
}

TreeNode_t *CreateTreeNodeInstance()
{
    static int count = 0;
    TreeNode_t *retval = (TreeNode_t*) calloc(1, sizeof(TreeNode_t));
    retval->memberA = count;
    retval->memberB =  2 * retval->memberA;
    retval->coords[0] = (double) count;
    retval->coords[1] = (double) 2 * count;
    retval->coords[2] = (double) 3 * count;
    count++;
    return retval;
}

TreeListNode_t *CreateTreeListNodeInstance()
{
    static int count = 0;
    char name[32];
    TreeListNode_t *retval = (TreeListNode_t*) calloc(1, sizeof(TreeListNode_t));
    snprintf(name, sizeof(name), "TreeListNode_%03d", count);
    strcpy(retval->name, name);
    retval->val1 = count;
    retval->val2 = count*count;
    retval->val3 = 3.14159265359*count;
    count++;
    return retval;
}

/* Top level data creation routine which creates an arbitrary
   arrangement of UDTs */
TreeListNode_t *CreateUDTData()
{
    TreeListNode_t *treeListHead, *treeList0, *treeList1, *treeList2;
    TreeNode_t *root, *treeNodeTmp;
    ListNode_t *head, *listNodeTmp;

    tln_map = (TreeListNode_t **) calloc(64, sizeof(TreeListNode_t*));
    tn_map = (TreeNode_t **) calloc(64, sizeof(TreeNode_t*));
    ln_map = (ListNode_t **) calloc(64, sizeof(ListNode_t*));

    /* top level is 3 entries of "trees" */
    treeList0 = CreateTreeListNodeInstance();
    treeList1 = CreateTreeListNodeInstance();
    treeList2 = CreateTreeListNodeInstance();
    treeListHead = treeList0;

    treeList0->next = treeList1;
    treeList1->prev = treeList0;
    treeList1->next = treeList2;
    treeList2->prev = treeList1;

    /* Tree 0; just two children and no lists in either */
    root = CreateTreeNodeInstance();
    treeList0->tree = root;
    root->left = CreateTreeNodeInstance();
    root->right = CreateTreeNodeInstance();

    /* Tree 1; unbalanced, a few levels, some with lists */
    root = CreateTreeNodeInstance();
    treeList1->tree = root;
    root->left = CreateTreeNodeInstance();
    root->left->right = CreateTreeNodeInstance();
    root->left->right->list = CreateListNodeInstance();
    root->left->right->list->next = CreateListNodeInstance();
    root->left->right->list->next->next = CreateListNodeInstance();
    root->left->right->list->next->next->next = CreateListNodeInstance();
    root->left->left = CreateTreeNodeInstance();
    root->left->left->left = CreateTreeNodeInstance();
    root->left->left->left->left = CreateTreeNodeInstance();
    root->left->left->left->left->list = CreateListNodeInstance();
    root->left->left->left->left->list->next = CreateListNodeInstance();
    root->left->left->left->left->list->next->next = CreateListNodeInstance();
    root->left->left->left->left->list->next->next->next = CreateListNodeInstance();
    root->left->left->left->left->list->next->next->next->next = CreateListNodeInstance();
    root->left->left->left->right = CreateTreeNodeInstance();
    root->right = CreateTreeNodeInstance();
    root->right->right = CreateTreeNodeInstance();
    root->right->right->list = CreateListNodeInstance();
    root->right->right->list = CreateListNodeInstance();

    /* Tree 2, more stuff */
    root = CreateTreeNodeInstance();
    treeList2->tree = root;
    root->left = CreateTreeNodeInstance();
    root->left->list = CreateListNodeInstance();
    root->right = CreateTreeNodeInstance();
    root->right->list = CreateListNodeInstance();
    root->right->list->next = CreateListNodeInstance();
    root->right->list->next->next = CreateListNodeInstance();
    root->left->left = CreateTreeNodeInstance();
    root->left->left->list = CreateListNodeInstance();
    root->left->right = CreateTreeNodeInstance();
    root->left->right->list = CreateListNodeInstance();
    root->right->left = CreateTreeNodeInstance();
    root->right->left->list = CreateListNodeInstance();
    root->right->right = CreateTreeNodeInstance();
    root->right->right->list = CreateListNodeInstance();

    /* Several more empty tree list nodes at the end */
    treeList2->next = CreateTreeListNodeInstance();
    treeList2->next->prev = treeList2;

    treeList2->next->next = CreateTreeListNodeInstance();
    treeList2->next->next->prev = treeList2->next;

    treeList2->next->next->next = CreateTreeListNodeInstance();
    treeList2->next->next->next->prev = treeList2->next->next;

    return treeListHead;
}

static ListNode_t *CreateListNodesRandom(int *nentities)
{
    int i;
    int nentstmp = *nentities;
    int nln = nentstmp > 5 ? random() % (nentstmp/2) : nentstmp;
    ListNode_t *ln;

    if (nln <= 0) return 0;

printf("random list of size %d %d\n", *nentities, nln);
    ln = CreateListNodeInstance();
    for (i = 1; i < nln; i++)
    {
        ln->next = CreateListNodeInstance();
        ln = ln->next;
    }

    *nentities -= nln;
    
    return ln;
}

static TreeNode_t *CreateTreeNodesRandom(int *nentities)
{
    int i;
    int nentstmp = *nentities;
    int ntns_max = nentstmp > 2 ? nentstmp / 2 : 1;
    int ntns = random() % ntns_max + 1;
    int nleftover = nentstmp - ntns;
    TreeNode_t **tn_last, *tnroot;

    tn_last = (TreeNode_t **) calloc(ntns, sizeof(TreeNode_t*));
    for (i = 0; i < ntns; i++)
    {
        TreeNode_t *tn_next = CreateTreeNodeInstance();
        tn_next->list = CreateListNodesRandom(&nleftover);
        tn_last[i] = tn_next;
        if (i == 0)
        {
            tnroot = tn_next;
            continue;
        }
        /*if (random() % 2)*/
        if (i % 2)
            tn_last[i/2  ]->left = tn_next;
        else
            tn_last[i/2-1]->right = tn_next;
    }
    free(tn_last);

    *nentities -= ntns;

    return tnroot;
}

TreeListNode_t *CreateUDTDataRandom(int nentities)
{
    int i;
    int ntlns_max = nentities > 100 ? nentities / 100 : 1;
    int ntlns = random() % ntlns_max + 1;
    int nleftover = nentities - ntlns;
    TreeListNode_t *tln_last, *tlnroot;

    assert(nentities < MAX_ENTS);

    /* huge over-alloc but who cares */
    tln_map = (TreeListNode_t **) calloc(nentities, sizeof(TreeListNode_t*));
    tn_map = (TreeNode_t **) calloc(nentities, sizeof(TreeNode_t*));
    ln_map = (ListNode_t **) calloc(nentities, sizeof(ListNode_t*));

    for (i = 0; i < ntlns; i++)
    {
        TreeListNode_t *tln_next = CreateTreeListNodeInstance();
        tln_next->tree = CreateTreeNodesRandom(&nleftover);
        if (i == 0)
        {
            tlnroot = tln_next;
            tln_last = tln_next;
            continue;
        }
        tln_last->next = tln_next;
        tln_next->prev = tln_last;
        tln_last = tln_next;
    }

    return tlnroot;
}

/* Print routines for debugging */
void PrintListNode(int indent, ListNode_t const *node)
{
    while (node)
    {
        printf("%*sListNode_t @ %p\n", 2+indent*2, "", node);
        printf("%*s  offsets = %d,%d,%d\n", 2+indent*3, "",
            node->offsets[0], node->offsets[1], node->offsets[2]);
        printf("%*s  a = %g\n", 2+indent*3, "", node->a);
        printf("%*s  b = %g\n", 2+indent*3, "", node->b);
        printf("%*s  next = %p\n", 2+indent*3, "", node->next);
        node = node->next;
    }
}

void PrintTreeNode(int indent, TreeNode_t const *node)
{
    int                 memberA;
    int                 memberB;
    double              coords[3];
    struct _TreeNode_t *left;
    struct _TreeNode_t *right;
    struct _ListNode_t *list;
    
    printf("%*sTreeNode_t @ %p\n", 2+indent*2, "", node);
    printf("%*s  memberA = %d\n", 2+indent*2, "", node->memberA);
    printf("%*s  memberB = %d\n", 2+indent*2, "", node->memberB);
    printf("%*s  coords = %g,%g,%g\n", 2+indent*2, "",
        node->coords[0], node->coords[1], node->coords[2]);
    printf("%*s  list = %p\n", 2+indent*2, "", node->list);
    if (node->list)
        PrintListNode(indent, node->list);
    printf("%*s  left = %p\n", 2+indent*2, "", node->left);
    if (node->left)
        PrintTreeNode(indent+1, node->left);
    printf("%*s  right = %p\n", 2+indent*2, "", node->right);
    if (node->right)
        PrintTreeNode(indent+1, node->right);
}


//=========================================================================
void PrintTreeListNode(TreeListNode_t const *node)
{
    printf("TreeListNode_t @ %p\n", node);
    printf("  name = \"%s\"\n", node->name);
    printf("  val1 = %d\n", node->val1);
    printf("  val2 = %g\n", node->val2);
    printf("  val3 = %g\n", node->val3);
    printf("  next = %p\n", node->next);
    printf("  prev = %p\n", node->prev);
    printf("  tree = %p\n", node->tree);
    if (node->tree)
        PrintTreeNode(0, node->tree);
}


//=========================================================================
void PrintUDTData(TreeListNode_t const *root)
{
    while (root)
    {
        PrintTreeListNode(root);
        root = root->next;
    }
}

void FreeUDTData()
{
    free(tln_map);
    free(tn_map);
    free(ln_map);
}
