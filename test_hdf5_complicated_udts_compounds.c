#include <hdf5.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

/* Test writing a complicated pointer linked data structure with
   multiple User Defined Types (UDTs) to HDF5 datasets */

/* Define three simple User Defined Types (UDTs).
   Top level is doubly linked list of TreeListNodes.
   Each tree List node may point to a TreeNode, which
   is a binary tree. Some binary trees are sparse and
   others are dense. Each binary tree node can point
   ListNode which is a linked list. */
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

/* Stuff to help with pointer->int conversions */
hid_t ln_p, tn_p, tln_p;
TreeListNode_t const *tln_map[64];
TreeNode_t const *tn_map[64];
ListNode_t const *ln_map[64];

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
    root->right->right->list->next = CreateListNodeInstance();

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

/* Data conversion routine callback used by HDF5 during H5Dwrite calls
   to convert memory pointer types to file integer types. Note that
   the conversion "maps" are inefficient using linear lookup. */
herr_t UDTPointerToInt(hid_t srctyp, hid_t dsttyp, H5T_cdata_t *cdata,
    size_t nelmts, size_t buf_stride, size_t bkg_stride,
    void *buf, void *bkg, hid_t plist)
{
    int* foo = (int*) buf;
    if (nelmts <= 0) return 0;
    assert(nelmts == 1);
    assert(sizeof(TreeNode_t*)>=sizeof(int));
    assert(H5Tequal(dsttyp, H5T_NATIVE_INT)>0);
    if (H5Tequal(srctyp, tn_p)>0)
    {
        int i;
        TreeNode_t *p = *((TreeNode_t**) buf);
        printf("UDTPointerToInt for TreeNode_t*: %p\n", p);
        if (p == 0)
        {
            *foo = -1;
            return 0;
        }
        for (i = 0; i < 64; i++)
        {
            if (tn_map[i] == p)
            {
                *foo = i;
                return 0;
            }
        }
        printf("***ERROR***: Can't find TreeNode_t* @ %p\n", p);
        return -1;
    }
    else if (H5Tequal(srctyp, ln_p)>0)
    {
        int i;
        ListNode_t *p = *((ListNode_t**) buf);
        printf("UDTPointerToInt for ListNode_t*: %p\n", p);
        if (p == 0)
        {
            *foo = -1;
            return 0;
        }
        for (i = 0; i < 64; i++)
        {
            if (ln_map[i] == p)
            {
                *foo = i;
                return 0;
            }
        }
        printf("***ERROR***: Can't find ListNode_t* @ %p\n", p);
        return -1;
    }
    else if (H5Tequal(srctyp, tln_p)>0)
    {
        int i;
        TreeListNode_t *p = *((TreeListNode_t**) buf);
        printf("UDTPointerToInt for TreeListNode_t*: %p\n", p);
        if (p == 0)
        {
            *foo = -1;
            return 0;
        }
        for (i = 0; i < 64; i++)
        {
            if (tln_map[i] == p)
            {
                *foo = i;
                return 0;
            }
        }
        printf("***ERROR***: Can't find TreeListNode_t* @ %p\n", p);
        return -1;
    }
    else
        return -1;
    return 0;
}

#define OFFSET(P,F)     ((char*)&((P).F)-(char*)&(P))


//=========================================================================
/* Create memory and file types for the 3 UDTs above */
//=========================================================================
void CreateListNodeTypes(hid_t fid, hid_t *mtype, hid_t *ftype)
{
    ListNode_t dummy;
    hid_t moff, foff;
    hsize_t dims = 3;

    /* memory type */
    moff = H5Tarray_create(H5T_NATIVE_INT, 1, &dims);
    *mtype = H5Tcreate(H5T_COMPOUND, sizeof(ListNode_t));
    H5Tinsert(*mtype, "offsets", OFFSET(dummy, offsets), moff);
    H5Tinsert(*mtype, "a", OFFSET(dummy, a), H5T_NATIVE_FLOAT);
    H5Tinsert(*mtype, "b", OFFSET(dummy, b), H5T_NATIVE_FLOAT);
    H5Tinsert(*mtype, "next", OFFSET(dummy, next), ln_p);
    H5Tclose(moff);

    /* file type */
    foff = H5Tarray_create(H5T_NATIVE_INT, 1, &dims);
    H5Tcommit(fid, "offsets", foff, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    *ftype = H5Tcreate(H5T_COMPOUND, sizeof(ListNode_t));
    H5Tinsert(*ftype, "offsets", OFFSET(dummy, offsets), foff);
    H5Tinsert(*ftype, "a", OFFSET(dummy, a), H5T_NATIVE_FLOAT);
    H5Tinsert(*ftype, "b", OFFSET(dummy, b), H5T_NATIVE_FLOAT);
    H5Tinsert(*ftype, "next", OFFSET(dummy, next), H5T_NATIVE_INT);
    H5Tcommit(fid, "ListNode_t", *ftype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Tclose(foff);
}

void CreateTreeNodeTypes(hid_t fid, hid_t *mtype, hid_t *ftype)
{
    TreeNode_t dummy;
    hid_t mcoords, fcoords;
    hsize_t dims = 3;

    /* memory type */
    mcoords = H5Tarray_create(H5T_NATIVE_DOUBLE, 1, &dims);
    *mtype = H5Tcreate(H5T_COMPOUND, sizeof(TreeNode_t));
    H5Tinsert(*mtype, "memberA", OFFSET(dummy, memberA), H5T_NATIVE_INT);
    H5Tinsert(*mtype, "memberB", OFFSET(dummy, memberB), H5T_NATIVE_INT);
    H5Tinsert(*mtype, "coords", OFFSET(dummy, coords), mcoords);
    H5Tinsert(*mtype, "left", OFFSET(dummy, left), tn_p);
    H5Tinsert(*mtype, "right", OFFSET(dummy, right), tn_p);
    H5Tinsert(*mtype, "list", OFFSET(dummy, list), ln_p);
    H5Tclose(mcoords);

    /* file type */
    fcoords = H5Tarray_create(H5T_NATIVE_DOUBLE, 1, &dims);
    H5Tcommit(fid, "coords", fcoords, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    *ftype = H5Tcreate(H5T_COMPOUND, sizeof(TreeNode_t));
    H5Tinsert(*ftype, "memberA", OFFSET(dummy, memberA), H5T_NATIVE_INT);
    H5Tinsert(*ftype, "memberB", OFFSET(dummy, memberB), H5T_NATIVE_INT);
    H5Tinsert(*ftype, "coords", OFFSET(dummy, coords), fcoords);
    H5Tinsert(*ftype, "left", OFFSET(dummy, left), H5T_NATIVE_INT);
    H5Tinsert(*ftype, "right", OFFSET(dummy, right), H5T_NATIVE_INT);
    H5Tinsert(*ftype, "list", OFFSET(dummy, list), H5T_NATIVE_INT);
    H5Tcommit(fid, "TreeNode_t", *ftype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Tclose(fcoords);
}

void CreateTreeListNodeTypes(hid_t fid, hid_t *mtype, hid_t *ftype)
{
    TreeListNode_t dummy;
    hid_t mname, fname;
    hsize_t dims = 32;

    /* memory type */
    mname = H5Tcopy(H5T_C_S1);
    H5Tset_size(mname, sizeof(dummy.name));
    *mtype = H5Tcreate(H5T_COMPOUND, sizeof(TreeListNode_t));
    H5Tinsert(*mtype, "name", OFFSET(dummy, name), mname);
    H5Tinsert(*mtype, "val1", OFFSET(dummy, val1), H5T_NATIVE_INT);
    H5Tinsert(*mtype, "val2", OFFSET(dummy, val2), H5T_NATIVE_DOUBLE);
    H5Tinsert(*mtype, "val3", OFFSET(dummy, val3), H5T_NATIVE_FLOAT);
    H5Tinsert(*mtype, "next", OFFSET(dummy, next), tln_p);
    H5Tinsert(*mtype, "prev", OFFSET(dummy, prev), tln_p);
    H5Tinsert(*mtype, "tree", OFFSET(dummy, tree), tn_p);
    H5Tclose(mname);

    fname = H5Tcopy(H5T_C_S1);
    H5Tset_size(fname, sizeof(dummy.name));
    H5Tcommit(fid, "name", fname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    *ftype = H5Tcreate(H5T_COMPOUND, sizeof(TreeListNode_t));
    H5Tinsert(*ftype, "name", OFFSET(dummy, name), fname);
    H5Tinsert(*ftype, "val1", OFFSET(dummy, val1), H5T_NATIVE_INT);
    H5Tinsert(*ftype, "val2", OFFSET(dummy, val2), H5T_NATIVE_DOUBLE);
    H5Tinsert(*ftype, "val3", OFFSET(dummy, val3), H5T_NATIVE_FLOAT);
    H5Tinsert(*ftype, "next", OFFSET(dummy, next), H5T_NATIVE_INT);
    H5Tinsert(*ftype, "prev", OFFSET(dummy, prev), H5T_NATIVE_INT);
    H5Tinsert(*ftype, "tree", OFFSET(dummy, tree), H5T_NATIVE_INT);
    H5Tcommit(fid, "TreeListNode_t", *ftype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Tclose(fname);
}

/* Traversal prep to count occurrences of a given type and copy their
   pointers to a map */
void TraverseListNodeInPreparationForWriting(ListNode_t const *node,
    int *nln, ListNode_t const *ln_map[],
    int *ntn, TreeNode_t const *tn_map[],
    int *ntln, TreeListNode_t const *tln_map[])
{
    while (node)
    {
        ln_map[*nln] = node; (*nln)++;
        node = node->next;
    }
}

void TraverseTreeNodeInPreparationForWriting(TreeNode_t const *node,
    int *nln, ListNode_t const *ln_map[],
    int *ntn, TreeNode_t const *tn_map[],
    int *ntln, TreeListNode_t const *tln_map[])
{
    tn_map[*ntn] = node; (*ntn)++;
    if (node->list)
        TraverseListNodeInPreparationForWriting(node->list,
            nln, ln_map, ntn, tn_map, ntln, tln_map);
    if (node->left)
        TraverseTreeNodeInPreparationForWriting(node->left,
            nln, ln_map, ntn, tn_map, ntln, tln_map);
    if (node->right)
        TraverseTreeNodeInPreparationForWriting(node->right,
            nln, ln_map, ntn, tn_map, ntln, tln_map);
}


//=========================================================================
void TraverseUDTInPreparationForWriting(TreeListNode_t const *node,
    int *nln, ListNode_t const *ln_map[],
    int *ntn, TreeNode_t const *tn_map[],
    int *ntln, TreeListNode_t const *tln_map[])
{
    *nln = *ntn = *ntln = 0;
    while (node)
    {
        tln_map[*ntln] = node; (*ntln)++; 
        if (node->tree)
            TraverseTreeNodeInPreparationForWriting(node->tree,
                nln, ln_map, ntn, tn_map, ntln, tln_map);
        node = node->next;
    }
}


//=========================================================================
/* Traversal routines to do the actual writing */
//=========================================================================
void TraverseListNodeAndWrite(hid_t fid, ListNode_t const *node,
    int *nln, hid_t ln_m, hid_t lnspaceid, hid_t lnsetid)
{
    hsize_t hdimm = 1;
    hid_t spidm = H5Screate_simple(1, &hdimm, 0);
    H5Sselect_all(spidm);
    while (node)
    {
        hsize_t coord = *nln;
        (*nln)++;
        H5Sselect_none(lnspaceid);
        H5Sselect_elements(lnspaceid, H5S_SELECT_SET, 1, &coord);
        H5Dwrite(lnsetid, ln_m, spidm, lnspaceid, H5P_DEFAULT, node);
        node = node->next;
    }
    H5Sclose(spidm);
}


//=========================================================================
void TraverseTreeNodeAndWrite(hid_t fid, TreeNode_t const *node,
    int *nln, hid_t ln_m, hid_t lnspaceid, hid_t lnsetid,
    int *ntn, hid_t tn_m, hid_t tnspid, hid_t tnid)
{
    hsize_t hdimm = 1;
    hid_t spidm = H5Screate_simple(1, &hdimm, 0);
    hsize_t coord = *ntn;

    H5Sselect_all(spidm);
    (*ntn)++;
    H5Sselect_none(tnspid);
    H5Sselect_elements(tnspid, H5S_SELECT_SET, 1, &coord);
    H5Dwrite(tnid, tn_m, spidm, tnspid, H5P_DEFAULT, node);
    if (node->list)
        TraverseListNodeAndWrite(fid, node->list, nln, ln_m, lnspaceid, lnsetid);
    if (node->left)
        TraverseTreeNodeAndWrite(fid, node->left, nln, ln_m, lnspaceid, lnsetid, ntn, tn_m, tnspid, tnid);
    if (node->right)
        TraverseTreeNodeAndWrite(fid, node->right, nln, ln_m, lnspaceid, lnsetid, ntn, tn_m, tnspid, tnid);
    H5Sclose(spidm);
}


//=========================================================================
void TraverseUDTAndWrite(hid_t fid, TreeListNode_t const *node,
    int *nln, hid_t ln_m, hid_t lnspaceid, hid_t lnsetid,
    int *ntn, hid_t tn_m, hid_t tnspid, hid_t tnid,
    int *ntln, hid_t tln_m, hid_t tlnspid, hid_t tlnid)
{
    hsize_t hdimm = 1;
    hid_t spidm = H5Screate_simple(1, &hdimm, 0);

    H5Sselect_all(spidm);
    *nln = *ntn = *ntln = 0;
    while (node)
    {
        hsize_t coord = *ntln;
        (*ntln)++;
        H5Sselect_none(tlnspid);
        H5Sselect_elements(tlnspid, H5S_SELECT_SET, 1, &coord);
        H5Dwrite(tlnid, tln_m, spidm, tlnspid, H5P_DEFAULT, node);
        if (node->tree) {
            TraverseTreeNodeAndWrite(fid, node->tree,
                nln, ln_m, lnspaceid, lnsetid,
                ntn, tn_m, tnspid, tnid);
        }
        node = node->next;
    }
    H5Sclose(spidm);
}


//=========================================================================
int main(int argc, char **argv)
{
    TreeListNode_t *head;

    hid_t fid, tgid, spid;
    hid_t ln_m, ln_f, tn_m, tn_f, tln_m, tln_f;
    hid_t lnsetid, tndsid, tlndsid;
    hid_t lnspaceid, tnspid, tlnspid;
    hsize_t hdim;
    int i, nln, ntn, ntln;

    /* create some data to write */
    head = CreateUDTData();

    /* traverse and print the data for debug purposes */
    PrintUDTData(head);

    /* Create the HDF5 file */
    fid = H5Fcreate("test_hdf5_udt_compounds_c.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    tgid = H5Gcreate(fid, "Types", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Create memory POINTER types. These are never committed and are only
       used to facilitate the conversion process on write and read.
       The associated file types will be a suitable integer type */
    ln_p = H5Tcreate(H5T_OPAQUE, sizeof(ListNode_t*));
    H5Tset_tag(ln_p, "ListNode_t*");
    tn_p = H5Tcreate(H5T_OPAQUE, sizeof(TreeNode_t*));
    H5Tset_tag(tn_p, "TreeNode_t*");
    tln_p = H5Tcreate(H5T_OPAQUE, sizeof(TreeListNode_t*));
    H5Tset_tag(tln_p, "TreeListNode_t*");
    H5Tlock(ln_p);
    H5Tlock(tn_p);
    H5Tlock(tln_p);
    /* same conversion routine for all 3 pointer types */
    H5Tregister(H5T_PERS_HARD, "UDT*->int", tln_p, H5T_NATIVE_INT, UDTPointerToInt);
    H5Tregister(H5T_PERS_HARD, "UDT*->int", tn_p, H5T_NATIVE_INT, UDTPointerToInt);
    H5Tregister(H5T_PERS_HARD, "UDT*->int", ln_p, H5T_NATIVE_INT, UDTPointerToInt);

    /* Create memory and file types for our 3 user defined types */
    CreateListNodeTypes(tgid, &ln_m, &ln_f);
    CreateTreeNodeTypes(tgid, &tn_m, &tn_f);
    CreateTreeListNodeTypes(tgid, &tln_m, &tln_f);

    TraverseUDTInPreparationForWriting(head,
        &nln, ln_map, &ntn, tn_map, &ntln, tln_map);

#ifdef DEBUG
    printf("nln = %d, ntn = %d, ntln = %d\n", nln, ntn, ntln);
    printf("\nList Node Map:");
    for (i = 0; i < nln; i++)
        printf("%s%p", i%5?", ":"\n    ", ln_map[i]);
    printf("\nTree Node Map:");
    for (i = 0; i < ntn; i++)
        printf("%s%p", i%5?", ":"\n    ", tn_map[i]);
    printf("\nTree List Node Map:");
    for (i = 0; i < ntln; i++)
        printf("%s%p", i%5?", ":"\n    ", tln_map[i]);
    printf("\n");
#endif

    /* Create Datasets; one for each UDT */
    hdim = (hsize_t) nln;
    lnspaceid = H5Screate_simple(1, &hdim, 0);
    lnsetid = H5Dcreate(fid, "ListNode_data", ln_f, lnspaceid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    hdim = (hsize_t) ntn;
    tnspid = H5Screate_simple(1, &hdim, 0);
    tndsid = H5Dcreate(fid, "TreeNode_data", tn_f, tnspid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    hdim = (hsize_t) ntln;
    tlnspid = H5Screate_simple(1, &hdim, 0);
    tlndsid = H5Dcreate(fid, "TreeListNode_data", tln_f, tlnspid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Do the actual traversal and writing. Note, pointer->int conversions happen
       automatically as hdf5 encounters the need due to src and dst type mismatches. */
    TraverseUDTAndWrite(fid, head,
        &nln, ln_m, lnspaceid, lnsetid,
        &ntn, tn_m, tnspid, tndsid,
        &ntln, tln_m, tlnspid, tlndsid);

#ifdef DEBUG
    PrintUDTData(head);
#endif

    H5Sclose(lnspaceid);
    H5Sclose(tnspid);
    H5Sclose(tlnspid);
    H5Dclose(lnsetid);
    H5Dclose(tndsid);
    H5Dclose(tlndsid);
    H5Gclose(tgid);
    H5Fclose(fid);

    return 0;
}
