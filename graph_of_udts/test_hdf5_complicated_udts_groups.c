#include <hdf5.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

/* Test writing a complicated pointer linked data structure with
   pointers to User Defined Types (UDTs) represented via HDF groups */

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


//=========================================================================
/* Create memory and file types for the 3 UDTs above */
//=========================================================================
#define OFFSET(P,F)     ((char*)&((P).F)-(char*)&(P))

void CreateListNodeTypes(hid_t fid, hid_t *ftype)
{
    ListNode_t dummy;
    hid_t foff;
    hsize_t dims = 3;

    /* file type */
    foff = H5Tarray_create(H5T_NATIVE_INT, 1, &dims);
    H5Tcommit(fid, "offsets", foff, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    *ftype = H5Tcreate(H5T_COMPOUND, sizeof(ListNode_t));
    H5Tinsert(*ftype, "offsets", OFFSET(dummy, offsets), foff);
    H5Tinsert(*ftype, "a", OFFSET(dummy, a), H5T_NATIVE_FLOAT);
    H5Tinsert(*ftype, "b", OFFSET(dummy, b), H5T_NATIVE_FLOAT);
    H5Tcommit(fid, "ListNode_t", *ftype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Tclose(foff);
}

void CreateTreeNodeTypes(hid_t fid, hid_t *ftype)
{
    TreeNode_t dummy;
    hid_t fcoords;
    hsize_t dims = 3;

    /* file type */
    fcoords = H5Tarray_create(H5T_NATIVE_DOUBLE, 1, &dims);
    H5Tcommit(fid, "coords", fcoords, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    *ftype = H5Tcreate(H5T_COMPOUND, sizeof(TreeNode_t));
    H5Tinsert(*ftype, "memberA", OFFSET(dummy, memberA), H5T_NATIVE_INT);
    H5Tinsert(*ftype, "memberB", OFFSET(dummy, memberB), H5T_NATIVE_INT);
    H5Tinsert(*ftype, "coords", OFFSET(dummy, coords), fcoords);
    H5Tcommit(fid, "TreeNode_t", *ftype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Tclose(fcoords);
}

void CreateTreeListNodeTypes(hid_t fid, hid_t *ftype)
{
    TreeListNode_t dummy;
    hid_t fname;
    hsize_t dims = 32;

    /* file type */
    fname = H5Tcopy(H5T_C_S1);
    H5Tset_size(fname, sizeof(dummy.name));
    H5Tcommit(fid, "name", fname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    *ftype = H5Tcreate(H5T_COMPOUND, sizeof(TreeListNode_t));
    H5Tinsert(*ftype, "name", OFFSET(dummy, name), fname);
    H5Tinsert(*ftype, "val1", OFFSET(dummy, val1), H5T_NATIVE_INT);
    H5Tinsert(*ftype, "val2", OFFSET(dummy, val2), H5T_NATIVE_DOUBLE);
    H5Tinsert(*ftype, "val3", OFFSET(dummy, val3), H5T_NATIVE_FLOAT);
    H5Tcommit(fid, "TreeListNode_t", *ftype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Tclose(fname);
}


//=========================================================================
/* Traversal routines to do the actual writing */
//=========================================================================
void TraverseListNodeAndWrite(hid_t locid, ListNode_t const *node, hid_t ln_f)
{
    char name[32];
    hid_t gid,spaceid,setid;

    while (node)
    {
        /* create a group for the current guy */
        sprintf(name,"ListNode_%03d",node->offsets[0]);
        gid=H5Gcreate1(locid,name,0);
        /* create the dataspace */
        spaceid=H5Screate(H5S_SCALAR);
        /* create the dataset */
        setid=H5Dcreate(gid,"ListNode_data",ln_f,spaceid,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
        /* do it */
        H5Dwrite(setid,ln_f,H5S_ALL,spaceid,H5P_DEFAULT,node);
        /* close the dataset */
        H5Dclose(setid);
        /* close the dataspace */
        H5Sclose(spaceid);

        /* close the group for the current guy */
        H5Gclose(gid);

        /* proceed */
        node = node->next;
    }
}


//=========================================================================
void TraverseTreeNodeAndWrite(hid_t locid,TreeNode_t const *node, hid_t ln_f, hid_t tn_f)
{
    char name[32];
    hid_t gid,spaceid,setid;

    /* create a group for the current guy */
    sprintf(name,"TreeNode_%03d",node->memberA);
    gid=H5Gcreate1(locid,name,0);

    /* create the dataspace */
    spaceid=H5Screate(H5S_SCALAR);
    /* create the dataset */
    setid=H5Dcreate(gid,"TreeNode_data",tn_f,spaceid,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
    /* do it */
    H5Dwrite(setid,tn_f,H5S_ALL,spaceid,H5P_DEFAULT,node);
    /* close the dataset */
    H5Dclose(setid);
    /* close the dataspace */
    H5Sclose(spaceid);

    if (node->list)
        TraverseListNodeAndWrite(gid, node->list, ln_f);
    if (node->left)
        TraverseTreeNodeAndWrite(locid, node->left, ln_f, tn_f);
    if (node->right)
        TraverseTreeNodeAndWrite(locid, node->right, ln_f, tn_f);

    /* close the group for the current guy */
    H5Gclose(gid);
}


//=========================================================================
void TraverseUDTAndWrite(hid_t locid, TreeListNode_t const *node,
    hid_t ln_f, hid_t tn_f, hid_t tln_f)
{
    hid_t gid,spaceid,setid;

    while (node)
    {
        /* create a group for the current guy */
        gid=H5Gcreate1(locid,node->name,0);

        /* create the dataspace */
        spaceid=H5Screate(H5S_SCALAR);
        /* create the dataset */
        setid=H5Dcreate(gid,"TreeListNode_data",tln_f,spaceid,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
        /* do it */
        H5Dwrite(setid,tln_f,H5S_ALL,spaceid,H5P_DEFAULT,node);
        /* close the dataset */
        H5Dclose(setid);
        /* close the dataspace */
        H5Sclose(spaceid);

        if (node->tree) {
            TraverseTreeNodeAndWrite(gid,node->tree, ln_f, tn_f);
        }

        /* close the group for the current guy */
        H5Gclose(gid);

        /* proceed */
        node = node->next;
    }
}


//=========================================================================
int main(int argc, char **argv)
{
    TreeListNode_t *head;

    hid_t fid, tgid;
    hid_t ln_f, tn_f, tln_f;

    /* create some data to write */
    head = CreateUDTData();

    /* traverse and print the data for debug purposes */
    PrintUDTData(head);

    /* Create the HDF5 file */
    fid = H5Fcreate("test_hdf5_udt_groups.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    tgid = H5Gcreate(fid, "Types", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Create memory and file types for our 3 user defined types */
    CreateListNodeTypes(tgid, &ln_f);
    CreateTreeNodeTypes(tgid, &tn_f);
    CreateTreeListNodeTypes(tgid, &tln_f);

    /* Do the traversal and writing. */
    /* In this case, dataspace cereation, dataset creation, and data writing
     * all done in the same location. */
    TraverseUDTAndWrite(fid, head, ln_f, tn_f, tln_f);

#ifdef DEBUG
    PrintUDTData(head);
#endif

    H5Gclose(tgid);
    H5Fclose(fid);

    return 0;
}
