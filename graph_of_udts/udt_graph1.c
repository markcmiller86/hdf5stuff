#include "udt_data_utils.h"

#include <hdf5.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

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

    ADD_ARG(unsigned, seed,  0xDeadBeef, "0x%X", "random number generator");
    ADD_ARG(int,      nrandom,  0, "%d", "number of entities in randomized data gen");
    ADD_ARG(char*,    h5file, "udt_graph1.h5", "\"%s\"", "name of hdf5 file");
    ADD_ARG(int,      help,  0, "%d", "this help message");

    /* create some data to write */
    if (nrandom > 0)
    {
        srandom(seed);
        head = CreateUDTDataRandom(nrandom);
    }
    else
    {
        head = CreateUDTData();
    }

    /* traverse and print the data for debug purposes */
    PrintUDTData(head);

    /* Create the HDF5 file */
    fid = H5Fcreate(h5file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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

    FreeUDTData();

    return 0;
}
