
! Test writing a complicated pointer linked data structure with
! multiple User Defined Types (UDTs) to HDF5 datasets
! via fortran

MODULE UDTsCompounds

    USE,INTRINSIC :: ISO_C_BINDING

! #include <hdf5.h>
    USE HDF5
    ! USE H5FORTRAN_TYPES
! #include <assert.h>
! #include <math.h>
! #include <stdlib.h>
! #include <string.h>

    IMPLICIT NONE

    INTEGER,PARAMETER :: singR = kind(0.)
    INTEGER,PARAMETER :: fullR = kind(0.d0)
    INTEGER,PARAMETER :: singI = kind(0)
    INTEGER,PARAMETER :: fullI = selected_int_kind(18)
    INTEGER,PARAMETER :: pntrI = selected_int_kind(18)

    ! Define three simple User Defined Types (UDTs).
    ! Top level is doubly linked list of TreeListNodes.
    ! Each tree List node may point to a TreeNode, which
    ! is a binary tree. Some binary trees are sparse and
    ! others are dense. Each binary tree node can point
    ! ListNode which is a linked list.
    TYPE ListNode_t
        INTEGER :: offsets(3)
        REAL :: a
        REAL :: b
        TYPE(ListNode_t),POINTER :: next
    END TYPE ListNode_t

    TYPE TreeNode_t
        INTEGER :: memberA
        INTEGER :: memberB
        DOUBLE PRECISION :: coords(3)
        TYPE(TreeNode_t),POINTER :: left
        TYPE(TreeNode_t),POINTER :: right
        TYPE(ListNode_t),POINTER :: list
    END TYPE TreeNode_t

    TYPE TreeListNode_t
        CHARACTER(LEN=32) :: name=""
        INTEGER(singI) :: val1=1
        REAL(fullR) :: val2=REAL(2.02,fullR)
        REAL(singR) :: val3=REAL(3.03,singR)
        TYPE(TreeListNode_t),POINTER :: next=>NULL()
        TYPE(TreeListNode_t),POINTER :: prev=>NULL()
        TYPE(TreeNode_t),POINTER :: tree=>NULL()
    END TYPE TreeListNode_t
    TYPE TreeListNode_t1
        CHARACTER(LEN=32) :: name
        INTEGER(singI) :: val1
        REAL(fullR) :: val2
        REAL(singR) :: val3
    END TYPE TreeListNode_t1



! /* Stuff to help with pointer->int conversions */
! hid_t ln_p, tn_p, tln_p;
! TreeListNode_t const *tln_map[64];
! TreeNode_t const *tn_map[64];
! ListNode_t const *ln_map[64];

! /* Data conversion routine callback used by HDF5 during H5Dwrite calls
!    to convert memory pointer types to file integer types. Note that
!    the conversion "maps" are inefficient using linear lookup. */
! herr_t UDTPointerToInt(hid_t srctyp, hid_t dsttyp, H5T_cdata_t *cdata,
!     size_t nelmts, size_t buf_stride, size_t bkg_stride,
!     void *buf, void *bkg, hid_t plist)
! {
!     int* foo = (int*) buf;
!     if (nelmts <= 0) return 0;
!     assert(nelmts == 1);
!     assert(sizeof(TreeNode_t*)>=sizeof(int));
!     assert(H5Tequal(dsttyp, H5T_NATIVE_INT)>0);
!     if (H5Tequal(srctyp, tn_p)>0)
!     {
!         int i;
!         TreeNode_t *p = *((TreeNode_t**) buf);
!         printf("UDTPointerToInt for TreeNode_t*: %p\n", p);
!         if (p == 0)
!         {
!             *foo = -1;
!             return 0;
!         }
!         for (i = 0; i < 64; i++)
!         {
!             if (tn_map[i] == p)
!             {
!                 *foo = i;
!ver                 return 0;
!             }
!         }
!         printf("***ERROR***: Can't find TreeNode_t* @ %p\n", p);
!         return -1;
!     }
!     else if (H5Tequal(srctyp, ln_p)>0)
!     {
!         int i;
!         ListNode_t *p = *((ListNode_t**) buf);
!         printf("UDTPointerToInt for ListNode_t*: %p\n", p);
!         if (p == 0)
!         {
!             *foo = -1;
!             return 0;
!         }
!         for (i = 0; i < 64; i++)
!         {
!             if (ln_map[i] == p)
!             {
!                 *foo = i;
!                 return 0;
!             }
!         }
!         printf("***ERROR***: Can't find ListNode_t* @ %p\n", p);
!         return -1;
!     }
!     else if (H5Tequal(srctyp, tln_p)>0)
!     {
!         int i;
!         TreeListNode_t *p = *((TreeListNode_t**) buf);
!         printf("UDTPointerToInt for TreeListNode_t*: %p\n", p);
!         if (p == 0)
!         {
!             *foo = -1;
!             return 0;
!         }
!         for (i = 0; i < 64; i++)
!         {
!             if (tln_map[i] == p)
!             {
!                 *foo = i;
!                 return 0;
!             }
!         }
!         printf("***ERROR***: Can't find TreeListNode_t* @ %p\n", p);
!         return -1;
!     }
!     else
!         return -1;
!     return 0;
! }



CONTAINS

!==========================================================================
! Top level data creation routine which creates an arbitrary
! arrangement of UDTs
!==========================================================================
SUBROUTINE CreateUDTData(retval)

    TYPE(TreeListNode_t),POINTER :: retval

    TYPE(TreeListNode_t),POINTER :: treeListHead,treeList0,treeList1,treeList2
    TYPE(TreeNode_t),POINTER :: root,treeNodeTmp
    TYPE(ListNode_t),POINTER :: head,listNodeTmp

    treeListHead=>NULL()
    treeList0=>NULL()
    treeList1=>NULL()
    treeList2=>NULL()
    root=>NULL()
    treeNodeTmp=>NULL()
    head=>NULL()
    listNodeTmp=>NULL()


    ! top level is 3 entries of "trees"
    CALL CreateTreeListNodeInstance(treeList0)
    CALL CreateTreeListNodeInstance(treeList1)
    CALL CreateTreeListNodeInstance(treeList2)
    treeListHead=>treeList0

    treeList0%next=>treeList1
    treeList1%prev=>treeList0
    treeList1%next=>treeList2
    treeList2%prev=>treeList1

    ! Tree 0; just two children and no lists in either
    CALL CreateTreeNodeInstance(root)
    treeList0%tree=>root
    CALL CreateTreeNodeInstance(root%left)
    CALL CreateTreeNodeInstance(root%right)

    ! Tree 1; unbalanced, a few levels, some with lists
    CALL CreateTreeNodeInstance(root)
    treeList1%tree=>root
    CALL CreateTreeNodeInstance(root%left)
    CALL CreateTreeNodeInstance(root%left%right)
    CALL CreateListNodeInstance(root%left%right%list)
    CALL CreateListNodeInstance(root%left%right%list%next)
    CALL CreateListNodeInstance(root%left%right%list%next%next)
    CALL CreateListNodeInstance(root%left%right%list%next%next%next)
    CALL CreateTreeNodeInstance(root%left%left)
    CALL CreateTreeNodeInstance(root%left%left%left)
    CALL CreateTreeNodeInstance(root%left%left%left%left)
    CALL CreateListNodeInstance(root%left%left%left%left%list)
    CALL CreateListNodeInstance(root%left%left%left%left%list%next)
    CALL CreateListNodeInstance(root%left%left%left%left%list%next%next)
    CALL CreateListNodeInstance(root%left%left%left%left%list%next%next%next)
    CALL CreateListNodeInstance(root%left%left%left%left%list%next%next%next%next)
    CALL CreateTreeNodeInstance(root%left%left%left%right)
    CALL CreateTreeNodeInstance(root%right)
    CALL CreateTreeNodeInstance(root%right%right)
    CALL CreateListNodeInstance(root%right%right%list)
    CALL CreateListNodeInstance(root%right%right%list%next)

    ! Tree 2, more stuff
    CALL CreateTreeNodeInstance(root)
    treeList2%tree=>root
    CALL CreateTreeNodeInstance(root%left)
    CALL CreateListNodeInstance(root%left%list)
    CALL CreateTreeNodeInstance(root%right)
    CALL CreateListNodeInstance(root%right%list)
    CALL CreateListNodeInstance(root%right%list%next)
    CALL CreateListNodeInstance(root%right%list%next%next)
    CALL CreateTreeNodeInstance(root%left%left)
    CALL CreateListNodeInstance(root%left%left%list)
    CALL CreateTreeNodeInstance(root%left%right)
    CALL CreateListNodeInstance(root%left%right%list)
    CALL CreateTreeNodeInstance(root%right%left)
    CALL CreateListNodeInstance(root%right%left%list)
    CALL CreateTreeNodeInstance(root%right%right)
    CALL CreateListNodeInstance(root%right%right%list)

    ! Several more empty tree list nodes at the end
    CALL CreateTreeListNodeInstance(treeList2%next)
    treeList2%next%prev=>treeList2

    CALL CreateTreeListNodeInstance(treeList2%next%next)
    treeList2%next%next%prev=>treeList2%next

    CALL CreateTreeListNodeInstance(treeList2%next%next%next)
    treeList2%next%next%next%prev=>treeList2%next%next

    ! done
    retval=>treeListHead

END SUBROUTINE CreateUDTData


!==========================================================================
! Data creation routines just to create some useable data that I can easily
! understand with h5ls
!==========================================================================
SUBROUTINE CreateListNodeInstance(ln)

    TYPE(ListNode_t),POINTER :: ln

    INTEGER,SAVE :: count=0

    ALLOCATE(ln)
    ln%offsets(1)=count
    ln%offsets(2)=2*count
    ln%offsets(3)=3*count
    ln%a=SQRT(REAL(count))
    ln%b=2*ln%a
    ln%next=>NULL()
    count=count+1
    
END SUBROUTINE CreateListNodeInstance


!==========================================================================
SUBROUTINE CreateTreeNodeInstance(tn)

    TYPE(TreeNode_t),POINTER :: tn

    INTEGER,SAVE :: count=0

    ALLOCATE(tn)
    tn%memberA=count
    tn%memberB=2*tn%memberA
    tn%coords(1)=REAL(count)
    tn%coords(2)=REAL(2*count)
    tn%coords(3)=REAL(3*count)
    tn%left=>NULL()
    tn%right=>NULL()
    tn%list=>NULL()
    count=count+1

END SUBROUTINE CreateTreeNodeInstance


!==========================================================================
SUBROUTINE CreateTreeListNodeInstance(tln)

    TYPE(TreeListNode_t),POINTER :: tln

    INTEGER,SAVE :: count=0
    CHARACTER(LEN=32) :: name

    ALLOCATE(tln)
    WRITE(tln%name,FMT='(a,i0.3)') "TreeListNode_",count
    tln%val1=count
    tln%val2=count*count
    tln%val3=3.14159265359*count
    tln%next=>NULL()
    tln%prev=>NULL()
    tln%tree=>NULL()
    count=count+1

END SUBROUTINE CreateTreeListNodeInstance


!==========================================================================
! /* Print routines for debugging */
!==========================================================================
SUBROUTINE PrintListNode(indent,node)

    INTEGER,INTENT(IN) :: indent
    TYPE(ListNode_t),POINTER :: node

    CHARACTER(LEN=(indent+1)*2) :: ts
    TYPE(ListNode_t),POINTER :: ln


    WRITE(UNIT=ts,FMT='(a)') REPEAT(" ",(indent+1)*2)

    ln=>node
    DO
        IF (.NOT.ASSOCIATED(ln)) EXIT

        WRITE(UNIT=*,FMT='(a,a)') ts,"ListNode_t"
        WRITE(UNIT=*,FMT='(a,a,i0,a,i0,a,i0)') ts, &
            "  offsets = ",ln%offsets(1),",",ln%offsets(2),",",ln%offsets(3)
        WRITE(UNIT=*,FMT='(a,a,es12.5)') ts,"  a = ",ln%a
        WRITE(UNIT=*,FMT='(a,a,es12.5)') ts,"  b = ",ln%b

        ln=>ln%next
    END DO

END SUBROUTINE PrintListNode

!==========================================================================
RECURSIVE SUBROUTINE PrintTreeNode(indent,node,label)

    INTEGER,INTENT(IN) :: indent
    TYPE(TreeNode_t),POINTER :: node
    CHARACTER(LEN=*) :: label

    CHARACTER(LEN=(indent+1)*2) :: ts
! {
!     int                 memberA;
!     int                 memberB;
!     double              coords[3];
!     struct _TreeNode_t *left;
!     struct _TreeNode_t *right;
!     struct _ListNode_t *list;

    WRITE(UNIT=ts,FMT='(a)') REPEAT(" ",(indent+1)*2)

    WRITE(UNIT=*,FMT='(a,a,a,a)') ts,"TreeNode_t """,TRIM(label),""""
    WRITE(UNIT=*,FMT='(a,a,i0)') ts,"  memberA = ",node%memberA
    WRITE(UNIT=*,FMT='(a,a,i0)') ts,"  memberB = ",node%memberB
    WRITE(UNIT=*,FMT='(a,a,es22.15,a,es22.15,a,es22.15)') &
        ts,"  coords = ",node%coords(1),",",node%coords(2),",",node%coords(3)
    IF (ASSOCIATED(node%list)) CALL PrintListNode(indent,node%list)
    IF (ASSOCIATED(node%left)) CALL PrintTreeNode(indent+1,node%left,TRIM(label)//"%left")
    IF (ASSOCIATED(node%right)) CALL PrintTreeNode(indent+1,node%right,TRIM(label)//"%right")

END SUBROUTINE PrintTreeNode


!==========================================================================
SUBROUTINE PrintTreeListNode(node)

    TYPE(TreeListNode_t),POINTER :: node



    WRITE(UNIT=*,FMT='(a)') "TreeListNode_t"

    WRITE(UNIT=*,FMT='(a,a)') "  name = ",TRIM(node%name)
    WRITE(UNIT=*,FMT='(a,i0)') "  val1 = ",node%val1
    WRITE(UNIT=*,FMT='(a,es22.15)') "  val2 = ",node%val2
    WRITE(UNIT=*,FMT='(a,es12.5)') "  val3 = ",node%val3
    IF (ASSOCIATED(node%tree)) CALL PrintTreeNode(0,node%tree,TRIM(node%name)//"%root")

END SUBROUTINE PrintTreeListNode


!==========================================================================
SUBROUTINE PrintUDTData(root)

    TYPE(TreeListNode_t),POINTER :: root

    TYPE(TreeListNode_t),POINTER :: tln

    tln=>root
    DO
        IF (.NOT.ASSOCIATED(tln)) EXIT
        
        CALL PrintTreeListNode(tln)

        tln=>tln%next
    END DO

END SUBROUTINE PrintUDTData


!==========================================================================
! Create memory and file types for the 3 UDTs above */
!==========================================================================
! #define OFFSET(P,F)     ((char*)&((P).F)-(char*)&(P))
SUBROUTINE CreateListNodeTypes(fid,mtype,ftype)

    INTEGER(hid_t),INTENT(IN) :: fid
    INTEGER(hid_t),INTENT(OUT) :: mtype,ftype

    INTEGER :: hdferr
    TYPE(ListNode_t),TARGET :: dummy
    INTEGER(hid_t) :: moff,foff
    INTEGER(hsize_t) :: dims(1)=3


!     /* memory type */
!     moff = H5Tarray_create(H5T_NATIVE_INT, 1, &dims);
!     *mtype = H5Tcreate(H5T_COMPOUND, sizeof(ListNode_t));
!     H5Tinsert(*mtype, "offsets", OFFSET(dummy, offsets), moff);
!     H5Tinsert(*mtype, "a", OFFSET(dummy, a), H5T_NATIVE_FLOAT);
!     H5Tinsert(*mtype, "b", OFFSET(dummy, b), H5T_NATIVE_FLOAT);
!     H5Tinsert(*mtype, "next", OFFSET(dummy, next), ln_p);
!     H5Tclose(moff);

    ! file type
!     foff = H5Tarray_create(H5T_NATIVE_INT, 1, &dims);
    CALL h5tarray_create_f(H5T_NATIVE_INTEGER,1,dims,foff,hdferr)
!     H5Tcommit(fid, "offsets", foff, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CALL h5tcommit_f(fid,"offsets",foff,hdferr)
    ! *ftype = H5Tcreate(H5T_COMPOUND, sizeof(ListNode_t));
    CALL h5tcreate_f(H5T_COMPOUND_F,INT(STORAGE_SIZE(dummy)/8,hsize_t),ftype,hdferr)
    CALL h5tinsert_f(ftype,"offsets",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%offsets)),foff,hdferr)
!     H5Tinsert(*ftype, "a", OFFSET(dummy, a), H5T_NATIVE_FLOAT);
    CALL h5tinsert_f(ftype,"a",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%a)),H5T_NATIVE_REAL,hdferr)
!     H5Tinsert(*ftype, "b", OFFSET(dummy, b), H5T_NATIVE_FLOAT);
    CALL h5tinsert_f(ftype,"b",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%b)),H5T_NATIVE_REAL,hdferr)
!     H5Tinsert(*ftype, "next", OFFSET(dummy, next), H5T_NATIVE_INT);
    ! H5Tcommit(fid, "ListNode_t", *ftype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CALL h5tcommit_f(fid,"ListNode_t",ftype,hdferr)
!     H5Tclose(foff);
    CALL h5tclose_f(foff,hdferr)

END SUBROUTINE CreateListNodeTypes


!==========================================================================
! void CreateTreeNodeTypes(hid_t fid, hid_t *mtype, hid_t *ftype)
! {
!     TreeNode_t dummy;
!     hid_t mcoords, fcoords;
!     hsize_t dims = 3;

!     /* memory type */
!     mcoords = H5Tarray_create(H5T_NATIVE_DOUBLE, 1, &dims);
!     *mtype = H5Tcreate(H5T_COMPOUND, sizeof(TreeNode_t));
!     H5Tinsert(*mtype, "memberA", OFFSET(dummy, memberA), H5T_NATIVE_INT);
!     H5Tinsert(*mtype, "memberB", OFFSET(dummy, memberB), H5T_NATIVE_INT);
!     H5Tinsert(*mtype, "coords", OFFSET(dummy, coords), mcoords);
!     H5Tinsert(*mtype, "left", OFFSET(dummy, left), tn_p);
!     H5Tinsert(*mtype, "right", OFFSET(dummy, right), tn_p);
!     H5Tinsert(*mtype, "list", OFFSET(dummy, list), ln_p);
!     H5Tclose(mcoords);

!     /* file type */
!     fcoords = H5Tarray_create(H5T_NATIVE_DOUBLE, 1, &dims);
!     H5Tcommit(fid, "coords", fcoords, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
!     *ftype = H5Tcreate(H5T_COMPOUND, sizeof(TreeNode_t));
!     H5Tinsert(*ftype, "memberA", OFFSET(dummy, memberA), H5T_NATIVE_INT);
!     H5Tinsert(*ftype, "memberB", OFFSET(dummy, memberB), H5T_NATIVE_INT);
!     H5Tinsert(*ftype, "coords", OFFSET(dummy, coords), fcoords);
!     H5Tinsert(*ftype, "left", OFFSET(dummy, left), H5T_NATIVE_INT);
!     H5Tinsert(*ftype, "right", OFFSET(dummy, right), H5T_NATIVE_INT);
!     H5Tinsert(*ftype, "list", OFFSET(dummy, list), H5T_NATIVE_INT);
!     H5Tcommit(fid, "TreeNode_t", *ftype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
!     H5Tclose(fcoords);
! }


!==========================================================================
SUBROUTINE CreateTreeListNodeTypes(fid,tln_p,mtype,ftype)

    INTEGER(hid_t),INTENT(IN) :: fid,tln_p
    INTEGER(hid_t),INTENT(OUT) :: mtype,ftype

    INTEGER :: hdferr
    TYPE(TreeListNode_t),TARGET :: dummy
    TYPE(TreeListNode_t1) :: dummy1
    INTEGER(hid_t) :: mname,fname
!     hsize_t dims = 32;


WRITE(UNIT=*,FMT='(a,i0)') "TreeListNode_t, without pointers=",(STORAGE_SIZE(dummy1)/8)
WRITE(UNIT=*,FMT='(a,i0)') "TreeListNode_t, with pointers=",(STORAGE_SIZE(dummy)/8)
!     /* memory type */
!     mname = H5Tcopy(H5T_C_S1);
    CALL h5tcopy_f(H5T_STRING,mname,hdferr)
!     H5Tset_size(mname, sizeof(dummy.name));
    CALL h5tset_size_f(mname,INT(STORAGE_SIZE(dummy%name),hsize_t),hdferr)
!     *mtype = H5Tcreate(H5T_COMPOUND, sizeof(TreeListNode_t));
    CALL h5tcreate_f(H5T_COMPOUND_F,INT(STORAGE_SIZE(dummy)/8,hsize_t),mtype,hdferr)
!     H5Tinsert(*mtype, "name", OFFSET(dummy, name), mname);
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""name""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%name))
    CALL h5tinsert_f(mtype,"name",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%name)),mname,hdferr)
!     H5Tinsert(*mtype, "val1", OFFSET(dummy, val1), H5T_NATIVE_INT);
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""val1""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val1))
    CALL h5tinsert_f(mtype,"val1",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val1)),h5kind_to_type(singI,H5_INTEGER_KIND),hdferr)
!     H5Tinsert(*mtype, "val2", OFFSET(dummy, val2), H5T_NATIVE_DOUBLE);
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""val2""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val2))
    CALL h5tinsert_f(mtype,"val2",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val2)),h5kind_to_type(fullR,H5_REAL_KIND),hdferr)
!     H5Tinsert(*mtype, "val3", OFFSET(dummy, val3), H5T_NATIVE_FLOAT);
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""val3""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val3))
    CALL h5tinsert_f(mtype,"val3",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val3)),h5kind_to_type(singR,H5_REAL_KIND),hdferr)
!     H5Tinsert(*mtype, "next", OFFSET(dummy, next), tln_p);
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""next""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%next))
    ! CALL h5tinsert_f(mtype,"next",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%next)),H5T_OPAQUE_F,hdferr)
    ! CALL h5tinsert_f(mtype,"next",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%next)),H5T_REFERENCE_F,hdferr)
    ! CALL h5tinsert_f(mtype,"next",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%next)),tln_p,hdferr)
    CALL h5tinsert_f(mtype,"next",INT(56,SIZE_T),tln_p,hdferr)
    ! CALL h5tinsert_f(mtype,"next",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%next)),h5kind_to_type(pntrI,H5_INTEGER_KIND),hdferr)
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""prev""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%prev))
    CALL h5tinsert_f(mtype,"prev",INT(64,SIZE_T),tln_p,hdferr)
!     H5Tinsert(*mtype, "prev", OFFSET(dummy, prev), tln_p);
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""tree""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%tree))
!     H5Tinsert(*mtype, "tree", OFFSET(dummy, tree), tn_p);
    CALL h5tinsert_f(mtype,"tree",INT(72,SIZE_T),tln_p,hdferr)
    CALL h5tclose_f(mname,hdferr)

!     fname = H5Tcopy(H5T_C_S1);
    CALL h5tcopy_f(H5T_STRING,fname,hdferr)
    CALL h5tset_size_f(fname,INT(STORAGE_SIZE(dummy%name),hsize_t),hdferr)
    CALL h5tcommit_f(fid,"name",fname,hdferr)
!     *ftype = H5Tcreate(H5T_COMPOUND, sizeof(TreeListNode_t));
    CALL h5tcreate_f(H5T_COMPOUND_F,INT(STORAGE_SIZE(dummy)/8,hsize_t),ftype,hdferr)
!     H5Tinsert(*ftype, "name", OFFSET(dummy, name), fname);
    CALL h5tinsert_f(ftype,"name",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%name)),fname,hdferr)
!     H5Tinsert(*ftype, "val1", OFFSET(dummy, val1), H5T_NATIVE_INT);
    ! CALL h5tinsert_f(ftype,"val1",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val1)),H5T_NATIVE_INTEGER,hdferr)
    CALL h5tinsert_f(ftype,"val1",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val1)),h5kind_to_type(singI,H5_INTEGER_KIND),hdferr)
!     H5Tinsert(*ftype, "val2", OFFSET(dummy, val2), H5T_NATIVE_DOUBLE);
    ! CALL h5tinsert_f(ftype,"val2",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val2)),H5T_NATIVE_DOUBLE,hdferr)
    CALL h5tinsert_f(ftype,"val2",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val2)),h5kind_to_type(fullR,H5_REAL_KIND),hdferr)
!     H5Tinsert(*ftype, "val3", OFFSET(dummy, val3), H5T_NATIVE_FLOAT);
    ! CALL h5tinsert_f(ftype,"val3",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val3)),H5T_NATIVE_REAL,hdferr)
    CALL h5tinsert_f(ftype,"val3",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val3)),h5kind_to_type(singR,H5_REAL_KIND),hdferr)
!     H5Tinsert(*ftype, "next", OFFSET(dummy, next), H5T_NATIVE_INT);
!     H5Tinsert(*ftype, "prev", OFFSET(dummy, prev), H5T_NATIVE_INT);
!     H5Tinsert(*ftype, "tree", OFFSET(dummy, tree), H5T_NATIVE_INT);
    CALL h5tcommit_f(fid,"TreeListNode_t",ftype,hdferr)
    CALL h5tclose_f(fname,hdferr)

END SUBROUTINE CreateTreeListNodeTypes


!==========================================================================
! Traversal prep to count occurrences of a given type and copy their
! pointers to a map
!==========================================================================
SUBROUTINE TraverseListNodeInPreparationForWriting(node,nln,ln_map)

    TYPE(ListNode_t),POINTER :: node
    INTEGER,INTENT(INOUT) :: nln
    INTEGER,INTENT(INOUT) :: ln_map(:)

    TYPE(ListNode_t),POINTER :: ln

    ln=>NULL()


    ln=>node
    DO
        IF (.NOT.ASSOCIATED(ln)) EXIT

        nln=nln+1
!         ln_map[*nln] = node;

        ln=>ln%next
    END DO

END SUBROUTINE


!==========================================================================
RECURSIVE SUBROUTINE TraverseTreeNodeInPreparationForWriting(node,nln,ln_map,ntn,tn_map)

    TYPE(TreeNode_t),POINTER :: node
    INTEGER,INTENT(INOUT) :: nln
    INTEGER,INTENT(INOUT) :: ln_map(:)
    INTEGER,INTENT(INOUT) :: ntn
    INTEGER,INTENT(INOUT) :: tn_map(:)


    ntn=ntn+1
!     tn_map[*ntn] = node; (*ntn)++;
    IF (ASSOCIATED(node%list)) &
        CALL TraverseListNodeInPreparationForWriting(node%list,nln,ln_map)
    IF (ASSOCIATED(node%left)) &
        CALL TraverseTreeNodeInPreparationForWriting(node%left,nln,ln_map,ntn,tn_map)
    IF (ASSOCIATED(node%right)) &
        CALL TraverseTreeNodeInPreparationForWriting(node%right,nln,ln_map,ntn,tn_map)

END SUBROUTINE TraverseTreeNodeInPreparationForWriting


!==========================================================================
SUBROUTINE TraverseUDTInPreparationForWriting(node,nln,ln_map, &
                                                   ntn,tn_map, &
                                                   ntln,tln_map)

    TYPE(TreeListNode_t),POINTER :: node
!     int *nln, ListNode_t const *ln_map[],
    INTEGER,INTENT(INOUT) :: nln
    INTEGER,INTENT(INOUT) :: ln_map(:)
!     int *ntn, TreeNode_t const *tn_map[],
    INTEGER,INTENT(INOUT) :: ntn
    INTEGER,INTENT(INOUT) :: tn_map(:)
!     int *ntln, TreeListNode_t const *tln_map[])
    INTEGER,INTENT(INOUT) :: ntln
    INTEGER,INTENT(INOUT) :: tln_map(:)

    TYPE(TreeListNode_t),POINTER :: tln

    tln=>NULL()


    tln=>node
    DO
        IF (.NOT.ASSOCIATED(tln)) EXIT

        ntln=ntln+1
!         tln_map[*ntln] = node;
        IF (ASSOCIATED(tln%tree)) &
            CALL TraverseTreeNodeInPreparationForWriting(tln%tree,nln,ln_map,ntn,tn_map)

        tln=>tln%next
    END DO

END SUBROUTINE TraverseUDTInPreparationForWriting


! //=========================================================================
! /* Traversal routines to do the actual writing */
! //=========================================================================
! void TraverseListNodeAndWrite(hid_t fid, ListNode_t const *node,
!     int *nln, hid_t ln_m, hid_t lnspaceid, hid_t lnsetid)
! {
!     hsize_t hdimm = 1;
!     hid_t spidm = H5Screate_simple(1, &hdimm, 0);
!     H5Sselect_all(spidm);
!     while (node)
!     {
!         hsize_t coord = *nln;
!         (*nln)++;
!         H5Sselect_none(lnspaceid);
!         H5Sselect_elements(lnspaceid, H5S_SELECT_SET, 1, &coord);
!         H5Dwrite(lnsetid, ln_m, spidm, lnspaceid, H5P_DEFAULT, node);
!         node = node->next;
!     }
!     H5Sclose(spidm);
! }


! //=========================================================================
! void TraverseTreeNodeAndWrite(hid_t fid, TreeNode_t const *node,
!     int *nln, hid_t ln_m, hid_t lnspaceid, hid_t lnsetid,
!     int *ntn, hid_t tn_m, hid_t tnspaceid, hid_t tnid)
! {
!     hsize_t hdimm = 1;
!     hid_t spidm = H5Screate_simple(1, &hdimm, 0);
!     hsize_t coord = *ntn;

!     H5Sselect_all(spidm);
!     (*ntn)++;
!     H5Sselect_none(tnspaceid);
!     H5Sselect_elements(tnspaceid, H5S_SELECT_SET, 1, &coord);
!     H5Dwrite(tnid, tn_m, spidm, tnspaceid, H5P_DEFAULT, node);
!     if (node->list)
!         TraverseListNodeAndWrite(fid, node->list, nln, ln_m, lnspaceid, lnsetid);
!     if (node->left)
!         TraverseTreeNodeAndWrite(fid, node->left, nln, ln_m, lnspaceid, lnsetid, ntn, tn_m, tnspaceid, tnid);
!     if (node->right)
!         TraverseTreeNodeAndWrite(fid, node->right, nln, ln_m, lnspaceid, lnsetid, ntn, tn_m, tnspaceid, tnid);
!     H5Sclose(spidm);
! }


!==========================================================================
SUBROUTINE TraverseUDTAndWrite(fid,node, &
    nln, ln_m, lnspaceid, lnsetid, &
    ntn, tn_m, tnspaceid, tnid, &
    ntln, tln_m, tlnspaceid, tlnsetid)

    INTEGER(hid_t),INTENT(IN) :: fid
    TYPE(TreeListNode_t),POINTER :: node
    INTEGER,INTENT(INOUT) :: nln
    INTEGER(hid_t),INTENT(IN) :: ln_m,lnspaceid,lnsetid
    INTEGER,INTENT(INOUT) :: ntn
    INTEGER(hid_t),INTENT(IN) :: tn_m,tnspaceid,tnid
    INTEGER,INTENT(INOUT) :: ntln
    INTEGER(hid_t),INTENT(IN) :: tln_m,tlnspaceid,tlnsetid

    INTEGER(hsize_t) :: hdimm(1)
    INTEGER(hid_t) :: spidm
    TYPE(TreeListNode_t),POINTER :: tln
    ! TYPE(TreeListNode_t),POINTER :: tln_null_ptr
    TYPE(TreeListNode_t),TARGET :: tln_scalar
    TYPE(TreeListNode_t),TARGET,DIMENSION(1:6) :: tln_array
    INTEGER(hsize_t) :: coord(1,1)
    INTEGER(hsize_t) :: dims(1,1),length
    INTEGER :: hdferr

    tln=>NULL()
    ! tln_null_ptr=>NULL()
    ! ALLOCATE(tln_null_ptr)


!     hsize_t hdimm = 1;
    hdimm=1
!     hid_t spidm = H5Screate_simple(1, &hdimm, 0);
    CALL h5screate_simple_f(1,hdimm,spidm,hdferr)
!     H5Sselect_all(spidm);
    CALL h5sselect_all_f(spidm,hdferr)

!     *nln = *ntn = *ntln = 0;
    tln=>node
    DO
        IF (.NOT.ASSOCIATED(tln)) EXIT

        ! tln_array=tln
WRITE(UNIT=*,FMT='(a,a,a)') "tln%name=""",TRIM(tln%name),""""
WRITE(UNIT=*,FMT='(a,i0)') "tln%val1=",tln%val1
WRITE(UNIT=*,FMT='(a,es22.15)') "tln%val2=",tln%val2
WRITE(UNIT=*,FMT='(a,es12.5)') "tln%val3=",tln%val3
WRITE(UNIT=*,FMT='(a,L)') "ASSOCIATED(tln%next)=",ASSOCIATED(tln%next)
WRITE(UNIT=*,FMT='(a,es12.5)') "tln%next%val3=",tln%next%val3
WRITE(UNIT=*,FMT='(a,a,a)') "tln_array(1)%name=""",TRIM(tln_array(1)%name),""""
WRITE(UNIT=*,FMT='(a,i0)') "tln_array(1)%val1=",tln_array(1)%val1
WRITE(UNIT=*,FMT='(a,es22.15)') "tln_array(1)%val2=",tln_array(1)%val2
WRITE(UNIT=*,FMT='(a,es12.5)') "tln_array(1)%val3=",tln_array(1)%val3
WRITE(UNIT=*,FMT='(a,L)') "ASSOCIATED(tln_array(1)%next)=",ASSOCIATED(tln_array(1)%next)
IF (ASSOCIATED(tln_array(1)%next)) THEN
WRITE(UNIT=*,FMT='(a,es12.5)') "tln_array(1)%next%val3=",tln_array(1)%next%val3
END IF
        ! tln_scalar=tln

        ntln=ntln+1
WRITE(UNIT=*,FMT='(a,i0)') "ntln=",ntln 
!         hsize_t coord = *ntln;
        coord=INT(ntln,hsize_t)
        CALL h5sselect_none_f(tlnspaceid,hdferr)
        CALL h5sselect_elements_f(tlnspaceid,H5S_SELECT_SET_F,1,INT(1,hsize_t),coord,hdferr)
        ! dims should be ignored
        dims=-1
        length=INT(1,hsize_t)
        ! H5Dwrite(tlnid, tln_m, spidm, tlnspid, H5P_DEFAULT, node);
        ! CALL h5dwrite_f(tlnsetid,tln_m,C_LOC(tln),hdferr,spidm,tlnspaceid,H5P_DEFAULT_F)
        ! CALL h5dwrite_f(tlnsetid,tln_m,tln,hdferr,spidm,tlnspaceid,H5P_DEFAULT_F)
        ! CALL blah(tlnsetid,tln_m,tln,spidm,tlnspaceid)
        ! CALL blah(tlnsetid,tln_m,tln_array,spidm,tlnspaceid)
        CALL h5dwrite_f(tlnsetid,tln_m,C_LOC(tln_array(1)),hdferr,spidm,tlnspaceid,H5P_DEFAULT_F)
        ! CALL h5dwrite_f(tlnsetid,tln_m,C_LOC(tln_null_ptr),hdferr,spidm,tlnspaceid,H5P_DEFAULT_F)
        ! CALL h5dwrite_f(tlnsetid,tln_m,C_LOC(tln_array(1)),hdferr,spidm,H5S_ALL_F,H5P_DEFAULT_F)
        ! CALL h5dwrite_f(tlnsetid,tln_m,C_LOC(tln),dims,hdferr,spidm,tlnspaceid,H5P_DEFAULT_F)
        ! CALL h5dwrite_f(tlnsetid,tln_m,tln,dims,hdferr,spidm,tlnspaceid,H5P_DEFAULT_F)
        ! CALL h5dwrite_f(tlnsetid,tln_m,C_LOC(tln_scalar),hdferr,spidm,tlnspaceid,H5P_DEFAULT_F)
!         if (node->tree) {
!             TraverseTreeNodeAndWrite(fid, node->tree,
!                 nln, ln_m, lnspaceid, lnsetid,
!                 ntn, tn_m, tnspaceid, tnid);
!         }

        tln=>tln%next
    END DO

END SUBROUTINE TraverseUDTAndWrite


SUBROUTINE blah(tlnsetid,tln_m,tln,spidm,tlnspaceid)
    INTEGER(hid_t),INTENT(IN) :: tlnsetid
    INTEGER(hid_t),INTENT(IN) :: tln_m
    TYPE(TreeListNode_t),TARGET :: tln(:)
    ! TYPE(TreeListNode_t),TARGET,DIMENSION(1:1) :: tln
    INTEGER(hid_t),INTENT(IN) :: spidm
    INTEGER(hid_t),INTENT(IN) :: tlnspaceid

    INTEGER :: hdferr
    TYPE(C_PTR) :: f_ptr

    ! CALL h5dwrite_f(tlnsetid,tln_m,C_LOC(tln),hdferr,spidm,tlnspaceid,H5P_DEFAULT_F)
    f_ptr=C_LOC(tln)
    CALL h5dwrite_f(tlnsetid,tln_m,f_ptr,hdferr,spidm,tlnspaceid,H5P_DEFAULT_F)
END SUBROUTINE blah

END MODULE UDTsCompounds
!==========================================================================



!==========================================================================
PROGRAM main

    USE UDTsCompounds

    IMPLICIT NONE

    TYPE(TreeListNode_t),POINTER :: head
    INTEGER :: hdferr
    INTEGER(hid_t) :: fid,tgid
    INTEGER(pntrI) :: dummy_ptr
    INTEGER(hid_t) :: ln_p,tn_p,tln_p
    INTEGER(hid_t) :: ln_m,ln_f,tn_m,tn_f,tln_m,tln_f
    INTEGER(hid_t) :: lnspaceid,tnspaceid,tlnspaceid
    INTEGER(hid_t) :: lnsetid,tndsid,tlnsetid;
    INTEGER(hsize_t) :: hdim(1)
    INTEGER :: i,nln,ntn,ntln
! ListNode_t const *ln_map[64];
    INTEGER :: ln_map(64)
    INTEGER :: tn_map(64)
    INTEGER :: tln_map(64)

    head=>NULL()


    ! create some data to write
    CALL CreateUDTData(head)

    ! traverse and print the data for debug purposes
    CALL PrintUDTData(head)

    ! Create the HDF5 file
    CALL h5open_f(hdferr)
    CALL h5fcreate_f("test_hdf5_udt_compounds_f.h5",H5F_ACC_TRUNC_F,fid,hdferr)
    CALL h5gcreate_f(fid,"Types",tgid,hdferr)

    ! Create memory POINTER types. These are never committed and are only
    ! used to facilitate the conversion process on write and read.
    ! The associated file types will be a suitable integer type.
    ! CALL h5tcreate_f(H5T_OPAQUE_F,INT(STORAGE_SIZE(head%tree%list)/8,SIZE_T),ln_p,hdferr)
    ! CALL h5tset_tag_f(ln_p,"ListNode_t pointer",hdferr)
!     tn_p = H5Tcreate(H5T_OPAQUE, sizeof(TreeNode_t*));
!     H5Tset_tag(tn_p, "TreeNode_t*");
    CALL h5tcreate_f(H5T_OPAQUE_F,INT(STORAGE_SIZE(dummy_ptr)/8,SIZE_T),tln_p,hdferr)
    CALL h5tset_tag_f(tln_p,"TreeListNode_t pointer",hdferr)

    ! Create memory and file types for our 3 user defined types
    CALL CreateListNodeTypes(tgid,ln_m,ln_f)
!     CreateTreeNodeTypes(tgid, &tn_m, &tn_f);
    CALL CreateTreeListNodeTypes(tgid,tln_p,tln_m,tln_f)

    nln=0
    ln_map=0
    ntn=0
    tn_map=0
    ntln=0
    tln_map=0
    CALL TraverseUDTInPreparationForWriting(head,nln,ln_map,ntn,tn_map,ntln,tln_map)

! #ifdef DEBUG
!     printf("nln = %d, ntn = %d, ntln = %d\n", nln, ntn, ntln);
    ! WRITE(UNIT=*,FMT='(a,i0,a,i0,a,i0)') "nln = ",nln,", ntn = ",ntn,", ntln = ",ntln
!     printf("\nList Node Map:");
!     for (i = 0; i < nln; i++)
!         printf("%s%p", i%5?", ":"\n    ", ln_map[i]);
!     printf("\nTree Node Map:");
!     for (i = 0; i < ntn; i++)
!         printf("%s%p", i%5?", ":"\n    ", tn_map[i]);
!     printf("\nTree List Node Map:");
!     for (i = 0; i < ntln; i++)
!         printf("%s%p", i%5?", ":"\n    ", tln_map[i]);
!     printf("\n");
! #endif

    ! Create Datasets; one for each UDT
    hdim=INT(nln,hsize_t)
!     lnspaceid = H5Screate_simple(1, &hdim, 0);
    CALL h5screate_simple_f(1,hdim,lnspaceid,hdferr)
!     lnsetid = H5Dcreate(fid, "ListNode_data", ln_f, lnspaceid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CALL h5dcreate_f(fid,"ListNode_data",ln_f,lnspaceid,lnsetid,hdferr)

    hdim=INT(ntn,hsize_t)
!     tnspaceid = H5Screate_simple(1, &hdim, 0);
    CALL h5screate_simple_f(1,hdim,tnspaceid,hdferr)
!     tndsid = H5Dcreate(fid, "TreeNode_data", tn_f, tnspaceid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    hdim=INT(ntln,hsize_t)
!     tlnspaceid = H5Screate_simple(1, &hdim, 0);
    CALL h5screate_simple_f(1,hdim,tlnspaceid,hdferr)
!     tlnsetid = H5Dcreate(fid, "TreeListNode_data", tln_f, tlnspaceid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CALL h5dcreate_f(fid,"TreeListNode_data",tln_f,tlnspaceid,tlnsetid,hdferr)

    ! Do the actual traversal and writing. Note, pointer->int conversions happen
    ! automatically as hdf5 encounters the need due to src and dst type mismatches.
! THIS COMMENT DOESNT REALLY APPLY NOW . . .
    nln=0
    ntn=0
    ntln=0
    CALL TraverseUDTAndWrite(fid,head, &
        nln, ln_m, lnspaceid, lnsetid, &
        ntn, tn_m, tnspaceid, tndsid, &
        ntln, tln_m, tlnspaceid, tlnsetid)

! #ifdef DEBUG
!     PrintUDTData(head);
! #endif

    CALL h5dclose_f(tlnsetid,hdferr)
    CALL h5sclose_f(tlnspaceid,hdferr)
!     H5Dclose(tndsid);
    CALL h5sclose_f(tnspaceid,hdferr)
    CALL h5dclose_f(lnsetid,hdferr)
    CALL h5sclose_f(lnspaceid,hdferr)
    CALL h5gclose_f(tgid,hdferr)
    CALL h5fclose_f(fid,hdferr)
    CALL h5close_f(hdferr)

END PROGRAM main


