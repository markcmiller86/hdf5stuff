
! Test writing a complicated pointer linked data structure with
! multiple User Defined Types (UDTs) to HDF5 datasets
! via fortran

MODULE UDTsCompounds

    USE,INTRINSIC :: ISO_C_BINDING

    USE HDF5

    IMPLICIT NONE

    INTEGER,PARAMETER :: singR = kind(0.)
    INTEGER,PARAMETER :: fullR = kind(0.d0)
    INTEGER,PARAMETER :: singI = kind(0)
    INTEGER,PARAMETER :: fullI = selected_int_kind(18)
    INTEGER,PARAMETER :: pntrI = selected_int_kind(18)

    INTEGER,PARAMETER :: TYPE_LISTNODE=303+1
    INTEGER,PARAMETER :: TYPE_TREENODE=303+2
    INTEGER,PARAMETER :: TYPE_TREELISTNODE=303+3

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
! currently have these "t1" versions as a hack to deal with the fact
! that H5OFFSETOF does not work properly with members which are pointers
! to other UDTs
    TYPE ListNode_t1
        INTEGER :: offsets(3)
        REAL :: a
        REAL :: b
    END TYPE ListNode_t1

    TYPE TreeNode_t
        INTEGER :: memberA
        INTEGER :: memberB
        DOUBLE PRECISION :: coords(3)
        TYPE(TreeNode_t),POINTER :: left
        TYPE(TreeNode_t),POINTER :: right
        TYPE(ListNode_t),POINTER :: list
    END TYPE TreeNode_t
    TYPE TreeNode_t1
        INTEGER(singI) :: memberA
        INTEGER(singI) :: memberB
        DOUBLE PRECISION :: coords(3)
    END TYPE TreeNode_t1

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
! Create memory and file types for the 3 UDTs above
!==========================================================================
SUBROUTINE CreateListNodeTypes(fid,ln_p,mtype,ftype)

    INTEGER(hid_t),INTENT(IN) :: fid,ln_p
    INTEGER(hid_t),INTENT(OUT) :: mtype,ftype

    INTEGER :: hdferr
    TYPE(ListNode_t),TARGET :: dummy
    TYPE(ListNode_t1),TARGET :: dummy1
    INTEGER(hid_t) :: moff,foff
    INTEGER(hsize_t) :: dims(1)=3


WRITE(UNIT=*,FMT='(a,i0)') "ListNode_t, without pointers=",(STORAGE_SIZE(dummy1)/8)
WRITE(UNIT=*,FMT='(a,i0)') "ListNode_t, with pointers=",(STORAGE_SIZE(dummy)/8)
    ! memory type
    CALL h5tarray_create_f(H5T_NATIVE_INTEGER,1,dims,moff,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tcreate_f(H5T_COMPOUND_F,INT(STORAGE_SIZE(dummy)/8,hsize_t),mtype,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(mtype,"offsets",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%offsets)),moff,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(mtype,"a",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%a)),h5kind_to_type(singR,H5_REAL_KIND),hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(mtype,"b",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%b)),h5kind_to_type(singI,H5_REAL_KIND),hdferr); CALL stopper(hdferr,__LINE__)
!XXX: H5OFFSETOF does not work properly here!!
    CALL h5tinsert_f(mtype,"next",INT(24,SIZE_T),ln_p,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tclose_f(moff,hdferr); CALL stopper(hdferr,__LINE__)

    ! file type
    CALL h5tarray_create_f(H5T_NATIVE_INTEGER,1,dims,foff,hdferr)
    CALL h5tcommit_f(fid,"offsets",foff,hdferr)
    CALL h5tcreate_f(H5T_COMPOUND_F,INT(STORAGE_SIZE(dummy)/8,hsize_t),ftype,hdferr)
    CALL h5tinsert_f(ftype,"offsets",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%offsets)),foff,hdferr)
    CALL h5tinsert_f(ftype,"a",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%a)),H5T_NATIVE_REAL,hdferr)
    CALL h5tinsert_f(ftype,"b",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%b)),H5T_NATIVE_REAL,hdferr)
    ! fortran does not need the pointers to other UDTs in the file datatypes
    CALL h5tcommit_f(fid,"ListNode_t",ftype,hdferr)
    CALL h5tclose_f(foff,hdferr)

END SUBROUTINE CreateListNodeTypes


!==========================================================================
SUBROUTINE CreateTreeNodeTypes(fid,tn_p,ln_p,mtype,ftype)

    INTEGER(hid_T),INTENT(IN) :: fid,tn_p,ln_p
    INTEGER(hid_t),INTENT(OUT) :: mtype,ftype

    TYPE(TreeNode_t),TARGET :: dummy
    TYPE(TreeNode_t1),TARGET :: dummy1
    INTEGER(hsize_t) :: dims(1)=3
    INTEGER(HID_T) :: mcoords,fcoords
    INTEGER :: hdferr


WRITE(UNIT=*,FMT='(a,i0)') "TreeNode_t, without pointers=",(STORAGE_SIZE(dummy1)/8)
WRITE(UNIT=*,FMT='(a,i0)') "TreeNode_t, with pointers=",(STORAGE_SIZE(dummy)/8)
    ! memory type
    CALL h5tarray_create_f(H5T_NATIVE_DOUBLE,1,dims,mcoords,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tcreate_f(H5T_COMPOUND_F,INT(STORAGE_SIZE(dummy)/8,hsize_t),mtype,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(mtype,"memberA",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%memberA)),h5kind_to_type(singI,H5_INTEGER_KIND),hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(mtype,"memberB",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%memberB)),h5kind_to_type(singI,H5_INTEGER_KIND),hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(mtype,"coords",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%coords)),mcoords,hdferr); CALL stopper(hdferr,__LINE__)
!XXX: H5OFFSETOF does not work properly here!!
    CALL h5tinsert_f(mtype,"left",INT(32,SIZE_T),tn_p,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(mtype,"right",INT(40,SIZE_T),tn_p,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(mtype,"list",INT(48,SIZE_T),ln_p,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tclose_f(mcoords,hdferr); CALL stopper(hdferr,__LINE__)

    ! file type
    CALL h5tarray_create_f(H5T_NATIVE_DOUBLE,1,dims,fcoords,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tcommit_f(fid,"coords",fcoords,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tcreate_f(H5T_COMPOUND_F,INT(STORAGE_SIZE(dummy)/8,hsize_t),ftype,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(ftype,"memberA",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%memberA)),h5kind_to_type(singI,H5_INTEGER_KIND),hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(ftype,"memberB",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%memberB)),h5kind_to_type(singI,H5_INTEGER_KIND),hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(ftype,"coords",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%coords)),fcoords,hdferr); CALL stopper(hdferr,__LINE__)
    ! fortran does not need the pointers to other UDTs in the file datatypes
    CALL h5tcommit_f(fid,"TreeNode_t",ftype,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tclose_f(fcoords,hdferr); CALL stopper(hdferr,__LINE__)

END SUBROUTINE CreateTreeNodeTypes


!==========================================================================
SUBROUTINE CreateTreeListNodeTypes(fid,tln_p,mtype,ftype)

    INTEGER(hid_t),INTENT(IN) :: fid,tln_p
    INTEGER(hid_t),INTENT(OUT) :: mtype,ftype

    TYPE(TreeListNode_t),TARGET :: dummy
    TYPE(TreeListNode_t1) :: dummy1
    INTEGER(hid_t) :: mname,fname
    INTEGER :: hdferr


WRITE(UNIT=*,FMT='(a,i0)') "TreeListNode_t, without pointers=",(STORAGE_SIZE(dummy1)/8)
WRITE(UNIT=*,FMT='(a,i0)') "TreeListNode_t, with pointers=",(STORAGE_SIZE(dummy)/8)
    ! memory type
    CALL h5tcopy_f(H5T_FORTRAN_S1,mname,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tset_size_f(mname,INT(STORAGE_SIZE(dummy%name)/8,hsize_t),hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tcreate_f(H5T_COMPOUND_F,INT(STORAGE_SIZE(dummy)/8,hsize_t),mtype,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(mtype,"name",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%name)),mname,hdferr); CALL stopper(hdferr,__LINE__)
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""val1""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val1))
    CALL h5tinsert_f(mtype,"val1",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val1)),h5kind_to_type(singI,H5_INTEGER_KIND),hdferr); CALL stopper(hdferr,__LINE__)
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""val2""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val2))
    CALL h5tinsert_f(mtype,"val2",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val2)),h5kind_to_type(fullR,H5_REAL_KIND),hdferr); CALL stopper(hdferr,__LINE__)
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""val3""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val3))
    CALL h5tinsert_f(mtype,"val3",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val3)),h5kind_to_type(singR,H5_REAL_KIND),hdferr); CALL stopper(hdferr,__LINE__)
!XXX: H5OFFSETOF does not work properly here!!
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""next""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%next))
    ! CALL h5tinsert_f(mtype,"next",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%next)),H5T_OPAQUE_F,hdferr)
    ! CALL h5tinsert_f(mtype,"next",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%next)),H5T_REFERENCE_F,hdferr)
    ! CALL h5tinsert_f(mtype,"next",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%next)),tln_p,hdferr)
    CALL h5tinsert_f(mtype,"next",INT(56,SIZE_T),tln_p,hdferr); CALL stopper(hdferr,__LINE__)
    ! CALL h5tinsert_f(mtype,"next",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%next)),h5kind_to_type(pntrI,H5_INTEGER_KIND),hdferr)
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""prev""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%prev))
    CALL h5tinsert_f(mtype,"prev",INT(64,SIZE_T),tln_p,hdferr); CALL stopper(hdferr,__LINE__)
WRITE(UNIT=*,FMT='(a,i0)') "  offset of ""tree""=",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%tree))
    CALL h5tinsert_f(mtype,"tree",INT(72,SIZE_T),tln_p,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tclose_f(mname,hdferr); CALL stopper(hdferr,__LINE__)

    ! file type
    CALL h5tcopy_f(H5T_FORTRAN_S1,fname,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tset_size_f(fname,INT(STORAGE_SIZE(dummy%name)/8,hsize_t),hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tcommit_f(fid,"name",fname,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tcreate_f(H5T_COMPOUND_F,INT(STORAGE_SIZE(dummy)/8,hsize_t),ftype,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(ftype,"name",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%name)),fname,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(ftype,"val1",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val1)),h5kind_to_type(singI,H5_INTEGER_KIND),hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(ftype,"val2",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val2)),h5kind_to_type(fullR,H5_REAL_KIND),hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tinsert_f(ftype,"val3",H5OFFSETOF(C_LOC(dummy),C_LOC(dummy%val3)),h5kind_to_type(singR,H5_REAL_KIND),hdferr); CALL stopper(hdferr,__LINE__)
    ! fortran does not need the pointers to other UDTs in the file datatypes
    CALL h5tcommit_f(fid,"TreeListNode_t",ftype,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tclose_f(fname,hdferr); CALL stopper(hdferr,__LINE__)

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

        ln=>ln%next
    END DO

END SUBROUTINE


!==========================================================================
RECURSIVE SUBROUTINE TraverseTreeNodeInPreparationForWriting(node,nln,ln_map,ntn,tn_map, &
                                                             UDTPointerCount,UDTPointerTable)

    TYPE(TreeNode_t),POINTER :: node
    INTEGER,INTENT(INOUT) :: nln
    INTEGER,INTENT(INOUT) :: ln_map(:)
    INTEGER,INTENT(INOUT) :: ntn
    INTEGER,INTENT(INOUT) :: tn_map(:)
    INTEGER(singI),INTENT(INOUT) :: UDTPointerCount
    INTEGER(singI),POINTER :: UDTPointerTable(:,:)


    ntn=ntn+1

    UDTPointerCount=UDTPointerCount+1
    IF (UDTPointerCount>SIZE(UDTPointerTable,DIM=1)) CALL ExpandUDTPointerTable(UDTPointerTable)
    UDTPointerTable(UDTPointerCount,1)=TYPE_TREENODE
    UDTPointerTable(UDTPointerCount,2)=ntn
    UDTPointerTable(UDTPointerCount,3)=TYPE_LISTNODE
    IF (ASSOCIATED(node%list)) THEN
        UDTPointerTable(UDTPointerCount,3)=nln+1
        CALL TraverseListNodeInPreparationForWriting(node%list,nln,ln_map)
    ELSE
        UDTPointerTable(UDTPointerCount,4)=0
    END IF

    UDTPointerCount=UDTPointerCount+1
    IF (UDTPointerCount>SIZE(UDTPointerTable,DIM=1)) CALL ExpandUDTPointerTable(UDTPointerTable)
    UDTPointerTable(UDTPointerCount,1)=TYPE_TREENODE
    UDTPointerTable(UDTPointerCount,2)=ntn
    UDTPointerTable(UDTPointerCount,3)=TYPE_TREENODE
    IF (ASSOCIATED(node%left)) THEN
        UDTPointerTable(UDTPointerCount,4)=ntn+1
        CALL TraverseTreeNodeInPreparationForWriting(node%left,nln,ln_map,ntn,tn_map, &
                                                     UDTPointerCount,UDTPointerTable)
    ELSE
        UDTPointerTable(UDTPointerCount,4)=0
    END IF

    UDTPointerCount=UDTPointerCount+1
    IF (UDTPointerCount>SIZE(UDTPointerTable,DIM=1)) CALL ExpandUDTPointerTable(UDTPointerTable)
    UDTPointerTable(UDTPointerCount,1)=TYPE_TREENODE
    UDTPointerTable(UDTPointerCount,2)=ntn
    UDTPointerTable(UDTPointerCount,3)=TYPE_TREENODE
    IF (ASSOCIATED(node%right)) THEN
        UDTPointerTable(UDTPointerCount,4)=ntn+1
        CALL TraverseTreeNodeInPreparationForWriting(node%right,nln,ln_map,ntn,tn_map, &
                                                     UDTPointerCount,UDTPointerTable)
    ELSE
        UDTPointerTable(UDTPointerCount,4)=0
    END IF

END SUBROUTINE TraverseTreeNodeInPreparationForWriting


!==========================================================================
SUBROUTINE TraverseUDTInPreparationForWriting(node,nln,ln_map, &
                                                   ntn,tn_map, &
                                                   ntln,tln_map, &
                                              UDTPointerCount, &
                                              UDTPointerTable)

    TYPE(TreeListNode_t),POINTER :: node
    INTEGER,INTENT(INOUT) :: nln
    INTEGER,INTENT(INOUT) :: ln_map(:)
    INTEGER,INTENT(INOUT) :: ntn
    INTEGER,INTENT(INOUT) :: tn_map(:)
    INTEGER,INTENT(INOUT) :: ntln
    INTEGER,INTENT(INOUT) :: tln_map(:)
    INTEGER(singI),INTENT(INOUT) :: UDTPointerCount
    INTEGER(singI),POINTER :: UDTPointerTable(:,:)

    TYPE(TreeListNode_t),POINTER :: tln

    tln=>NULL()


    tln=>node
    DO
        IF (.NOT.ASSOCIATED(tln)) EXIT

        ntln=ntln+1

        UDTPointerCount=UDTPointerCount+1
        IF (UDTPointerCount>SIZE(UDTPointerTable,DIM=1)) CALL ExpandUDTPointerTable(UDTPointerTable)
        UDTPointerTable(UDTPointerCount,1)=TYPE_TREELISTNODE
        UDTPointerTable(UDTPointerCount,2)=ntln
        UDTPointerTable(UDTPointerCount,3)=TYPE_TREENODE
        IF (ASSOCIATED(tln%tree)) THEN
            UDTPointerTable(UDTPointerCount,4)=ntn+1
            CALL TraverseTreeNodeInPreparationForWriting(tln%tree,nln,ln_map,ntn,tn_map, &
                                                         UDTPointerCount,UDTPointerTable)
        ELSE
            UDTPointerTable(UDTPointerCount,4)=0
        END IF

        tln=>tln%next
    END DO

END SUBROUTINE TraverseUDTInPreparationForWriting


!==========================================================================
! Traversal routines to do the actual writing
!==========================================================================
SUBROUTINE TraverseListNodeAndWrite(fid,node,nln,ln_m,lnspaceid,lnsetid)

    INTEGER(hid_t),INTENT(IN) :: fid
    TYPE(ListNode_t),POINTER :: node
    INTEGER,INTENT(INOUT) :: nln
    INTEGER(hid_t),INTENT(IN) :: ln_m,lnspaceid,lnsetid

    INTEGER(hsize_t) :: hdimm(1)
    INTEGER(hid_t) :: spidm
    TYPE(ListNode_t),POINTER :: ln
    INTEGER(hsize_t) :: coord(1,1)
    TYPE(ListNode_t),TARGET :: ln_scalar
    INTEGER :: hdferr

    ln=>NULL()


    hdimm=1
    CALL h5screate_simple_f(1,hdimm,spidm,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5sselect_all_f(spidm,hdferr); CALL stopper(hdferr,__LINE__)

    ln=>node
    DO
        IF (.NOT.ASSOCIATED(ln)) EXIT

!         (*nln)++;
        nln=nln+1
!         H5Sselect_none(lnspaceid);
        CALL h5sselect_none_f(lnspaceid,hdferr); CALL stopper(hdferr,__LINE__)
!         hsize_t coord = *nln;
        coord=INT(nln,hsize_t)
!         H5Sselect_elements(lnspaceid, H5S_SELECT_SET, 1, &coord);
        CALL h5sselect_elements_f(lnspaceid,H5S_SELECT_SET_F,1,INT(1,hsize_t),coord,hdferr); CALL stopper(hdferr,__LINE__)
!         H5Dwrite(lnsetid, ln_m, spidm, lnspaceid, H5P_DEFAULT, node);
        ln_scalar=ln
        CALL h5dwrite_f(lnsetid,ln_m,C_LOC(ln_scalar),hdferr,spidm,lnspaceid,H5P_DEFAULT_F); CALL stopper(hdferr,__LINE__)

!         node = node->next;
        ln=>ln%next
    END DO
!     H5Sclose(spidm);
    CALL h5sclose_f(spidm,hdferr)

END SUBROUTINE TraverseListNodeAndWrite


!==========================================================================
RECURSIVE SUBROUTINE TraverseTreeNodeAndWrite(fid, node, &
    nln, ln_m, lnspaceid, lnsetid, &
    ntn, tn_m, tnspaceid, tnsetid)

    INTEGER(hid_t),INTENT(IN) :: fid
    TYPE(TreeNode_t),POINTER :: node
    INTEGER,INTENT(INOUT) :: nln
    INTEGER(hid_t),INTENT(IN) :: ln_m,lnspaceid,lnsetid
    INTEGER,INTENT(INOUT) :: ntn
    INTEGER(hid_t),INTENT(IN) :: tn_m,tnspaceid,tnsetid

    INTEGER(hsize_t) :: hdimm(1)
    INTEGER(hid_t) :: spidm
    INTEGER(hsize_t) :: coord(1,1)
    TYPE(TreeNode_t),TARGET :: tn_scalar
    INTEGER :: hdferr


!     hsize_t hdimm = 1;
    hdimm=1
!     hid_t spidm = H5Screate_simple(1, &hdimm, 0);
    CALL h5screate_simple_f(1,hdimm,spidm,hdferr); CALL stopper(hdferr,__LINE__)
!     H5Sselect_all(spidm);
    CALL h5sselect_all_f(spidm,hdferr); CALL stopper(hdferr,__LINE__)

!     (*ntn)++;
    ntn=ntn+1
!     H5Sselect_none(tnspaceid);
    CALL h5sselect_none_f(tnspaceid,hdferr); CALL stopper(hdferr,__LINE__)
!     hsize_t coord = *ntn;
    coord=INT(ntn,hsize_t)
!     H5Sselect_elements(tnspaceid, H5S_SELECT_SET, 1, &coord);
    CALL h5sselect_elements_f(tnspaceid,H5S_SELECT_SET_F,1,INT(1,hsize_t),coord,hdferr); CALL stopper(hdferr,__LINE__)
!     H5Dwrite(tnsetid, tn_m, spidm, tnspaceid, H5P_DEFAULT, node);
    tn_scalar=node
    CALL h5dwrite_f(tnsetid,tn_m,C_LOC(tn_scalar),hdferr,spidm,tnspaceid,H5P_DEFAULT_F); CALL stopper(hdferr,__LINE__)

    IF (ASSOCIATED(node%list)) &
        CALL TraverseListNodeAndWrite(fid,node%list,nln,ln_m,lnspaceid,lnsetid)
    IF (ASSOCIATED(node%left)) &
        CALL TraverseTreeNodeAndWrite(fid,node%left,nln,ln_m,lnspaceid,lnsetid,ntn,tn_m,tnspaceid,tnsetid)
    IF (ASSOCIATED(node%right)) &
        CALL TraverseTreeNodeAndWrite(fid,node%right,nln,ln_m,lnspaceid,lnsetid,ntn,tn_m,tnspaceid,tnsetid)
!     H5Sclose(spidm);
    CALL h5sclose_f(spidm,hdferr)

END SUBROUTINE TraverseTreeNodeAndWrite


!==========================================================================
SUBROUTINE TraverseUDTAndWrite(fid,node, &
    nln, ln_m, lnspaceid, lnsetid, &
    ntn, tn_m, tnspaceid, tnsetid, &
    ntln, tln_m, tlnspaceid, tlnsetid)

    INTEGER(hid_t),INTENT(IN) :: fid
    TYPE(TreeListNode_t),POINTER :: node
    INTEGER,INTENT(INOUT) :: nln
    INTEGER(hid_t),INTENT(IN) :: ln_m,lnspaceid,lnsetid
    INTEGER,INTENT(INOUT) :: ntn
    INTEGER(hid_t),INTENT(IN) :: tn_m,tnspaceid,tnsetid
    INTEGER,INTENT(INOUT) :: ntln
    INTEGER(hid_t),INTENT(IN) :: tln_m,tlnspaceid,tlnsetid

    INTEGER(hsize_t) :: hdimm(1)
    INTEGER(hid_t) :: spidm
    TYPE(TreeListNode_t),POINTER :: tln
    TYPE(TreeListNode_t),TARGET :: tln_scalar
    INTEGER(hsize_t) :: coord(1,1)
    INTEGER(hsize_t) :: dims(1,1),length
    INTEGER :: hdferr

    tln=>NULL()


!     hsize_t hdimm = 1;
    hdimm=1
!     hid_t spidm = H5Screate_simple(1, &hdimm, 0);
    CALL h5screate_simple_f(1,hdimm,spidm,hdferr); CALL stopper(hdferr,__LINE__)
!     H5Sselect_all(spidm);
    CALL h5sselect_all_f(spidm,hdferr); CALL stopper(hdferr,__LINE__)

!     *nln = *ntn = *ntln = 0;
    tln=>node
    DO
        IF (.NOT.ASSOCIATED(tln)) EXIT

        tln_scalar=tln

        ntln=ntln+1
!         hsize_t coord = *ntln;
        coord=INT(ntln,hsize_t)
        CALL h5sselect_none_f(tlnspaceid,hdferr); CALL stopper(hdferr,__LINE__)
        CALL h5sselect_elements_f(tlnspaceid,H5S_SELECT_SET_F,1,INT(1,hsize_t),coord,hdferr); CALL stopper(hdferr,__LINE__)
        ! CALL h5sselect_all_f(tlnspaceid,hdferr); CALL stopper(hdferr,__LINE__)
        ! dims should be ignored
        dims=-1
        length=INT(1,hsize_t)
        ! H5Dwrite(tlnid, tln_m, spidm, tlnspid, H5P_DEFAULT, node);
        CALL h5dwrite_f(tlnsetid,tln_m,C_LOC(tln_scalar),hdferr,spidm,tlnspaceid,H5P_DEFAULT_F); CALL stopper(hdferr,__LINE__)
        IF (ASSOCIATED(tln%tree)) THEN
            CALL TraverseTreeNodeAndWrite(fid,tln%tree, &
                nln, ln_m, lnspaceid, lnsetid, &
                ntn, tn_m, tnspaceid, tnsetid)
        END IF

        tln=>tln%next
    END DO
!     H5Sclose(spidm);
    CALL h5sclose_f(spidm,hdferr)

END SUBROUTINE TraverseUDTAndWrite


!==========================================================================
SUBROUTINE ExpandUDTPointerTable(table)

    INTEGER(singI),POINTER :: table(:,:)

    INTEGER(singI) :: NumRows
    INTEGER(singI),ALLOCATABLE :: temptable(:,:)


    NumRows=SIZE(table,DIM=1)
    ALLOCATE(temptable(NumRows,4))
    temptable=table
    DEALLOCATE(table)
    ALLOCATE(table(NumRows*2,4))
    table=0
    table(1:NumRows,:)=temptable
    DEALLOCATE(temptable)

END SUBROUTINE ExpandUDTPointerTable


!==========================================================================
SUBROUTINE stopper(hdferr,LineNo)

    INTEGER(singI),INTENT(IN) :: hdferr
    INTEGER(singI),INTENT(IN) :: LineNo

    IF (hdferr/=0) THEN
        WRITE(UNIT=*,FMT='(a,i0,a)') "Error at line ",LineNo,", quitting . . ."
        STOP
    END IF

END SUBROUTINE stopper


!==========================================================================
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
    INTEGER(hid_t) :: lnsetid,tnsetid,tlnsetid;
    INTEGER(hsize_t) :: hdim(1)
    INTEGER :: i,nln,ntn,ntln
! ListNode_t const *ln_map[64];
    INTEGER :: ln_map(64)
    INTEGER :: tn_map(64)
    INTEGER :: tln_map(64)
    !
    ! indexing is as follows:
    ! i1=number of "graph edges", or pointers of the form "UDT1->UDT2"
    ! i2=single graph edge, encoded as (UDT type 1,index of UDT type 1,UDT type 2,index of UDT type 2)
    !   * if a pointer is pointing to null, (index of UDT type 2=0)
    !   * singly-linked lists are skipped, as the associated graph edges should
    !     trivial to reconstruct
    !
    ! note the the initial number of graph edges is set to two; will expand
    ! dynamically as necessary
    !
    INTEGER(singI) :: UDTPointerCount,iPointer
    INTEGER(singI),POINTER :: UDTPointerTable(:,:)
    INTEGER(singI),ALLOCATABLE :: UDTPtrHDF(:,:)
    CHARACTER(LEN=64) :: type1str,type2str
    INTEGER(hid_t) :: pspaceid,psetid,psetPropID
    INTEGER(hsize_t) :: phdim(2)

    head=>NULL()
    UDTPointerCount=0
    ALLOCATE(UDTPointerTable(2,4))
    UDTPointerTable=0


    ! create some data to write
    CALL CreateUDTData(head)

    ! traverse and print the data for debug purposes
    CALL PrintUDTData(head)

    ! Create the HDF5 file
    CALL h5open_f(hdferr)
    CALL h5fcreate_f("udt_graph2_f.h5",H5F_ACC_TRUNC_F,fid,hdferr)
    CALL h5gcreate_f(fid,"Types",tgid,hdferr)

    ! Create memory POINTER types. These are never committed and are only
    ! used to facilitate the conversion process on write and read.
    ! The associated file types will be a suitable integer type.
    CALL h5tcreate_f(H5T_OPAQUE_F,INT(STORAGE_SIZE(dummy_ptr)/8,SIZE_T),ln_p,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tset_tag_f(ln_p,"ListNode_t pointer",hdferr)
    CALL h5tcreate_f(H5T_OPAQUE_F,INT(STORAGE_SIZE(dummy_ptr)/8,SIZE_T),tn_p,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tset_tag_f(tn_p,"TreeNode_t pointer",hdferr)
    CALL h5tcreate_f(H5T_OPAQUE_F,INT(STORAGE_SIZE(dummy_ptr)/8,SIZE_T),tln_p,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5tset_tag_f(tln_p,"TreeListNode_t pointer",hdferr)

    ! Create memory and file types for our 3 user defined types
    CALL CreateListNodeTypes(tgid,ln_p,ln_m,ln_f)
    CALL CreateTreeNodeTypes(tgid,tn_p,ln_p,tn_m,tn_f)
    CALL CreateTreeListNodeTypes(tgid,tln_p,tln_m,tln_f)

    nln=0
    ln_map=0
    ntn=0
    tn_map=0
    ntln=0
    tln_map=0
    CALL TraverseUDTInPreparationForWriting(head,nln,ln_map,ntn,tn_map,ntln,tln_map, &
                                            UDTPointerCount,UDTPointerTable)
! print out the pointer table for a quick check
ALLOCATE(UDTPtrHDF(UDTPointerCount,4))
DO iPointer=1,UDTPointerCount
    UDTPtrHDF(iPointer,:)=UDTPointerTable(iPointer,:)
    IF (UDTPointerTable(iPointer,1)==TYPE_LISTNODE) THEN
        WRITE(UNIT=type1str,FMT='(a)') "listnode"
    ELSE IF (UDTPointerTable(iPointer,1)==TYPE_TREENODE) THEN
        WRITE(UNIT=type1str,FMT='(a)') "treenode"
    ELSE IF (UDTPointerTable(iPointer,1)==TYPE_TREELISTNODE) THEN
        WRITE(UNIT=type1str,FMT='(a)') "treelistnode"
    END IF
    IF (UDTPointerTable(iPointer,3)==TYPE_LISTNODE) THEN
        WRITE(UNIT=type2str,FMT='(a)') "listnode"
    ELSE IF (UDTPointerTable(iPointer,3)==TYPE_TREENODE) THEN
        WRITE(UNIT=type2str,FMT='(a)') "treenode"
    ELSE IF (UDTPointerTable(iPointer,3)==TYPE_TREELISTNODE) THEN
        WRITE(UNIT=type2str,FMT='(a)') "treelistnode"
    END IF
    WRITE(UNIT=*,FMT='(a,a,a,i0,a,a,a,i0)') &
        "type """,TRIM(type1str), &
        """, index ",UDTPointerTable(iPointer,2), &
        " -> type """,TRIM(type2str), &
        """, index ",UDTPointerTable(iPointer,4)
END DO
    ! currently has a lot of zeros
    ! lets do some conditioning . . . 
    phdim(1)=INT(UDTPointerCount,hsize_t)
    phdim(2)=INT(4,hsize_t)
    CALL h5screate_simple_f(2,phdim,pspaceid,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5pcreate_f(H5P_DATASET_CREATE_F,psetPropID,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5pset_chunk_f(psetPropID,2,phdim,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5pset_shuffle_f(psetPropID,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5pset_deflate_f(psetPropID,7,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5dcreate_f(fid,"UDTPointerTable",H5T_NATIVE_INTEGER,pspaceid,psetid,hdferr,dcpl_id=psetPropID); CALL stopper(hdferr,__LINE__)
    CALL h5dwrite_f(psetid,H5T_NATIVE_INTEGER,C_LOC(UDTPtrHDF),hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5pclose_f(psetPropID,hdferr); CALL stopper(hdferr,__LINE__)
    CALL h5dclose_f(psetid,hdferr)
    CALL h5sclose_f(pspaceid,hdferr)
    DEALLOCATE(UDTPtrHDF)

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
    CALL h5screate_simple_f(1,hdim,lnspaceid,hdferr); CALL stopper(hdferr,__LINE__)
!     lnsetid = H5Dcreate(fid, "ListNode_data", ln_f, lnspaceid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CALL h5dcreate_f(fid,"ListNode_data",ln_f,lnspaceid,lnsetid,hdferr); CALL stopper(hdferr,__LINE__)

    hdim=INT(ntn,hsize_t)
!     tnspaceid = H5Screate_simple(1, &hdim, 0);
    CALL h5screate_simple_f(1,hdim,tnspaceid,hdferr); CALL stopper(hdferr,__LINE__)
!     tnsetid = H5Dcreate(fid, "TreeNode_data", tn_f, tnspaceid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CALL h5dcreate_f(fid,"TreeNode_data",tn_f,tnspaceid,tnsetid,hdferr); CALL stopper(hdferr,__LINE__)

    hdim=INT(ntln,hsize_t)
!     tlnspaceid = H5Screate_simple(1, &hdim, 0);
    CALL h5screate_simple_f(1,hdim,tlnspaceid,hdferr); CALL stopper(hdferr,__LINE__)
!     tlnsetid = H5Dcreate(fid, "TreeListNode_data", tln_f, tlnspaceid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CALL h5dcreate_f(fid,"TreeListNode_data",tln_f,tlnspaceid,tlnsetid,hdferr); CALL stopper(hdferr,__LINE__)

    ! Do the actual traversal and writing. Note, pointer->int conversions happen
    ! automatically as hdf5 encounters the need due to src and dst type mismatches.
! THIS COMMENT DOESNT REALLY APPLY NOW . . .
    nln=0
    ntn=0
    ntln=0
    CALL TraverseUDTAndWrite(fid,head, &
        nln, ln_m, lnspaceid, lnsetid, &
        ntn, tn_m, tnspaceid, tnsetid, &
        ntln, tln_m, tlnspaceid, tlnsetid)

! #ifdef DEBUG
!     PrintUDTData(head);
! #endif

    CALL h5dclose_f(tlnsetid,hdferr)
    CALL h5sclose_f(tlnspaceid,hdferr)
!     H5Dclose(tnsetid);
    CALL h5sclose_f(tnspaceid,hdferr)
    CALL h5dclose_f(lnsetid,hdferr)
    CALL h5sclose_f(lnspaceid,hdferr)
    CALL h5gclose_f(tgid,hdferr)
    CALL h5fclose_f(fid,hdferr)
    CALL h5close_f(hdferr)

END PROGRAM main


