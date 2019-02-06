!**********************************************************************************************************************!
!This module provides procedures for memory allocation and deallocation.                                               !
!**********************************************************************************************************************!
module MemoryManagement

  implicit none

  public

  !******************************************************************************************************************!
  !                                               CONTAINED PROCEDURES                                               !
  !                                                                                                                  !
  !'ResizeArray' resizes arrays of intrinsic data types (overloaded).                                                !
  !******************************************************************************************************************!

  interface ResizeArray
    module procedure ResizeArray_idpa
    module procedure ResizeArray_iia
    module procedure ResizeArray_ila
    module procedure ResizeArray_isa
    module procedure ResizeArray_iX2dpa
    module procedure ResizeArray_iX2ia
    module procedure ResizeArray_iX3dpa
    module procedure ResizeArray_iX3ia
  end interface ResizeArray

  contains

  !******************************************************************************************************************!
  !                                              INTERFACE RESIZEARRAY                                               !
  !******************************************************************************************************************!
  subroutine ResizeArray_idpa (nume, ida)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp
    implicit none

    integer,               intent(in)    :: nume
    real(dp), allocatable, intent(inout) :: ida(:)

    integer :: code

    procname = 'MemoryManagement::ResizeArray_idpa'
    if (ltrace) call PrintTrace()

    !Negative array size is not permitted. Allocation of a zero-sized array is permitted to release memory.
    if (nume<0) call PrintError (ekey=1102, lstop=.true.)

    !No action is required if the array is already allocated to correct size. Otherwise, first deallocate, and
    !then reallocate to correct size.
    if (allocated(ida)) then
      if ((nume/=0) .and. (nume==size(ida))) return
      deallocate (ida, stat=code)
        if (code>0) call PrintError (ekey=1112, lstop=.true.)
    end if

    if (nume>0) then
      allocate (ida(1:nume), stat=code)
        if (code>0) call PrintError (ekey=1111, lstop=.true.)
    end if

  end subroutine ResizeArray_idpa

  subroutine ResizeArray_iia (nume, ida)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    integer,              intent(in)    :: nume
    integer, allocatable, intent(inout) :: ida(:)

    integer :: code

    procname = 'MemoryManagement::ResizeArray_iia'
    if (ltrace) call PrintTrace()

    !Negative array size is not permitted. Allocation of a zero-sized array is permitted to release memory.
    if (nume<0) call PrintError (ekey=1102, lstop=.true.)

    !No action is required if the array is already allocated to correct size. Otherwise, first deallocate, and
    !then reallocate to correct size.
    if (allocated(ida)) then
      if ((nume/=0) .and. (nume==size(ida))) return
      deallocate (ida, stat=code)
        if (code>0) call PrintError (ekey=1112, lstop=.true.)
    end if

    if (nume>0) then
      allocate (ida(1:nume), stat=code)
        if (code>0) call PrintError (ekey=1111, lstop=.true.)
    end if

  end subroutine ResizeArray_iia

  subroutine ResizeArray_ila (nume, ida)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    integer,              intent(in)    :: nume
    logical, allocatable, intent(inout) :: ida(:)

    integer :: code

    procname = 'MemoryManagement::ResizeArray_ila'
    if (ltrace) call PrintTrace()

    !Negative array size is not permitted. Allocation of a zero-sized array is permitted to release memory.
    if (nume<0) call PrintError (ekey=1102, lstop=.true.)

    !No action is required if the array is already allocated to correct size. Otherwise, first deallocate, and
    !then reallocate to correct size.
    if (allocated(ida)) then
      if ((nume/=0) .and. (nume==size(ida))) return
      deallocate (ida, stat=code)
        if (code>0) call PrintError (ekey=1112, lstop=.true.)
    end if

    if (nume>0) then
      allocate (ida(1:nume), stat=code)
        if (code>0) call PrintError (ekey=1111, lstop=.true.)
    end if

  end subroutine ResizeArray_ila

  subroutine ResizeArray_isa (nume, ida)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    integer,                       intent(in)    :: nume
    character(len=*), allocatable, intent(inout) :: ida(:)

    integer :: code

    procname = 'MemoryManagement::ResizeArray_isa'
    if (ltrace) call PrintTrace()

    !Negative array size is not permitted. Allocation of a zero-sized array is permitted to release memory.
    if (nume<0) call PrintError (ekey=1102, lstop=.true.)

    !No action is required if the array is already allocated to correct size. Otherwise, first deallocate, and
    !then reallocate to correct size.
    if (allocated(ida)) then
      if ((nume/=0) .and. (nume==size(ida))) return
      deallocate (ida, stat=code)
        if (code>0) call PrintError (ekey=1112, lstop=.true.)
    end if

    if (nume>0) then
      allocate (ida(1:nume), stat=code)
        if (code>0) call PrintError (ekey=1111, lstop=.true.)
    end if

  end subroutine ResizeArray_isa

  subroutine ResizeArray_iX2dpa (numr, numc, ida)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp
    implicit none

    integer,               intent(in)    :: numc, numr
    real(dp), allocatable, intent(inout) :: ida(:,:)

    integer :: code

    procname = 'MemoryManagement::ResizeArray_iX2dpa'
    if (ltrace) call PrintTrace()

    !Negative array size is not permitted. Allocation of a zero-sized array is permitted to release memory.
    if ((numc<0) .or. (numr<0)) call PrintError (ekey=1102, lstop=.true.)

    !No action is required if the array is already allocated to correct size. Otherwise, first deallocate, and
    !then reallocate to correct size.
    if (allocated(ida)) then
      if ((numr/=0) .and. (numc/=0) .and. (numr==size(ida,1)) .and. (numc==size(ida,2))) return
      deallocate (ida, stat=code)
        if (code>0) call PrintError (ekey=1112, lstop=.true.)
    end if

    if ((numc>0) .and. (numr>0)) then
      allocate (ida(1:numr,1:numc), stat=code)
        if (code>0) call PrintError (ekey=1111, lstop=.true.)
    end if

  end subroutine ResizeArray_iX2dpa

  subroutine ResizeArray_iX2ia (numr, numc, ida)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    integer,              intent(in)    :: numc, numr
    integer, allocatable, intent(inout) :: ida(:,:)

    integer :: code

    procname = 'MemoryManagement::ResizeArray_iX2ia'
    if (ltrace) call PrintTrace()

    !Negative array size is not permitted. Allocation of a zero-sized array is permitted to release memory.
    if ((numc<0) .or. (numr<0)) call PrintError (ekey=1102, lstop=.true.)

    !No action is required if the array is already allocated to correct size. Otherwise, first deallocate, and
    !then reallocate to correct size.
    if (allocated(ida)) then
      if ((numr/=0) .and. (numc/=0) .and. (numr==size(ida,1)) .and. (numc==size(ida,2))) return
      deallocate (ida, stat=code)
        if (code>0) call PrintError (ekey=1112, lstop=.true.)
    end if

    if ((numc>0) .and. (numr>0)) then
      allocate (ida(1:numr,1:numc), stat=code)
        if (code>0) call PrintError (ekey=1111, lstop=.true.)
    end if

  end subroutine ResizeArray_iX2ia

  subroutine ResizeArray_iX3dpa (numi, numj, numk, ida)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp
    implicit none

    integer,               intent(in)    :: numi, numj, numk
    real(dp), allocatable, intent(inout) :: ida(:,:,:)

    integer :: code

    procname = 'MemoryManagement::ResizeArray_iX3dpa'
    if (ltrace) call PrintTrace()

    !Negative array size is not permitted. Allocation of a zero-sized array is permitted to release memory.
    if ((numi<0) .or. (numj<0) .or. (numk<0)) call PrintError (ekey=1102, lstop=.true.)

    !No action is required if the array is already allocated to correct size. Otherwise, first deallocate, and
    !then reallocate to correct size.
    if (allocated(ida)) then
      if ((numi/=0) .and. (numj/=0) .and. (numk/=0) .and. (numi==size(ida,1)) .and. (numj==size(ida,2)) .and.  &
          (numk==size(ida,3))) return
      deallocate (ida, stat=code)
        if (code>0) call PrintError (ekey=1112, lstop=.true.)
    end if

    if ((numi>0) .and. (numj>0) .and. (numk>0)) then
      allocate (ida(1:numi,1:numj,1:numk), stat=code)
        if (code>0) call PrintError (ekey=1111, lstop=.true.)
    end if

  end subroutine ResizeArray_iX3dpa

  subroutine ResizeArray_iX3ia (numi, numj, numk, ida)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    integer,              intent(in)    :: numi, numj, numk
    integer, allocatable, intent(inout) :: ida(:,:,:)

    integer :: code

    procname = 'MemoryManagement::ResizeArray_iX3ia'
    if (ltrace) call PrintTrace()

    !Negative array size is not permitted. Allocation of a zero-sized array is permitted to release memory.
    if ((numi<0) .or. (numj<0) .or. (numk<0)) call PrintError (ekey=1102, lstop=.true.)

    !No action is required if the array is already allocated to correct size. Otherwise, first deallocate, and
    !then reallocate to correct size.
    if (allocated(ida)) then
      if ((numi/=0) .and. (numj/=0) .and. (numk/=0) .and. (numi==size(ida,1)) .and. (numj==size(ida,2)) .and.  &
          (numk==size(ida,3))) return
      deallocate (ida, stat=code)
        if (code>0) call PrintError (ekey=1112, lstop=.true.)
    end if

    if ((numi>0) .and. (numj>0) .and. (numk>0)) then
      allocate (ida(1:numi,1:numj,1:numk), stat=code)
        if (code>0) call PrintError (ekey=1111, lstop=.true.)
    end if

  end subroutine ResizeArray_iX3ia
  !******************************************************************************************************************!

end module MemoryManagement
!**********************************************************************************************************************!
