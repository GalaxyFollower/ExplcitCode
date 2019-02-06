!**********************************************************************************************************************!
!This module provides datatypes and procedures for calculations relevant to implicit/explicit solvation schemes.       !
!**********************************************************************************************************************!
module SolvationScheme

  use Parameters, only: dp, sl
  implicit none

  public

  !******************************************************************************************************************!
  !                                       SMSSGEOMETRY (SG) OBJECT DEFINITION                                        !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'ksiz' is the status of allocatable arrays.                                                                       !
  !'numa' is the total number of SMSS atoms.                                                                         !
  !'type' is an allocatable array containing atomic symbols.                                                         !
  !'idxc, 'idxm', and 'idxs' are allocatable arrays containing indices for matching atomic coordinates.              !
  !'fix' is an allocatable array containing permissions to move atoms.                                               !
  !'pc' is an allocatable array containing atomic charges for all atoms.                                             !
  !'cx', 'cy', and 'cz' are allocatable arrays containing Cartesian coordinates for all atoms.                       !
  !'fx', 'fy', and 'fz' are allocatable arrays containing force components for all atoms.                            !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'Resize' resizes all allocatable arrays.                                                                          !
  !'Erase' release all memory from allocatable arrays.                                                               !
  !'Initialize' resizes the strucutre to a specified size and sets all members to default values.                    !
  !'Duplicate' creates a complete copy of the object.                                                                !
  !'Translate' translates all atoms by a specified distance.                                                         !
  !'CountFixedAtoms' returns the number of fixed atoms in the geometry/structure.                                    !
  !'CountFreeAtoms' returns the number of free atoms in the geometry/structure.                                      !
  !'FreezeAtoms' removes gradients from all fixed atoms.                                                             !
  !'NormalizeCharges' adjust all charges such that they add up to zero.                                              !
  !'ImportCoordinates' fills coordinate arrays using program-specified data arrays.                                  !
  !'ImportIndices' fills index arrays using SMSS setup.                                                              !
  !******************************************************************************************************************!
  type SmssGeometry
      logical,                       public :: ksiz = .false.
      integer,                       public :: numa
      character(len=6), allocatable, public :: type(:)
      integer,          allocatable, public :: idxc(:), idxm(:), idxs(:)
      logical,          allocatable, public :: fix(:)
      real(dp),         allocatable, public :: pc(:)
      real(dp),         allocatable, public :: cx(:), cy(:), cz(:)
      real(dp),         allocatable, public :: fx(:), fy(:), fz(:)
    contains
      procedure, public :: Resize            => Resize_sg
      procedure, public :: Erase             => Erase_sg
      procedure, public :: Initialize        => Initialize_sg
      procedure, public :: Duplicate         => Duplicate_sgX2
      procedure, public :: Translate         => Translate_dpX3sg
      procedure, public :: CountFixedAtoms   => CountFixedAtoms_sg_i
      procedure, public :: CountFreeAtoms    => CountFreeAtoms_sg_i
      procedure, public :: FreezeAtoms       => FreezeAtoms_sg
      procedure, public :: NormalizeCharges  => NormalizeCharges_sg
      procedure, public :: ImportCoordinates => ImportCoordinates_tcdsg
      procedure, public :: ImportIndices     => ImportIndices_sgss
  end type SmssGeometry

  !******************************************************************************************************************!
  !                                         SMSSSETUP (SS) OBJECT DEFINITION                                         !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'ksiz' is the status of allocatable arrays.                                                                       !
  !'mode' differentiates different modes of SMSS (implicit/explicit).                                                !
  !'idir_' are directories containing input files for respective programs (base/cosmo/dlpoly/peecm/refimg/turbo/     !
  !  vasp).                                                                                                          !
  !'wdir_' are directories for running jobs of respective programs (base/cosmo/dlpoly/peecm/refimg/turbo/vasp).      !
  !'driver' is the program used as optimization driver (cosmo/peecm/turbomole).                                      !
  !'refresh_mos' specifies the source of MOs for replacement after each step (none/idir_all/idir_cosmo/idir_peecm/   !
  !  idir_turbo/wdir_cosmo/wdir_peecm/wdir_turbo).                                                                   !
  !'refresh_control' specifies whether the control files should be replaced after each step.                         !
  !'replace_energy' specifies whether the last energy should be replaced.                                            !
  !'rij' returns true if RI approximation is used.                                                                   !
  !'save_logs' specifies whether log files should be saved during optimization.                                      !
  !'numa' is the total number of atoms.                                                                              !
  !'idxc, 'idxm', and 'idxs' are allocatable arrays containing indices for matching atomic coordinates.              !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'IsCorrectlySized' returns true if all allocatable arrays are correctly sized.                                    !
  !'Resize' resizes all allocatable arrays.                                                                          !
  !'Erase' release all memory from allocatable arrays.                                                               !
  !'Initialize' resizes the strucutre to a specified size and sets all members to default values.                    !
  !'Duplicate' creates a complete copy of the object.                                                                !
  !'ReadFromFile' reads data from a text file.                                                                       !
  !******************************************************************************************************************!
  type SmssSetup
      logical,              public :: ksiz = .false.
      character(len=sl),    public :: mode
      character(len=sl),    public :: idir_base, idir_cosmo, idir_dlpoly, idir_peecm
      character(len=sl),    public :: idir_refimg, idir_turbo, idir_vasp
      character(len=sl),    public :: wdir_base, wdir_cosmo, wdir_dlpoly, wdir_peecm
      character(len=sl),    public :: wdir_refimg, wdir_turbo, wdir_vasp
      character(len=sl),    public :: driver
      character(len=sl),    public :: refresh_mos
      logical,              public :: refresh_control
      logical,              public :: replace_energy
      logical,              public :: rij
      logical,              public :: save_logs
      integer,              public :: numa
      integer, allocatable, public :: idxc(:), idxm(:), idxs(:)
    contains
      procedure, public :: Resize       => Resize_ss
      procedure, public :: Erase        => Erase_ss
      procedure, public :: Initialize   => Initialize_ss
      procedure, public :: Duplicate    => Duplicate_ssX2
      procedure, public :: ReadFromFile => ReadFromFile_gtfss
  end type SmssSetup

  !******************************************************************************************************************!
  !                                               CONTAINED PROCEDURES                                               !
  !                                                                                                                  !
  !'PrepareProgramJob' copies/updates all input files for a program in respective job directory.                     !
  !'ParseSmssSetup' extracts input options from SMSS_INPUT file.                                                     !
  !'FindSmssIndices' matches atoms/indices between different SMSS geometries (overloaded).                           !
  !'ValidateSmssIndices' ensures that all SMSS atoms are correctly matched (overloaded).                             !
  !'SynchronizeSmssCoordinates' synchronizes atomic coordinates between all geometries/jobs (overloaded).            !
  !'CalculateSmssEnergy' calculates total energy using SMSS (overloaded).                                            !
  !'CalculateSmssGradients' calculates total gradients for free atoms using SMSS (overloaded).                       !
  !******************************************************************************************************************!

  interface FindSmssIndices
    module procedure FindSmssIndices_iX2iadcdtcd
    module procedure FindSmssIndices_iX2iatcdvpd
  end interface FindSmssIndices

  interface ValidateSmssIndices
    module procedure ValidateSmssIndices_dcdtcdsg
    module procedure ValidateSmssIndices_tcdvpdsg
  end interface ValidateSmssIndices

  interface SynchronizeSmssCoordinates
    module procedure SynchronizeSmssCoordinates_dcdsg
    module procedure SynchronizeSmssCoordinates_vpdsg
  end interface SynchronizeSmssCoordinates

  interface CalculateSmssEnergy
    module procedure CalculateSmssEnergy_dpX2dpaX2
    module procedure CalculateSmssEnergy_dpX3
    module procedure CalculateSmssEnergy_dpX4dpa
  end interface CalculateSmssEnergy

  interface CalculateSmssGradients
    module procedure CalculateSmssGradients_tcdX2vpdsg
    module procedure CalculateSmssGradients_tcdX5vpdsg
  end interface CalculateSmssGradients

  interface PrepareProgramJob
    module procedure PrepareProgramJob_sX3
  end interface PrepareProgramJob

  contains

  !******************************************************************************************************************!
  !                                             SMSSGEOMETRY PROCEDURES                                              !
  !******************************************************************************************************************!
  subroutine Resize_sg (self)
    use Debugger,         only: PrintError, procname, ltrace, PrintTrace
    use MemoryManagement, only: ResizeArray
    implicit none

    class(SmssGeometry), intent(inout) :: self

    procname = 'SolvationScheme::Resize_sg'
    if (ltrace) call PrintTrace()

    self%ksiz = (self%numa==size(self%type)) .and. (self%numa==size(self%idxc)) .and.  &
                (self%numa==size(self%idxm)) .and. (self%numa==size(self%idxs)) .and.  &
                (self%numa==size(self%fix))  .and. (self%numa==size(self%pc))   .and.  &
                (self%numa==size(self%cx))   .and. (self%numa==size(self%cy))   .and.  &
                (self%numa==size(self%cz))   .and. (self%numa==size(self%fx))   .and.  &
                (self%numa==size(self%fy))   .and. (self%numa==size(self%fz))

    if (.not. self%ksiz) then
      call ResizeArray (nume=self%numa, ida=self%type)
      call ResizeArray (nume=self%numa, ida=self%idxc)
      call ResizeArray (nume=self%numa, ida=self%idxm)
      call ResizeArray (nume=self%numa, ida=self%idxs)
      call ResizeArray (nume=self%numa, ida=self%fix)
      call ResizeArray (nume=self%numa, ida=self%pc)
      call ResizeArray (nume=self%numa, ida=self%cx)
      call ResizeArray (nume=self%numa, ida=self%cy)
      call ResizeArray (nume=self%numa, ida=self%cz)
      call ResizeArray (nume=self%numa, ida=self%fx)
      call ResizeArray (nume=self%numa, ida=self%fy)
      call ResizeArray (nume=self%numa, ida=self%fz)
      self%ksiz = .true.
    end if

  end subroutine Resize_sg

  subroutine Erase_sg (self)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(SmssGeometry), intent(inout) :: self

    procname = 'SolvationScheme::Erase_sg'
    if (ltrace) call PrintTrace()

    self%numa = 0
    call self%Resize()
    self%ksiz = .false.

  end subroutine Erase_sg

  subroutine Initialize_sg (self)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: zero
    implicit none

    class(SmssGeometry), intent(inout) :: self

    procname = 'SolvationScheme::Initialize_sg'
    if (ltrace) call PrintTrace()

    !Resize object to desired size. Then initialize all members to default values.
    if (self%numa<1) call PrintError (ekey=1103, lstop=.true.)
    call self%Resize()
    self%type = ' '         !Unknown type.
    self%idxc = -1          !Out-of-range matching indices.
    self%idxm = -1
    self%idxs = -1
    self%fix  = .false.     !Free to move.
    self%pc   = zero        !Charge-neutral.
    self%cx   = zero        !Located at origin.
    self%cy   = zero
    self%cz   = zero
    self%fx   = zero        !In equilibrium.
    self%fy   = zero
    self%fz   = zero

  end subroutine Initialize_sg

  subroutine Duplicate_sgX2 (self, dupl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(SmssGeometry), intent(in)    :: self
    type(SmssGeometry),  intent(inout) :: dupl

    procname = 'SolvationScheme::Duplicate_sgX2'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    dupl%numa = self%numa
    call dupl%Resize()
    dupl%type = self%type
    dupl%idxc = self%idxc
    dupl%idxm = self%idxm
    dupl%idxs = self%idxs
    dupl%fix  = self%fix
    dupl%pc   = self%pc
    dupl%cx   = self%cx
    dupl%cy   = self%cy
    dupl%cz   = self%cz
    dupl%fx   = self%fx
    dupl%fy   = self%fy
    dupl%fz   = self%fz

  end subroutine Duplicate_sgX2

  subroutine Translate_dpX3sg (self, dx, dy, dz)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp
    implicit none

    class(SmssGeometry), intent(inout) :: self
    real(dp), optional,  intent(in)    :: dx, dy, dz     !Shift in each coordinate.

    procname = 'SolvationScheme::Translate_dpX3sg'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    if (present(dx)) self%cx = self%cx + dx
    if (present(dy)) self%cy = self%cy + dy
    if (present(dz)) self%cz = self%cz + dz

  end subroutine Translate_dpX3sg

  function CountFixedAtoms_sg_i (self) result (numf)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(SmssGeometry), intent(in) :: self
    integer                         :: numf

    procname = 'SolvationScheme::CountFixedAtoms_sg_i'
    if (ltrace) call PrintTrace()

    numf = 0
    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    numf = count(self%fix)

  end function CountFixedAtoms_sg_i

  function CountFreeAtoms_sg_i (self) result (numf)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(SmssGeometry), intent(in) :: self
    integer                         :: numf

    procname = 'SolvationScheme::CountFreeAtoms_sg_i'
    if (ltrace) call PrintTrace()

    numf = 0
    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    numf = self%numa - count(self%fix)

  end function CountFreeAtoms_sg_i

  subroutine FreezeAtoms_sg (self)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: zero
    implicit none

    class(SmssGeometry), intent(inout) :: self

    integer :: ii

    procname = 'SolvationScheme::FreezeAtoms_sg'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    do ii = 1, self%numa
      if (self%fix(ii)) then
        self%fx(ii) = zero
        self%fy(ii) = zero
        self%fz(ii) = zero
      end if
    end do

  end subroutine FreezeAtoms_sg

  subroutine NormalizeCharges_sg (self)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp, dtol, zero
    implicit none

    class(SmssGeometry), intent(inout) :: self

    integer  :: ii
    real(dp) :: sumqo, sumqn, normf

    procname = 'SolvationScheme::NormalizeCharges_sg'
    if (ltrace) call PrintTrace()

    !Ensure that QM cluster is (almost) charge-neutral and normalize charges.
    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    sumqo = sum(self%pc)
    if (abs(sumqo)>0.0001_dp) call PrintError (ekey=3021, lstop=.true.)
    if (abs(sumqo-zero)>dtol) then
      sumqn = sum(self%pc*self%pc)
      normf = sumqo / sumqn
      do ii = 1, self%numa
        self%pc(ii) = self%pc(ii) - self%pc(ii)*self%pc(ii)*normf
      end do
    end if

  end subroutine NormalizeCharges_sg

  subroutine ImportCoordinates_tcdsg (self, gmc)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use CoordinateFileFormats, only: CoordData
    implicit none

    class(SmssGeometry), intent(inout) :: self
    type(CoordData),     intent(in)    :: gmc      !Source TURBOMOLE coord data.

    procname = 'SolvationScheme::ImportCoordinates_tcdsg'
    if (ltrace) call PrintTrace()

    if ((.not. gmc%ksiz)  .or. (gmc%numa<1) .or.  &
        (.not. self%ksiz) .or. (self%numa/=gmc%numa)) call PrintError (ekey=1103, lstop=.true.)
    self%type = gmc%type
    self%fix  = gmc%fix
    self%cx   = gmc%cx
    self%cy   = gmc%cy
    self%cz   = gmc%cz

  end subroutine ImportCoordinates_tcdsg

  subroutine ImportIndices_sgss (self, setup)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(SmssGeometry), intent(inout) :: self
    type(SmssSetup),     intent(in)    :: setup     !Source SMSS_INPUT file (parsed).

    integer :: ii, jj

    procname = 'SolvationScheme::ImportIndices_sgss'
    if (ltrace) call PrintTrace()

    !Initialize index for QM cluster. Import indices for QM surface and MM system.
    if ((.not. setup%ksiz) .or. (setup%numa<1) .or.  &
        (.not. self%ksiz)  .or. (self%numa<setup%numa)) call PrintError (ekey=1103, lstop=.true.)
    if ((any(setup%idxc<1)) .or. (any(setup%idxc>self%numa))) call PrintError (ekey=3011, lstop=.true.)
    do ii = 1, self%numa
      self%idxc(ii) = ii
    end do
    self%idxs = -1
    self%idxm = -1
    do ii = 1, setup%numa
      jj = setup%idxc(ii)
      self%idxm(jj) = setup%idxm(ii)
      self%idxs(jj) = setup%idxs(ii)
    end do

  end subroutine ImportIndices_sgss
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                               SMSSSETUP PROCEDURES                                               !
  !******************************************************************************************************************!
  subroutine Resize_ss (self)
    use Debugger,         only: PrintError, procname, ltrace, PrintTrace
    use MemoryManagement, only: ResizeArray
    implicit none

    class(SmssSetup), intent(inout) :: self

    procname = 'SolvationScheme::Resize_ss'
    if (ltrace) call PrintTrace()

    self%ksiz = (self%numa==size(self%idxc)) .and. (self%numa==size(self%idxm)) .and.  &
                (self%numa==size(self%idxs))

    if (.not. self%ksiz) then
      call ResizeArray (nume=self%numa, ida=self%idxc)
      call ResizeArray (nume=self%numa, ida=self%idxm)
      call ResizeArray (nume=self%numa, ida=self%idxs)
      self%ksiz = .true.
    end if

  end subroutine Resize_ss

  subroutine Erase_ss (self)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(SmssSetup), intent(inout) :: self

    procname = 'SolvationScheme::Erase_ss'
    if (ltrace) call PrintTrace()

    self%numa = 0
    call self%Resize()
    self%ksiz = .false.

  end subroutine Erase_ss

  subroutine Initialize_ss (self)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: zero
    implicit none

    class(SmssSetup), intent(inout) :: self

    procname = 'SolvationScheme::Initialize_ss'
    if (ltrace) call PrintTrace()

    !Resize the object to desired size. Then initialize all members to default values.
    if (self%numa<1) call PrintError (ekey=1103, lstop=.true.)
    call self%Resize()
    self%mode            = ' '     !Must be read from $SMSS_MODE environment variable.
    self%idir_base       = ' '     !Must be read from SMSS_INPUT file.
    self%idir_cosmo      = ' '
    self%idir_dlpoly     = ' '
    self%idir_peecm      = ' '
    self%idir_refimg     = ' '
    self%idir_turbo      = ' '
    self%idir_vasp       = ' '
    self%wdir_base       = ' '     !Must be read from $PWD environment variable.
    self%wdir_cosmo      = ' '
    self%wdir_dlpoly     = ' '
    self%wdir_peecm      = ' '
    self%wdir_refimg     = ' '
    self%wdir_turbo      = ' '
    self%wdir_vasp       = ' '
    self%driver          = 'TURBOMOLE'     !Works with both implicit/explicit SMSS.
    self%refresh_mos     = 'NONE'
    self%refresh_control = .false.
    self%replace_energy  = .true.      !Required for optimization. Disable for frequency calculations.
    self%rij             = .false.     !Must be read from $SMSS_IDIR_TURBO/control file.
    self%save_logs       = .true.
    self%idxc            = -1          !Out-of-range indices.
    self%idxm            = -1
    self%idxs            = -1

  end subroutine Initialize_ss

  subroutine Duplicate_ssX2 (self, dupl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(SmssSetup), intent(in)    :: self
    type(SmssSetup),  intent(inout) :: dupl

    procname = 'SolvationScheme::Duplicate_ssX2'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    dupl%numa = self%numa
    call dupl%Resize()
    dupl%mode            = self%mode
    dupl%idir_base       = self%idir_base
    dupl%idir_cosmo      = self%idir_cosmo
    dupl%idir_dlpoly     = self%idir_dlpoly
    dupl%idir_peecm      = self%idir_peecm
    dupl%idir_refimg     = self%idir_refimg
    dupl%idir_turbo      = self%idir_turbo
    dupl%idir_vasp       = self%idir_vasp
    dupl%wdir_base       = self%wdir_base
    dupl%wdir_cosmo      = self%wdir_cosmo
    dupl%wdir_dlpoly     = self%wdir_dlpoly
    dupl%wdir_peecm      = self%wdir_peecm
    dupl%wdir_refimg     = self%wdir_refimg
    dupl%wdir_turbo      = self%wdir_turbo
    dupl%wdir_vasp       = self%wdir_vasp
    dupl%driver          = self%driver
    dupl%refresh_mos     = self%refresh_mos
    dupl%refresh_control = self%refresh_control
    dupl%replace_energy  = self%replace_energy
    dupl%rij             = self%rij
    dupl%save_logs       = self%save_logs
    dupl%idxc            = self%idxc
    dupl%idxm            = self%idxm
    dupl%idxs            = self%idxs

  end subroutine Duplicate_ssX2

  subroutine ReadFromFile_gtfss (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: sl
    use UtilityProcedures,  only: AppendCharacter, ConvertToUpperCase, DirectoryExists
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(SmssSetup),      intent(inout) :: self
    type(GenericTextFile), intent(in)    :: file

    character(len=sl) :: buff, tbuff
    integer           :: ii, jj, code

    procname = 'SolvationScheme::ReadFromFile_gtfss'
    if (ltrace) call PrintTrace()

    !Locate $SMSS_MATCH section for matching indices. This section must be present and contain at least 1 atom.
    if ((.not. file%ksiz) .or. (file%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    jj = 0
    do ii = 1, file%numl
      buff = adjustl(file%line(ii))
      call ConvertToUpperCase (line=buff)
      if (buff(1:11)=='$SMSS_MATCH') jj = ii
    end do
    if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: $SMSS_MATCH')

    !Count the number of SMSS atoms in this section. Counting continues till next $KEYWORD or end-of-file. Then
    !resize/initialize the object.
    self%numa = 0
    do ii = jj+1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:1)=='$') exit
      self%numa = self%numa + 1
    end do
    if (self%numa==0) call PrintError (ekey=3001, lstop=.true.)
    call self%Initialize()

    !Determine the mode of SMSS.
    call get_environment_variable ('SMSS_MODE', buff)
    if (buff=='1') then
       self%mode = 'IMPLICIT'
    else if (buff=='2') then
       self%mode = 'EXPLICIT'
    else
       call PrintError (ekey=1303, lstop=.true., msg1='Keyword: $SMSS_MODE')
    end if

    !Read index arrays from $SMSS_MATCH section. There are 2 (ISMSS) or 3 (ESMSS) indices per line.
    if (self%mode=='IMPLICIT') then
      do ii = 1, self%numa
        read (file%line(jj+ii),*,iostat=code) self%idxc(ii), self%idxs(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: $SMSS_MATCH')
      end do
    else if (self%mode=='EXPLICIT') then
      do ii = 1, self%numa
        read (file%line(jj+ii),*,iostat=code) self%idxc(ii), self%idxs(ii), self%idxm(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: $SMSS_MATCH')
      end do
    end if

    !Get the parent directory for running jobs. Default is the current directory and should not be overridden.
    call get_environment_variable ('PWD', buff)
    self%wdir_base = adjustl(buff)

    !Now extract all other settings from the input file.
    do ii = 1, file%numl
      buff  = file%line(ii)
      tbuff = adjustl(file%line(ii))
      call ConvertToUpperCase (line=tbuff)

      !Parent directory for input files. Use unchanged string for path. This keyword must be present.
      if (tbuff(1:10)=='$IDIR_BASE') then
        jj = 0
        jj = index (buff, '=', .true.)
          if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
        buff(1:jj) = ' '
        self%idir_base = adjustl(buff)

      !Driver program.
      else if (tbuff(1:7)=='$DRIVER') then
        jj = 0
        jj = index (tbuff, '=', .true.)
          if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
        tbuff(1:jj) = ' '
        self%driver = adjustl(tbuff)

      !Source for replacing MOs after each step.
      else if (tbuff(1:12)=='$REFRESH_MOS') then
        jj = 0
        jj = index (tbuff, '=', .true.)
          if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
        tbuff(1:jj) = ' '
        self%refresh_mos = adjustl(tbuff)

      !Replace control file after each step?
      else if (tbuff(1:16)=='$REFRESH_CONTROL') then
        jj = 0
        jj = index (tbuff, '=', .true.)
          if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
        tbuff(1:jj) = ' '
        tbuff = adjustl(tbuff)
        if ((tbuff(1:6)=='.TRUE.') .or. (tbuff(1:4)=='TRUE') .or. (tbuff(1:1)=='1')) then
          self%refresh_control = .true.
        else if ((tbuff(1:7)=='.FALSE.') .or. (tbuff(1:5)=='FALSE') .or. (tbuff(1:1)=='0')) then
          self%refresh_control = .false.
        else
          call PrintError (ekey=1303, lstop=.true., msg1='Keyword: $SMSS_REFRESH_CONTROL')
        end if

      !Replace last energy? Required for optimization. Disable for frequency calculations.
      else if (tbuff(1:15)=='$REPLACE_ENERGY') then
        jj = 0
        jj = index (tbuff, '=', .true.)
          if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
        tbuff(1:jj) = ' '
        tbuff = adjustl(tbuff)
        if ((tbuff(1:6)=='.TRUE.') .or. (tbuff(1:4)=='TRUE') .or. (tbuff(1:1)=='1')) then
          self%replace_energy = .true.
        else if ((tbuff(1:7)=='.FALSE.') .or. (tbuff(1:5)=='FALSE') .or. (tbuff(1:1)=='0')) then
          self%replace_energy = .false.
        else
          call PrintError (ekey=1303, lstop=.true., msg1='Keyword: $SMSS_REPLACE_ENERGY')
        end if

      !Save log files?
      else if (tbuff(1:10)=='$SAVE_LOGS') then
        jj = 0
        jj = index (tbuff, '=', .true.)
          if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
        tbuff(1:jj) = ' '
        tbuff = adjustl(tbuff)
        if ((tbuff(1:6)=='.TRUE.') .or. (tbuff(1:4)=='TRUE') .or. (tbuff(1:1)=='1')) then
          self%save_logs = .true.
        else if ((tbuff(1:7)=='.FALSE.') .or. (tbuff(1:5)=='FALSE') .or. (tbuff(1:1)=='0')) then
          self%save_logs = .false.
        else
          call PrintError (ekey=1303, lstop=.true., msg1='Keyword: $SMSS_SAVE_LOGS')
        end if
      end if
    end do

    !At this point, extraction of SMSS settings is complete. Now validate them.
    call AppendCharacter (line=self%idir_base, sym='/', lskip=.true.)
    call AppendCharacter (line=self%wdir_base, sym='/', lskip=.true.)
    if (self%idir_base==self%wdir_base)  &
      call PrintError (ekey=3101, lstop=.true., msg1='Directory: '//self%idir_base)

    !Validate settings for ISMSS.
    if (self%mode=='IMPLICIT') then
      !List input and work/job directories for all relevant programs.
      if (len(self%idir_base)<len_trim(self%idir_base)+12) call PrintError (ekey=1122, lstop=.true.)
      self%idir_cosmo = trim(self%idir_base) // 'CosmoSdir/'
      self%idir_turbo = trim(self%idir_base) // 'TurboSdir/'
      self%idir_vasp  = trim(self%idir_base) // 'VaspSdir/'
      if (len(self%wdir_base)<len_trim(self%wdir_base)+12) call PrintError (ekey=1122, lstop=.true.)
      self%wdir_cosmo = trim(self%wdir_base) // 'CosmoJdir/'
      self%wdir_turbo = trim(self%wdir_base) // 'TurboJdir/'
      self%wdir_vasp  = trim(self%wdir_base) // 'VaspJdir/'

      !Driver program is run in parent directory.
      if (self%driver=='COSMO') then
        self%wdir_cosmo = self%wdir_base
      else if (self%driver=='TURBOMOLE') then
        self%wdir_turbo = self%wdir_base
      else
        call PrintError (ekey=1303, lstop=.true., msg1='Keyword: $SMSS_DRIVER')
      end if

      !All source directories must already exist.
      if (.not. DirectoryExists(dir=self%idir_cosmo))  &
        call PrintError (ekey=2001, lstop=.true., msg1='Directory: '//self%idir_cosmo)
      if (.not. DirectoryExists(dir=self%idir_turbo))  &
        call PrintError (ekey=2001, lstop=.true., msg1='Directory: '//self%idir_turbo)
      if (.not. DirectoryExists(dir=self%idir_vasp))   &
        call PrintError (ekey=2001, lstop=.true., msg1='Directory: '//self%idir_vasp)

      !Replacing MOs after each step?
      if ((self%refresh_mos/='NONE')       .and. (self%refresh_mos/='IDIR_ALL')   .and.  &
          (self%refresh_mos/='IDIR_SWAP')  .and. (self%refresh_mos/='WDIR_SWAP')  .and.  &
          (self%refresh_mos/='IDIR_COSMO') .and. (self%refresh_mos/='WDIR_COSMO') .and.  &
          (self%refresh_mos/='IDIR_TURBO') .and. (self%refresh_mos/='WDIR_TURBO'))       &
        call PrintError (ekey=1303, lstop=.true., msg1='Keyword: $SMSS_REFRESH_MOS')

    !Validate settings for ESMSS.
    else if (self%mode=='EXPLICIT') then
      !List input and work/job directories for all relevant programs.
      if (len(self%idir_base)<len_trim(self%idir_base)+12) call PrintError (ekey=1122, lstop=.true.)
        self%idir_dlpoly = trim(self%idir_base) // 'DlpolySdir/'
        self%idir_peecm  = trim(self%idir_base) // 'PeecmSdir/'
        self%idir_refimg = trim(self%idir_base) // 'RefimgSdir/'
        self%idir_turbo  = trim(self%idir_base) // 'TurboSdir/'
        self%idir_vasp   = trim(self%idir_base) // 'VaspSdir/'
      if (len(self%wdir_base)<len_trim(self%wdir_base)+12) call PrintError (ekey=1122, lstop=.true.)
        self%wdir_dlpoly = trim(self%wdir_base) // 'DlpolyJdir/'
        self%wdir_peecm  = trim(self%wdir_base) // 'PeecmJdir/'
        self%wdir_refimg = trim(self%wdir_base) // 'RefimgJdir/'
        self%wdir_turbo  = trim(self%wdir_base) // 'TurboJdir/'
        self%wdir_vasp   = trim(self%wdir_base) // 'VaspJdir/'

      !Driver program is run in parent directory.
      if (self%driver=='PEECM') then
        self%wdir_peecm = self%wdir_base
      else if (self%driver=='TURBOMOLE') then
        self%wdir_turbo = self%wdir_base
      else
        call PrintError (ekey=1303, lstop=.true., msg1='Keyword: $SMSS_DRIVER')
      end if

      !All source directories must already exist.
      if (.not. DirectoryExists(dir=self%idir_dlpoly))  &
        call PrintError (ekey=2001, lstop=.true., msg1='Directory: '//self%idir_dlpoly)
      if (.not. DirectoryExists(dir=self%idir_peecm))   &
        call PrintError (ekey=2001, lstop=.true., msg1='Directory: '//self%idir_peecm)
      if (.not. DirectoryExists(dir=self%idir_refimg))  &
        call PrintError (ekey=2001, lstop=.true., msg1='Directory: '//self%idir_refimg)
      if (.not. DirectoryExists(dir=self%idir_turbo))   &
        call PrintError (ekey=2001, lstop=.true., msg1='Directory: '//self%idir_turbo)
      if (.not. DirectoryExists(dir=self%idir_vasp))    &
        call PrintError (ekey=2001, lstop=.true., msg1='Directory: '//self%idir_vasp)

      !Replacing MOs after each step?
      !Replacing MOs after each step?
      if ((self%refresh_mos/='NONE')       .and. (self%refresh_mos/='IDIR_ALL')   .and.  &
          (self%refresh_mos/='IDIR_SWAP')  .and. (self%refresh_mos/='WDIR_SWAP')  .and.  &
          (self%refresh_mos/='IDIR_PEECM') .and. (self%refresh_mos/='WDIR_PEECM') .and.  &
          (self%refresh_mos/='IDIR_TURBO') .and. (self%refresh_mos/='WDIR_TURBO'))       &
        call PrintError (ekey=1303, lstop=.true., msg1='Keyword: $SMSS_REFRESH_MOS')
    end if

  end subroutine ReadFromFile_gtfss
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                            INTERFACE FINDSMSSINDICES                                             !
  !******************************************************************************************************************!
  subroutine FindSmssIndices_iX2iadcdtcd (gmc, gmm, pivc, pivm, idxm)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: dp, mtol, A2B
    use CoordinateFileFormats, only: ConfigData, CoordData
    implicit none

    type(CoordData),  intent(in)    :: gmc            !QM cluster geometry (Bohr).
    type(ConfigData), intent(in)    :: gmm            !MM system geometry (Angstrom).
    integer,          intent(in)    :: pivc, pivm     !Indices of pivot atoms in respective geometries.
    integer,          intent(inout) :: idxm(:)        !Matching indices.

    integer          :: ii, jj
    real(dp)         :: dx, dy, dz
    type(CoordData)  :: tgmc
    type(ConfigData) :: tgmm

    procname = 'SolvationScheme::FindSmssIndices_iX2iadcdtcd'
    if (ltrace) call PrintTrace()

    !Validate all objects and create temporary copies.
    if ((.not. gmc%ksiz) .or. (gmc%numa<1) .or.  &
        (.not. gmm%ksiz) .or. (gmm%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    if (size(idxm)/=gmc%numa) call PrintError (ekey=1102, lstop=.true.)
    if ((pivc<1) .or. (pivc>gmc%numa) .or.  &
        (pivm<1) .or. (pivm>gmm%numa)) call PrintError (ekey=1101, lstop=.true.)
    call gmc%Duplicate (dupl=tgmc)
    call gmm%Duplicate (dupl=tgmm)

    !Convert all coordinates to atomic units. Then translate all atoms in QM cluster and QM surface to bring
    !respective pivots at origin.
    call tgmc%Translate (dx=-tgmc%cx(pivc), dy=-tgmc%cy(pivc), dz=-tgmc%cz(pivc))
    tgmm%cx = tgmm%cx * A2B
    tgmm%cy = tgmm%cy * A2B
    tgmm%cz = tgmm%cz * A2B
    call tgmm%Translate (dx=-tgmm%cx(pivm), dy=-tgmm%cy(pivm), dz=-tgmm%cz(pivm))

    !All atoms in QM cluster must be matched in MM system.
    idxm = -1
    do ii = 1, tgmc%numa
      do jj = 1, tgmm%numa
        dx = tgmc%cx(ii) - tgmm%cx(jj)
        dy = tgmc%cy(ii) - tgmm%cy(jj)
        dz = tgmc%cz(ii) - tgmm%cz(jj)
        if ((abs(dx)<mtol) .and. (abs(dy)<mtol) .and. (abs(dz)<mtol)) then
          idxm(ii) = jj
          exit
        end if
      end do
      if (idxm(ii)==-1) call PrintError (ekey=3013, lstop=.true.)
    end do

    !Clean up memory.
    call tgmc%Erase()
    call tgmm%Erase()

  end subroutine FindSmssIndices_iX2iadcdtcd

  subroutine FindSmssIndices_iX2iatcdvpd (gmc, gms, pivc, pivs, idxs)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: dp, mtol, A2B
    use CoordinateFileFormats, only: CoordData, PoscarData
    implicit none

    type(CoordData),  intent(in)    :: gmc            !QM cluster geometry (Bohr).
    type(PoscarData), intent(in)    :: gms            !QM surface geometry (Angstrom).
    integer,          intent(in)    :: pivc, pivs     !Indices of pivot atoms in respective geometries.
    integer,          intent(inout) :: idxs(:)        !Matching indices.

    integer          :: ii, jj
    real(dp)         :: dx, dy, dz
    type(CoordData)  :: tgmc
    type(PoscarData) :: tgms

    procname = 'SolvationScheme::FindSmssIndices_iX2iatcdvpd'
    if (ltrace) call PrintTrace()

    !Validate all objects and create temporary copies.
    if ((.not. gmc%ksiz) .or. (gmc%numa<1) .or.  &
        (.not. gms%ksiz) .or. (gms%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    if (size(idxs)/=gmc%numa) call PrintError (ekey=1102, lstop=.true.)
    if ((pivc<1) .or. (pivc>gmc%numa) .or.  &
        (pivs<1) .or. (pivs>gms%numa)) call PrintError (ekey=1101, lstop=.true.)
    call gmc%Duplicate (dupl=tgmc)
    call gms%Duplicate (dupl=tgms)

    !Convert all coordinates to atomic units. Then translate all atoms in QM cluster and QM surface to bring
    !respective pivots at origin.
    call tgmc%Translate (dx=-tgmc%cx(pivc), dy=-tgmc%cy(pivc), dz=-tgmc%cz(pivc))
    tgms%cx = tgms%cx * A2B
    tgms%cy = tgms%cy * A2B
    tgms%cz = tgms%cz * A2B
    call tgms%Translate (dx=-tgms%cx(pivs), dy=-tgms%cy(pivs), dz=-tgms%cz(pivs))

    !All free atoms in QM cluster must be matched in QM surface.
    idxs = -1
    do ii = 1, tgmc%numa
      if (.not. tgmc%fix(ii)) then
        do jj = 1, tgms%numa
          dx = tgmc%cx(ii) - tgms%cx(jj)
          dy = tgmc%cy(ii) - tgms%cy(jj)
          dz = tgmc%cz(ii) - tgms%cz(jj)
          if ((abs(dx)<mtol) .and. (abs(dy)<mtol) .and. (abs(dz)<mtol)) then
            idxs(ii) = jj
            exit
          end if
        end do
        if (idxs(ii)==-1) call PrintError (ekey=3012, lstop=.true.)
      end if
    end do

    !Clean up memory.
    call tgmc%Erase()
    call tgms%Erase()

  end subroutine FindSmssIndices_iX2iatcdvpd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                          INTERFACE VALIDATESMSSINDICES                                           !
  !******************************************************************************************************************!
  subroutine ValidateSmssIndices_dcdtcdsg (gmc, gmm, gmi)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: dp, mtol, A2B
    use CoordinateFileFormats, only: ConfigData, CoordData
    implicit none

    type(CoordData),    intent(in) :: gmc     !QM cluster geometry (Bohr).
    type(ConfigData),   intent(in) :: gmm     !MM system geometry (Angstrom).
    type(SmssGeometry), intent(in) :: gmi     !SMSS geometry (matching indices).

    integer          :: ii, jj
    real(dp)         :: dx, dy, dz
    type(CoordData)  :: tgmc
    type(ConfigData) :: tgmm

    procname = 'SolvationScheme::ValidateSmssIndices_dcdtcdsg'
    if (ltrace) call PrintTrace()

    !Validate all objects and create temporary copies.
    if ((.not. gmi%ksiz) .or. (gmi%numa<1)         .or.  &
        (.not. gmc%ksiz) .or. (gmc%numa/=gmi%numa) .or.  &
        (.not. gmm%ksiz) .or. (gmm%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    call gmc%Duplicate (dupl=tgmc)
    call gmm%Duplicate (dupl=tgmm)

    !Identify an atom in QM cluster that has a match in MM system.
    jj = 0
    do ii = 1, gmi%numa
      if (gmi%idxm(ii)/=-1) then
        jj = gmi%idxm(ii)
        exit
      end if
    end do
    if ((jj<1) .or. (jj>tgmm%numa)) call PrintError (ekey=3011, lstop=.true.)

    !Convert all coordinates to atomic units. Then translate all atoms in QM cluster and MM system to bring
    !respective pivots at origin.
    call tgmc%Translate (dx=-tgmc%cx(ii), dy=-tgmc%cy(ii), dz=-tgmc%cz(ii))
    tgmm%cx = tgmm%cx * A2B
    tgmm%cy = tgmm%cy * A2B
    tgmm%cz = tgmm%cz * A2B
    call tgmm%Translate (dx=-tgmm%cx(jj), dy=-tgmm%cy(jj), dz=-tgmm%cz(jj))

    !All atoms in QM cluster must be matched in MM system.
    do ii = 1, tgmc%numa
      jj = gmi%idxm(ii)
        if ((jj<1) .or. (jj>tgmm%numa)) call PrintError (ekey=3011, lstop=.true.)
      dx = tgmc%cx(ii) - tgmm%cx(jj)
      dy = tgmc%cy(ii) - tgmm%cy(jj)
      dz = tgmc%cz(ii) - tgmm%cz(jj)
      if ((abs(dx)>mtol) .or. (abs(dy)>mtol) .or. (abs(dz)>mtol)) call PrintError (ekey=3013, lstop=.true.)
    end do

    !Clean up memory.
    call tgmc%Erase()
    call tgmm%Erase()

  end subroutine ValidateSmssIndices_dcdtcdsg

  subroutine ValidateSmssIndices_tcdvpdsg (gmc, gms, gmi)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: dp, mtol, A2B
    use CoordinateFileFormats, only: CoordData, PoscarData
    implicit none

    type(CoordData),    intent(in) :: gmc     !QM cluster geometry (Bohr).
    type(PoscarData),   intent(in) :: gms     !QM surface geometry (Angstrom).
    type(SmssGeometry), intent(in) :: gmi     !SMSS geometry (matching indices).

    integer          :: ii, jj
    real(dp)         :: dx, dy, dz
    type(CoordData)  :: tgmc
    type(PoscarData) :: tgms

    procname = 'SolvationScheme::ValidateSmssIndices_tcdvpdsg'
    if (ltrace) call PrintTrace()

    !Validate all objects and create temporary copies.
    if ((.not. gmi%ksiz) .or. (gmi%numa<1)         .or.  &
        (.not. gmc%ksiz) .or. (gmc%numa/=gmi%numa) .or.  &
        (.not. gms%ksiz) .or. (gms%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    call gmc%Duplicate (dupl=tgmc)
    call gms%Duplicate (dupl=tgms)

    !Identify an atom in QM cluster that has a match in QM surface.
    jj = 0
    do ii = 1, gmi%numa
      if (gmi%idxs(ii)/=-1) then
        jj = gmi%idxs(ii)
        exit
      end if
    end do
    if ((jj<1) .or. (jj>tgms%numa)) call PrintError (ekey=3011, lstop=.true.)

    !Convert all coordinates to atomic units. Then translate all atoms in QM cluster and QM surface to bring
    !respective pivots at origin.
    call tgmc%Translate (dx=-tgmc%cx(ii), dy=-tgmc%cy(ii), dz=-tgmc%cz(ii))
    tgms%cx = tgms%cx * A2B
    tgms%cy = tgms%cy * A2B
    tgms%cz = tgms%cz * A2B
    call tgms%Translate (dx=-tgms%cx(jj), dy=-tgms%cy(jj), dz=-tgms%cz(jj))

    !All free atoms in QM cluster must be matched in QM surface.
    do ii = 1, tgmc%numa
      if (.not. tgmc%fix(ii)) then
        jj = gmi%idxs(ii)
          if ((jj<1) .or. (jj>tgms%numa)) call PrintError (ekey=3011, lstop=.true.)
        dx = tgmc%cx(ii) - tgms%cx(jj)
        dy = tgmc%cy(ii) - tgms%cy(jj)
        dz = tgmc%cz(ii) - tgms%cz(jj)
        if ((abs(dx)>mtol) .or. (abs(dy)>mtol) .or. (abs(dz)>mtol)) call PrintError (ekey=3012, lstop=.true.)
      end if
    end do

    !Clean up memory.
    call tgmc%Erase()
    call tgms%Erase()

  end subroutine ValidateSmssIndices_tcdvpdsg
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                       INTERFACE SYNCHRONIZESMSSCOORDINATES                                       !
  !******************************************************************************************************************!
  subroutine SynchronizeSmssCoordinates_dcdsg (gmm, gmi)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: B2A
    use CoordinateFileFormats, only: ConfigData
    implicit none

    type(ConfigData),   intent(inout) :: gmm     !MM system geometry (Angstrom).
    type(SmssGeometry), intent(in)    :: gmi     !SMSS geometry (matching indices and coordinate shifts (Bohr)).

    integer :: ii, jj

    procname = 'SolvationScheme::SynchronizeSmssCoordinates_dcdsg'
    if (ltrace) call PrintTrace()

    !For each free atom in the QM cluster, update the coordinates of its counterpart in MM system.
    if ((.not. gmi%ksiz) .or. (gmi%numa<1) .or.  &
        (.not. gmm%ksiz) .or. (gmm%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    do ii = 1, gmi%numa
      if (.not. gmi%fix(ii)) then
        jj = gmi%idxm(ii)
          if ((jj<1) .or. (jj>gmm%numa)) call PrintError (ekey=3011, lstop=.true.)
        gmm%cx(jj) = gmm%cx(jj) + gmi%cx(ii)*B2A
        gmm%cy(jj) = gmm%cy(jj) + gmi%cy(ii)*B2A
        gmm%cz(jj) = gmm%cz(jj) + gmi%cz(ii)*B2A
      end if
    end do

  end subroutine SynchronizeSmssCoordinates_dcdsg

  subroutine SynchronizeSmssCoordinates_vpdsg (gms, gmi)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: B2A
    use CoordinateFileFormats, only: PoscarData
    implicit none

    type(PoscarData),   intent(inout) :: gms     !QM surface geometry (Angstrom).
    type(SmssGeometry), intent(in)    :: gmi     !SMSS geometry (matching indices and coordinate shifts (Bohr)).

    integer :: ii, jj

    procname = 'SolvationScheme::SynchronizeSmssCoordinates_vpdsg'
    if (ltrace) call PrintTrace()

    !For each free atom in the QM cluster, update the fractional coordinates of its counterpart in QM surface.
    if ((.not. gmi%ksiz) .or. (gmi%numa<1) .or.  &
        (.not. gms%ksiz) .or. (gms%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    do ii = 1, gmi%numa
      if (.not. gmi%fix(ii)) then
        jj = gmi%idxs(ii)
          if ((jj<1) .or. (jj>gms%numa)) call PrintError (ekey=3011, lstop=.true.)
        gms%cx(jj) = gms%cx(jj) + gmi%cx(ii)*B2A
        gms%cy(jj) = gms%cy(jj) + gmi%cy(ii)*B2A
        gms%cz(jj) = gms%cz(jj) + gmi%cz(ii)*B2A
      end if
    end do

  end subroutine SynchronizeSmssCoordinates_vpdsg
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                          INTERFACE CALCULATESMSSENERGY                                           !
  !******************************************************************************************************************!
  subroutine CalculateSmssEnergy_dpX2dpaX2 (emin, emio, tk, ecw)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp, zero, one, BZHoK
    implicit none

    real(dp), intent(in)    :: emin(:)     !Energies (current) for MM system frames (Hartree).
    real(dp), intent(in)    :: emio(:)     !Energies (reference) for MM system frames (Hartree).
    real(dp), intent(in)    :: tk          !Temperature (Kelvin).
    real(dp), intent(inout) :: ecw         !Free energy for ESMSS (Hartree).

    integer  :: ii
    real(dp) :: beta

    procname = 'SolvationScheme::CalculateSmssEnergy_dpX2dpaX2'
    if (ltrace) call PrintTrace()

    if ((size(emin)<1) .or. (size(emin)/=size(emio))) call PrintError (ekey=1101, lstop=.true.)
    if (tk<one) call PrintError (ekey=3042, lstop=.true.)

    beta = one / (BZHoK*tk)
    ecw  = zero
    do ii = 1, size(emin)
      ecw = ecw + exp(beta*(emio(ii)-emin(ii)))
    end do
    ecw = -log(ecw/real(size(emin),dp)) / beta

  end subroutine CalculateSmssEnergy_dpX2dpaX2

  subroutine CalculateSmssEnergy_dpX3 (es, ec, ecw)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp, eV2H
    implicit none

    real(dp), intent(in)    :: es      !Energy for QM surface (eV).
    real(dp), intent(in)    :: ec      !Energy for QM cluster (Hartree).
    real(dp), intent(inout) :: ecw     !Energy for QM cluster in implicit solvent / ISMSS (Hartree).

    procname = 'SolvationScheme::CalculateSmssEnergy_dpX3'
    if (ltrace) call PrintTrace()

    ecw = es*eV2H - ec + ecw

  end subroutine CalculateSmssEnergy_dpX3

  subroutine CalculateSmssEnergy_dpX4dpa (es, ec, ecw, em, emi)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp, eV2H
    implicit none

    real(dp), intent(in)    :: es         !Energy for QM surface (eV).
    real(dp), intent(in)    :: ec         !Energy for QM cluster (Hartree).
    real(dp), intent(in)    :: ecw        !Energy for QM cluster in explicit solvent (Hartree).
    real(dp), intent(in)    :: em         !Energy for MM system (Hartree).
    real(dp), intent(inout) :: emi(:)     !Energies for MM system frames / ESMSS (Hartree).

    real(dp) :: shift

    procname = 'SolvationScheme::CalculateSmssEnergy_dpX4dpa'
    if (ltrace) call PrintTrace()

    if (size(emi)<1) call PrintError (ekey=1101, lstop=.true.)
    shift = es*eV2H - ec + ecw + em - emi(1)
    emi   = emi + shift

  end subroutine CalculateSmssEnergy_dpX4dpa
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                         INTERFACE CALCULATESMSSGRADIENTS                                         !
  !******************************************************************************************************************!
  subroutine CalculateSmssGradients_tcdX2vpdsg (gmc, gms, gmcw, gmi)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: zero, eVoA2HoB
    use CoordinateFileFormats, only: CoordData, PoscarData
    implicit none

    type(CoordData),    intent(in)    :: gmc      !QM cluster (gradients in a.u.).
    type(PoscarData),   intent(in)    :: gms      !QM surface (gradients in eV/Angstrom).
    type(CoordData),    intent(inout) :: gmcw     !QM cluster in implicit solvent / ISMSS (gradients in a.u.).
    type(SmssGeometry), intent(in)    :: gmi      !SMSS geometry (matching indices).

    integer :: ii, jj

    procname = 'SolvationScheme::CalculateSmssGradients_tcdX2vpdsg'
    if (ltrace) call PrintTrace()

    !Calculate total gradients only for free atoms in QM cluster.
    if ((.not. gmi%ksiz)  .or. (gmi%numa<1)          .or.  &
        (.not. gmc%ksiz)  .or. (gmc%numa/=gmi%numa)  .or.  &
        (.not. gmcw%ksiz) .or. (gmcw%numa/=gmi%numa) .or.  &
        (.not. gms%ksiz)  .or. (gms%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    do ii = 1, gmc%numa
      if (.not. gmc%fix(ii)) then
        jj = gmi%idxs(ii)
          if ((jj<1) .or. (jj>gms%numa)) call PrintError (ekey=3011, lstop=.true.)
        gmcw%fx(ii) = gms%fx(jj)*eVoA2HoB - gmc%fx(ii) + gmcw%fx(ii)
        gmcw%fy(ii) = gms%fy(jj)*eVoA2HoB - gmc%fy(ii) + gmcw%fy(ii)
        gmcw%fz(ii) = gms%fz(jj)*eVoA2HoB - gmc%fz(ii) + gmcw%fz(ii)
      end if
    end do

  end subroutine CalculateSmssGradients_tcdX2vpdsg

  subroutine CalculateSmssGradients_tcdX5vpdsg (gmc, gms, gmmr, gmme, gmmi, gmcw, gmi)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: zero, eVoA2HoB
    use CoordinateFileFormats, only: CoordData, PoscarData
    implicit none

    type(CoordData),    intent(in)    :: gmc      !QM cluster (gradients in a.u.).
    type(PoscarData),   intent(in)    :: gms      !QM surface (gradients in eV/Angstrom).
    type(CoordData),    intent(in)    :: gmmr     !QM cluster (reference) in MM system (gradients in a.u.).
    type(CoordData),    intent(in)    :: gmme     !QM cluster (ensemble) in MM system (gradients in a.u.).
    type(CoordData),    intent(in)    :: gmmi     !QM cluster (first) in MM system (gradients in a.u.).
    type(CoordData),    intent(inout) :: gmcw     !QM cluster in explicit solvent / ESMSS (gradients in a.u.).
    type(SmssGeometry), intent(in)    :: gmi      !SMSS geometry (matching indices).

    integer :: ii, jj

    procname = 'SolvationScheme::CalculateSmssGradients_tcdX5vpdsg'
    if (ltrace) call PrintTrace()

    !Calculate total gradients only for free atoms in QM cluster.
    if ((.not. gmi%ksiz)  .or. (gmi%numa<1)          .or.  &
        (.not. gmc%ksiz)  .or. (gmc%numa/=gmi%numa)  .or.  &
        (.not. gmcw%ksiz) .or. (gmcw%numa/=gmi%numa) .or.  &
        (.not. gmmr%ksiz) .or. (gmmr%numa/=gmi%numa) .or.  &
        (.not. gmme%ksiz) .or. (gmme%numa/=gmi%numa) .or.  &
        (.not. gmmi%ksiz) .or. (gmmi%numa/=gmi%numa) .or.  &
        (.not. gms%ksiz)  .or. (gms%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    do ii = 1, gmc%numa
      if (.not. gmc%fix(ii)) then
        jj = gmi%idxs(ii)
          if ((jj<1) .or. (jj>gms%numa)) call PrintError (ekey=3011, lstop=.true.)
        gmcw%fx(ii) = gms%fx(jj)*eVoA2HoB - gmc%fx(ii) + gmcw%fx(ii) + gmmr%fx(ii) + gmme%fx(ii) - gmmi%fx(ii)
        gmcw%fy(ii) = gms%fy(jj)*eVoA2HoB - gmc%fy(ii) + gmcw%fy(ii) + gmmr%fy(ii) + gmme%fy(ii) - gmmi%fy(ii)
        gmcw%fz(ii) = gms%fz(jj)*eVoA2HoB - gmc%fz(ii) + gmcw%fz(ii) + gmmr%fz(ii) + gmme%fz(ii) - gmmi%fz(ii)
      end if
    end do

  end subroutine CalculateSmssGradients_tcdX5vpdsg
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                           INTERFACE PREPAREPROGRAMJOB                                            !
  !******************************************************************************************************************!
  subroutine PrepareProgramJob_sX3 (sdir, tdir, prog)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: pl, sl
    use UtilityProcedures, only: AppendCharacter, ConvertToUpperCase, FileExists
    use CommandExecutors,  only: CopyFile
    implicit none

    character(len=*), intent(in) :: sdir, tdir     !Directories (source/target).
    character(len=*), intent(in) :: prog           !Which program?

    character(len=sl) :: file(1:20)
    character(len=pl) :: tprog, tsdir, fpath
    integer           :: ii

    procname = 'SolvationScheme::PrepareProgramJob_sX3'
    if (ltrace) call PrintTrace()

    !Validate input parameters.
    if (len_trim(sdir)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: sdir')
    if (len_trim(tdir)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: tdir')
    if (len_trim(prog)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: prog')

    if (len(tprog)<len(prog)) call PrintError (ekey=1122, lstop=.true.)
    tprog = adjustl(prog)
    call ConvertToUpperCase (line=tprog)

    !Depending upon the program, create a list of files to be copied.
    if (tprog=='COSMO') then
      file(1:5)   = (/ 'geometry.xyz', 'coord', 'control', 'control.dscf', 'control.ridft' /)
      file(6:10)  = (/ 'mos', 'alpha', 'beta', 'basis', 'auxbasis' /)
      file(11:15) = (/ 'energy', 'gradient', 'dscf.out', 'grad.out', 'ridft.out' /)
      file(16:20) = (/ 'rdgrad.out', 'out.ccf', ' ', ' ', ' ' /)

    else if (tprog=='DLPOLY') then
      file(1:5)   = (/ 'CONFIG', 'CONTROL', 'FIELD', 'REVCON', 'REVIVE' /)
      file(6:10)  = (/ 'HISTORY', 'OUTPUT', 'STATIS', 'FIELD_TEMPLATE', 'MMS_ENSEMBLE' /)
      file(11:15) = (/ ' ', ' ', ' ', ' ', ' ' /)
      file(16:20) = (/ ' ', ' ', ' ', ' ', ' ' /)

    else if (tprog=='PEECM') then
      file(1:5)   = (/ 'geometry.xyz', 'coord', 'control', 'control.dscf', 'control.ridft' /)
      file(6:10)  = (/ 'mos', 'alpha', 'beta', 'basis', 'auxbasis' /)
      file(11:15) = (/ 'embedded', 'energy', 'gradient', 'dscf.out', 'grad.out' /)
      file(16:20) = (/ 'ridft.out', 'rdgrad.out', ' ', ' ', ' ' /)

    else if (tprog=='TURBOMOLE') then
      file(1:5)   = (/ 'geometry.xyz', 'coord', 'control', 'control.dscf', 'control.ridft' /)
      file(6:10)  = (/ 'mos', 'alpha', 'beta', 'basis', 'auxbasis' /)
      file(11:15) = (/ 'energy', 'gradient', 'dscf.out', 'grad.out', 'ridft.out' /)
      file(16:20) = (/ 'rdgrad.out', ' ', ' ', ' ', ' ' /)

    else if (tprog=='VASP') then
      file(1:5)   = (/ 'INCAR', 'KPOINTS', 'POSCAR', 'POTCAR', 'MODECAR' /)
      file(6:10)  = (/ 'WAVECAR', 'OUTCAR', 'OSZICAR', ' ', ' ' /)
      file(11:15) = (/ ' ', ' ', ' ', ' ', ' ' /)
      file(16:20) = (/ ' ', ' ', ' ', ' ', ' ' /)

    else
      call PrintError (ekey=1302, lstop=.true., msg1='Keyword: prog')
    end if

    !Copy all files from source directory to target directory. None of these files should be replaced.
    if (len(tsdir)<len(sdir)) call PrintError (ekey=1122, lstop=.true.)
    tsdir = adjustl(sdir)
    call AppendCharacter (line=tsdir, sym='/', lskip=.true.)
    do ii = 1, 20
      if (len(fpath)<len_trim(tsdir)+len_trim(file(ii))) call PrintError (ekey=1122, lstop=.true.)
      fpath = trim(tsdir) // trim(file(ii))
      if (FileExists(file=fpath))  &
        call CopyFile (sdir=sdir, tdir=tdir, sfile=file(ii), tfile=file(ii), lreplace=.false.)
    end do

  end subroutine PrepareProgramJob_sX3
  !******************************************************************************************************************!

end module SolvationScheme
!***********************************************************************************************************************
