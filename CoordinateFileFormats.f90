!**********************************************************************************************************************!
!This module provides the definitions and supporting functionality for various coordinate file-types' objects.         !
!**********************************************************************************************************************!
module CoordinateFileFormats

  use Parameters, only: dp, sl
  implicit none

  public

  !******************************************************************************************************************!
  !                                        CONFIGDATA (DCD) OBJECT DEFINITION                                        !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'ksiz' is the status of allocatable arrays.                                                                       !
  !'numa' is the total number of atoms.                                                                              !
  !'type' and 'idx' are allocatble arrays containing atomic symbols and indices for all atoms.                       !
  !'cx', 'cy', and 'cz' are allocatable arrays containing Cartesian coordinates for all atoms.                       !
  !'vx', 'vy', and 'vz' are allocatable arrays containing velocity components for all atoms.                         !
  !'fx', 'fy', and 'fz' are allocatable arrays containing force components for all atoms.                            !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'Resize' resizes all allocatable arrays of a specified section.                                                   !
  !'Erase' releases memory from all allocatable arrays of all sections.                                              !
  !'Duplicate' creates a complete copy of the object.                                                                !
  !'ReadFromFile' reads data from a text file.                                                                       !
  !'WriteToFile' writes data to a text file.                                                                         !
  !'Translate' translates all atoms by a specified distance.                                                         !
  !'ApplyPeriodicBoundary' relocates all atoms to positive quadrants.                                                !
  !******************************************************************************************************************!
  type ConfigData
      logical,                       public :: ksiz = .false.
      integer,                       public :: numa = 0
      character(len=6), allocatable, public :: type(:)
      integer,          allocatable, public :: idx(:)
      real(dp),         allocatable, public :: cx(:), cy(:), cz(:)
      real(dp),         allocatable, public :: vx(:), vy(:), vz(:)
      real(dp),         allocatable, public :: fx(:), fy(:), fz(:)
    contains
      procedure, public :: Resize                => Resize_dcd
      procedure, public :: Erase                 => Erase_dcd
      procedure, public :: Duplicate             => Duplicate_dcdX2
      procedure, public :: ReadFromFile          => ReadFromFile_igtfdcd
      procedure, public :: WriteToFile           => WriteToFile_igtfdcd
      procedure, public :: Translate             => Translate_dpX3dcd
      procedure, public :: ApplyPeriodicBoundary => ApplyPeriodicBoundary_dpX3dcd
  end type ConfigData

  !******************************************************************************************************************!
  !                                       CONFIGHEADER (DCH) OBJECT DEFINITION                                       !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'name' is the descriptive name of the geometry/job.                                                               !
  !'kfrm' specifies file format (0:coordinates, 1:coordinates and forces, 2:coordinates, forces and velocitites).    !
  !'kpbc' specifies the type of periodic-boundary conditions.                                                        !
  !'numa' is the total number of atoms.                                                                              !
  !'box' is a 3x3 matrix of periodic box dimensions.                                                                 !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'Duplicate' creates a complete copy of the object.                                                                !
  !'ReadFromFile' reads data from a text file.                                                                       !
  !'WriteToFile' writes data to a text file.                                                                         !
  !******************************************************************************************************************!
  type ConfigHeader
      logical,           public :: ksiz = .false.
      character(len=sl), public :: name
      integer,           public :: kfrm
      integer,           public :: kpbc
      integer,           public :: numa
      real(dp),          public :: box(1:3,1:3)
    contains
      procedure, public :: Duplicate    => Duplicate_dchX2
      procedure, public :: ReadFromFile => ReadFromFile_gtfdch
      procedure, public :: WriteToFile  => WriteToFile_gtfdch
  end type ConfigHeader

  !******************************************************************************************************************!
  !                                        COORDDATA (TCD) OBJECT DEFINITION                                         !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'ksiz' is the status of allocatable arrays.                                                                       !
  !'numa' is the total number of atoms.                                                                              !
  !'type' is an allocatble array containing atomic symbols for all atoms.                                            !
  !'fix' is an allocatable array containing permissions to move atoms.                                               !
  !'cx', 'cy', and 'cz' are allocatable arrays containing Cartesian coordinates for all atoms.                       !
  !'fx', 'fy', and 'fz' are allocatable arrays containing force components for all atoms (added functionality).      !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'Resize' resizes all allocatable arrays of a specified section.                                                   !
  !'Erase' releases memory from all allocatable arrays of all sections.                                              !
  !'Duplicate' creates a complete copy of the object.                                                                !
  !'ReadFromFile' reads data from a text file.                                                                       !
  !'WriteToFile' writes data to a text file.                                                                         !
  !'Translate' translates all atoms by a specified distance.                                                         !
  !******************************************************************************************************************!
  type CoordData
      logical,                       public :: ksiz = .false.
      integer,                       public :: numa = 0
      character(len=2), allocatable, public :: type(:)
      logical,          allocatable, public :: fix(:)
      real(dp),         allocatable, public :: cx(:), cy(:), cz(:)
      real(dp),         allocatable, public :: fx(:), fy(:), fz(:)
    contains
      procedure, public :: Resize       => Resize_tcd
      procedure, public :: Erase        => Erase_tcd
      procedure, public :: Duplicate    => Duplicate_tcdX2
      procedure, public :: ReadFromFile => ReadFromFile_gtftcd
      procedure, public :: WriteToFile  => WriteToFile_gtftcd
      procedure, public :: Translate    => Translate_dpX3tcd
  end type CoordData

  !******************************************************************************************************************!
  !                                        POSCARDATA (VPD) OBJECT DEFINITION                                        !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'ksiz' is the status of allocatable arrays.                                                                       !
  !'numa' is the total number of atoms.                                                                              !
  !'cx', 'cy', and 'cz' are allocatable arrays containing Cartesian coordinates for all atoms.                       !
  !'fx', 'fy', and 'fz' are allocatable arrays containing force components for all atoms (added functionality).      !
  !'mx', 'my', and 'mz' are allocatable arrays containing permissions to move for all atoms.                         !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'Resize' resizes all allocatable arrays of a specified section.                                                   !
  !'Erase' releases memory from all allocatable arrays of all sections.                                              !
  !'Duplicate' creates a complete copy of the object.                                                                !
  !'ReadFromFile' reads data from a text file.                                                                       !
  !'WriteToFile' writes data to a text file.                                                                         !
  !'Translate' translates all atoms by a specified distance.                                                         !
  !'ApplyPeriodicBoundary' relocates all atoms to positive quadrants.                                                !
  !******************************************************************************************************************!
  type PoscarData
      logical,                       public :: ksiz = .false.
      integer,                       public :: numa = 0
      real(dp),         allocatable, public :: cx(:), cy(:), cz(:)
      character(len=1), allocatable, public :: mx(:), my(:), mz(:)
      real(dp),         allocatable, public :: fx(:), fy(:), fz(:)
    contains
      procedure, public :: Resize                => Resize_vpd
      procedure, public :: Erase                 => Erase_vpd
      procedure, public :: Duplicate             => Duplicate_vpdX2
      procedure, public :: ReadFromFile          => ReadFromFile_gtfvpd
      procedure, public :: WriteToFile           => WriteToFile_gtfvpd
      procedure, public :: Translate             => Translate_dpX3vpd
      procedure, public :: ApplyPeriodicBoundary => ApplyPeriodicBoundary_dpX3vpd
  end type PoscarData

  !******************************************************************************************************************!
  !                                       POSCARHEADER (VPH) OBJECT DEFINITION                                       !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'ksiz' is the status of allocatable arrays.                                                                       !
  !'name' is the descriptive name of the geometry/job.                                                               !
  !'mult' is a universal scaling factor for the periodic box.                                                        !
  !'box' is a 3x3 matrix of periodic box dimensions.                                                                 !
  !'numt' is the number of unique atom types.                                                                        !
  !'typs' and 'nums' are allocatable arrays containing atomic symbols and number of atoms for all unique types.      !
  !'numa' is the total number of atoms.                                                                              !
  !'mtyp' specifies the type of movement of atoms (selective dynamics).                                              !
  !'ctyp' specifies the format of coordinates data (Cartesian/direct).                                               !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'Resize' resizes all allocatable arrays of a specified section.                                                   !
  !'Erase' releases memory from all allocatable arrays of all sections.                                              !
  !'Duplicate' creates a complete copy of the object.                                                                !
  !'ReadFromFile' reads data from a text file.                                                                       !
  !'WriteToFile' writes data to a text file.                                                                         !
  !******************************************************************************************************************!
  type PoscarHeader
      logical,                       public :: ksiz = .false.
      character(len=sl),             public :: name
      real(dp),                      public :: mult
      real(dp),                      public :: box(1:3,1:3)
      integer,                       public :: numt = 0
      character(len=2), allocatable, public :: typs(:)
      integer,          allocatable, public :: nums(:)
      integer,                       public :: numa
      character(len=sl),             public :: mtyp
      character(len=sl),             public :: ctyp
    contains
      procedure, public :: Resize       => Resize_vph
      procedure, public :: Erase        => Erase_vph
      procedure, public :: Duplicate    => Duplicate_vphX2
      procedure, public :: ReadFromFile => ReadFromFile_gtfvph
      procedure, public :: WriteToFile  => WriteToFile_gtfvph
  end type PoscarHeader

  !******************************************************************************************************************!
  !                                         XYZDATA (EXD) OBJECT DEFINITION                                          !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'ksiz' is the status of allocatable arrays.                                                                       !
  !'numa' is the total number of atoms.                                                                              !
  !'type' is an allocatble array containint atomic symbols for all atoms.                                            !
  !'cx', 'cy', and 'cz' are allocatable arrays containing Cartesian coordinates for all atoms.                       !
  !'fx', 'fy', and 'fz' are allocatable arrays containing force components for all atoms (added functionality).      !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'Resize' resizes all allocatable arrays of a specified section.                                                   !
  !'Erase' releases memory from all allocatable arrays of all sections.                                              !
  !'Duplicate' creates a complete copy of the object.                                                                !
  !'ReadFromFile' reads data from a text file.                                                                       !
  !'WriteToFile' writes data to a text file.                                                                         !
  !'Translate' translates all atoms by a specified distance.                                                         !
  !******************************************************************************************************************!
  type XyzData
      logical,                       public :: ksiz = .false.
      integer,                       public :: numa = 0
      character(len=6), allocatable, public :: type(:)
      real(dp),         allocatable, public :: cx(:), cy(:), cz(:)
      real(dp),         allocatable, public :: fx(:), fy(:), fz(:)
    contains
      procedure, public :: Resize       => Resize_exd
      procedure, public :: Erase        => Erase_exd
      procedure, public :: Duplicate    => Duplicate_exdX2
      procedure, public :: ReadFromFile => ReadFromFile_gtfexd
      procedure, public :: WriteToFile  => WriteToFile_gtfexd
      procedure, public :: Translate    => Translate_dpX3exd
  end type XyzData

  !******************************************************************************************************************!
  !                                        XYZHEADER (EXH) OBJECT DEFINITION                                         !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'name' is the descriptive name of the geometry/job.                                                               !
  !'numa' is the total number of atoms.                                                                              !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'Duplicate' creates a complete copy of the object.                                                                !
  !'ReadFromFile' reads data from a text file.                                                                       !
  !'WriteToFile' writes data to a text file.                                                                         !
  !******************************************************************************************************************!
  type XyzHeader
      integer,           public :: ksiz = -1
      character(len=sl), public :: name
      integer,           public :: numa
    contains
      procedure, public :: Duplicate    => Duplicate_exhX2
      procedure, public :: ReadFromFile => ReadFromFile_gtfexh
      procedure, public :: WriteToFile  => WriteToFile_gtfexh
  end type XyzHeader

  !******************************************************************************************************************!
  !                                               CONTAINED PROCEDURES                                               !
  !                                                                                                                  !
  !'ConvertCoordinates' changes the type of VASP POSCAR coodinates.                                                  !
  !'PeriodicBoxIsAligned' returns true if all periodic-box dimensions are aligned along Cartesian axes.              !
  !******************************************************************************************************************!

  interface ConvertCoordinates
    module procedure ConvertCoordinates_sdpavpd
  end interface ConvertCoordinates

  interface PeriodicBoxIsAligned
    module procedure PeriodicBoxIsAligned_dpa_l
  end interface PeriodicBoxIsAligned

  contains

  !******************************************************************************************************************!
  !                                              CONFIGDATA PROCEDURES                                               !
  !******************************************************************************************************************!
  subroutine Resize_dcd (self)
    use Debugger,         only: PrintError, procname, ltrace, PrintTrace
    use MemoryManagement, only: ResizeArray
    implicit none

    class(ConfigData), intent(inout) :: self

    procname = 'CoordinateFileFormats::Resize_dcd'
    if (ltrace) call PrintTrace()

    self%ksiz = (self%numa==size(self%type)) .and. (self%numa==size(self%idx)) .and.  &
                (self%numa==size(self%cx))   .and. (self%numa==size(self%cy))  .and.  &
                (self%numa==size(self%cz))   .and. (self%numa==size(self%vx))  .and.  &
                (self%numa==size(self%vy))   .and. (self%numa==size(self%vz))  .and.  &
                (self%numa==size(self%fx))   .and. (self%numa==size(self%fy))  .and.  &
                (self%numa==size(self%fz))

    if (.not. self%ksiz) then
      call ResizeArray (nume=self%numa, ida=self%type)
      call ResizeArray (nume=self%numa, ida=self%idx)
      call ResizeArray (nume=self%numa, ida=self%cx)
      call ResizeArray (nume=self%numa, ida=self%cy)
      call ResizeArray (nume=self%numa, ida=self%cz)
      call ResizeArray (nume=self%numa, ida=self%vx)
      call ResizeArray (nume=self%numa, ida=self%vy)
      call ResizeArray (nume=self%numa, ida=self%vz)
      call ResizeArray (nume=self%numa, ida=self%fx)
      call ResizeArray (nume=self%numa, ida=self%fy)
      call ResizeArray (nume=self%numa, ida=self%fz)
      self%ksiz = .true.
    end if

  end subroutine Resize_dcd

  subroutine Erase_dcd (self)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(ConfigData), intent(inout) :: self

    procname = 'CoordinateFileFormats::Erase_dcd'
    if (ltrace) call PrintTrace()

    self%numa = 0
    call self%Resize()
    self%ksiz = .false.

  end subroutine Erase_dcd

  subroutine Duplicate_dcdX2 (self, dupl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(ConfigData), intent(in)    :: self
    type(ConfigData),  intent(inout) :: dupl

    procname = 'CoordinateFileFormats::Duplicate_dcdX2'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    dupl%numa = self%numa
    call dupl%Resize()
    dupl%type = self%type
    dupl%idx  = self%idx
    dupl%cx   = self%cx
    dupl%cy   = self%cy
    dupl%cz   = self%cz
    dupl%vx   = self%vx
    dupl%vy   = self%vy
    dupl%vz   = self%vz
    dupl%fx   = self%fx
    dupl%fy   = self%fy
    dupl%fz   = self%fz

  end subroutine Duplicate_dcdX2

  subroutine ReadFromFile_igtfdcd (self, file, kfrm)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(ConfigData),     intent(inout) :: self
    type(GenericTextFile), intent(in)    :: file
    integer, optional,     intent(in)    :: kfrm     !File-format key.

    integer :: ifrm = 0     !By default, only coordinates are included.
    integer :: ii, code

    procname = 'CoordinateFileFormats::ReadFromFile_igtfdcd'
    if (ltrace) call PrintTrace()

    !Override default format?
    if (present(kfrm)) ifrm = kfrm
    if ((ifrm/=0) .and. (ifrm/=1) .and. (ifrm/=2)) call PrintError (ekey=1301, lstop=.true., msg1='Key: kfrm')

    !There must be at least 1 atom. Each record is written on 2-4 lines.
    !Line 1 has 2 entries: atomic symbol and atomic index.
    !Line 2 has 3 entries: (3) coordinates.
    !Line 3 (optional) has 3 entries: (3) force components.
    !Line 4 (optional) has 3 entires: (3) velocity components.
    if (.not. file%ksiz) call PrintError (ekey=1103, lstop=.true.)
    self%numa = file%numl / (ifrm+2)
    if ((self%numa<1) .or. (mod(file%numl,ifrm+2)/=0)) call PrintError (ekey=1103, lstop=.true.)
    call self%Resize()

    !If only coordinates are listed.
    if (ifrm==0) then
      do ii = 1, self%numa
        read (file%line(ii*2-1),*,iostat=code) self%type(ii), self%idx(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true.)
        read (file%line(ii*2),*,iostat=code)   self%cx(ii), self%cy(ii), self%cz(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true.)
      end do

    !If coordinates as well as force components are listed.
    else if (ifrm==1) then
      do ii = 1, self%numa
        read (file%line(ii*3-2),*,iostat=code) self%type(ii), self%idx(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true.)
        read (file%line(ii*3-1),*,iostat=code) self%cx(ii), self%cy(ii), self%cz(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true.)
        read (file%line(ii*3),*,iostat=code)   self%vx(ii), self%vy(ii), self%vz(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true.)
      end do

    !If coordinates as well as force and velocity components are listed.
    else if (ifrm==2) then
      do ii = 1, self%numa
        read (file%line(ii*4-3),*,iostat=code) self%type(ii), self%idx(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true.)
        read (file%line(ii*4-2),*,iostat=code) self%cx(ii), self%cy(ii), self%cz(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true.)
        read (file%line(ii*4-1),*,iostat=code) self%vx(ii), self%vy(ii), self%vz(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true.)
        read (file%line(ii*4),*,iostat=code)   self%fx(ii), self%fy(ii), self%fz(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true.)
      end do
    end if

  end subroutine ReadFromFile_igtfdcd

  subroutine WriteToFile_igtfdcd (self, file, kfrm)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: F012, F232
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(ConfigData),     intent(in)    :: self
    type(GenericTextFile), intent(inout) :: file
    integer, optional,     intent(in)    :: kfrm     !File-format key.

    integer :: ifrm = 0     !By default, only coordinates are included.
    integer :: ii, code

    procname = 'CoordinateFileFormats::WriteToFile_igtfdcd'
    if (ltrace) call PrintTrace()

    !Override default format?
    if (present(kfrm)) ifrm = kfrm
    if ((ifrm/=0) .and. (ifrm/=1) .and. (ifrm/=2)) call PrintError (ekey=1301, lstop=.true., msg1='Key: kfrm')

    !There must be at least 1 atom. Each record is written on 2-4 lines.
    !Line 1 has 2 entries: atomic symbol and atomic index.
    !Line 2 has 3 entries: (3) coordinates.
    !Line 3 (optional) has 3 entries: (3) force components.
    !Line 4 (optional) has 3 entires: (3) velocity components.
    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    file%numl = self%numa * (ifrm+2)
    call file%Resize()

    !If only coordinates should be written.
    if (ifrm==0) then
      do ii = 1, self%numa
        write (file%line(ii*2-1),F012,iostat=code) self%type(ii), self%idx(ii)
          if (code>0) call PrintError (ekey=2121, lstop=.true.)
        write (file%line(ii*2),F232,iostat=code)   self%cx(ii), self%cy(ii), self%cz(ii)
          if (code>0) call PrintError (ekey=2121, lstop=.true.)
      end do

    !If coordinates as well as force components should be written.
    else if (ifrm==1) then
      do ii = 1, self%numa
        write (file%line(ii*3-2),F012,iostat=code) self%type(ii), self%idx(ii)
          if (code>0) call PrintError (ekey=2121, lstop=.true.)
        write (file%line(ii*3-1),F232,iostat=code) self%cx(ii), self%cy(ii), self%cz(ii)
          if (code>0) call PrintError (ekey=2121, lstop=.true.)
        write (file%line(ii*3),F232,iostat=code)   self%vx(ii), self%vy(ii), self%vz(ii)
          if (code>0) call PrintError (ekey=2121, lstop=.true.)
      end do

    !If coordinates as well as force and velocity components should be written.
    else if (ifrm==2) then
      do ii = 1, self%numa
        write (file%line(ii*4-3),F012,iostat=code) self%type(ii), self%idx(ii)
          if (code>0) call PrintError (ekey=2121, lstop=.true.)
        write (file%line(ii*4-2),F232,iostat=code) self%cx(ii), self%cy(ii), self%cz(ii)
          if (code>0) call PrintError (ekey=2121, lstop=.true.)
        write (file%line(ii*4-1),F232,iostat=code) self%vx(ii), self%vy(ii), self%vz(ii)
          if (code>0) call PrintError (ekey=2121, lstop=.true.)
        write (file%line(ii*4),F232,iostat=code)   self%fx(ii), self%fy(ii), self%fz(ii)
          if (code>0) call PrintError (ekey=2121, lstop=.true.)
      end do
    end if

  end subroutine WriteToFile_igtfdcd

  subroutine Translate_dpX3dcd (self, dx, dy, dz)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp
    implicit none

    class(ConfigData),  intent(inout) :: self
    real(dp), optional, intent(in)    :: dx, dy, dz     !Shift in each coordinate.

    procname = 'CoordinateFileFormats::Translate_dpX3dcd'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    if (present(dx)) self%cx = self%cx + dx
    if (present(dy)) self%cy = self%cy + dy
    if (present(dz)) self%cz = self%cz + dz

  end subroutine Translate_dpX3dcd

  subroutine ApplyPeriodicBoundary_dpX3dcd (self, dx, dy, dz)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp, zero
    implicit none

    class(ConfigData),  intent(inout) :: self
    real(dp), optional, intent(in)    :: dx, dy, dz     !Periodic box dimensions.

    procname = 'CoordinateFileFormats::ApplyPeriodicBoundary_dpX3dcd'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    if (present(dx)) then
      where (self%cx<zero) self%cx = self%cx + dx
      where (self%cx>dx)   self%cx = self%cx - dx
    end if
    if (present(dy)) then
      where (self%cy<zero) self%cy = self%cy + dy
      where (self%cy>dy)   self%cy = self%cy - dy
    end if
    if (present(dz)) then
      where (self%cz<zero) self%cz = self%cz + dz
      where (self%cz>dz)   self%cz = self%cz - dz
    end if

  end subroutine ApplyPeriodicBoundary_dpX3dcd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                             CONFIGHEADER PROCEDURES                                              !
  !******************************************************************************************************************!
  subroutine Duplicate_dchX2 (self, dupl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(ConfigHeader), intent(in)    :: self
    type(ConfigHeader),  intent(inout) :: dupl

    procname = 'CoordinateFileFormats::Duplicate_dchX2'
    if (ltrace) call PrintTrace()

    dupl%name = self%name
    dupl%kfrm = self%kfrm
    dupl%kpbc = self%kpbc
    dupl%numa = self%numa
    dupl%box  = self%box

  end subroutine Duplicate_dchX2

  subroutine ReadFromFile_gtfdch (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: sl
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(ConfigHeader),   intent(inout) :: self
    type(GenericTextFile), intent(in)    :: file

    character(len=sl) :: buff
    integer           :: aa, code

    procname = 'CoordinateFileFormats::ReadFromFile_gtfdch'
    if (ltrace) call PrintTrace()

    !There are only 4 (or 5) lines.
    !For 5 lines, line 1 contains geometry/job title. For 4 lines, title is skipped.
    !Line 2 contains 3 entries for file-format, periodic-box type, and total number of atoms.
    !Lines 3-5 contain periodic-box dimensions.
    if ((.not. file%ksiz) .or. ((file%numl/=4) .and. (file%numl/=5))) call PrintError (ekey=1103, lstop=.true.)
    if (file%numl==5) then
      self%name = file%line(1)
      read (file%line(2),*,iostat=code) self%kfrm, self%kpbc, self%numa
        if (code>0) call PrintError (ekey=2111, lstop=.true.)
      if ((self%kfrm/=0) .and. (self%kfrm/=1) .and. (self%kfrm/=2))  &
        call PrintError (ekey=1304, lstop=.true., msg1='CONFIG: file-format')
      if ((self%kpbc/=1) .and. (self%kpbc/=2))  &
        call PrintError (ekey=1304, lstop=.true., msg1='CONFIG: periodic-box type')
      if (self%numa<1) call PrintError (ekey=1304, lstop=.true., msg1='CONFIG: number of atoms')
      read (file%line(3),*,iostat=code) self%box(1,1), self%box(1,2), self%box(1,3)
        if (code>0) call PrintError (ekey=2111, lstop=.true.)
      read (file%line(4),*,iostat=code) self%box(2,1), self%box(2,2), self%box(2,3)
        if (code>0) call PrintError (ekey=2111, lstop=.true.)
      read (file%line(5),*,iostat=code) self%box(3,1), self%box(3,2), self%box(3,3)
        if (code>0) call PrintError (ekey=2111, lstop=.true.)

    else if (file%numl==4) then
      read (file%line(1),*,iostat=code) buff, aa, self%numa, self%kfrm, self%kpbc
        if (code>0) call PrintError (ekey=2111, lstop=.true.)
      if ((self%kfrm/=0) .and. (self%kfrm/=1) .and. (self%kfrm/=2))  &
        call PrintError (ekey=1304, lstop=.true., msg1='CONFIG: file-format')
      if ((self%kpbc/=1) .and. (self%kpbc/=2))  &
        call PrintError (ekey=1304, lstop=.true., msg1='CONFIG: periodic-box type')
      if (self%numa<1) call PrintError (ekey=1304, lstop=.true., msg1='CONFIG: number of atoms')
      read (file%line(2),*,iostat=code) self%box(1,1), self%box(1,2), self%box(1,3)
        if (code>0) call PrintError (ekey=2111, lstop=.true.)
      read (file%line(3),*,iostat=code) self%box(2,1), self%box(2,2), self%box(2,3)
        if (code>0) call PrintError (ekey=2111, lstop=.true.)
      read (file%line(4),*,iostat=code) self%box(3,1), self%box(3,2), self%box(3,3)
        if (code>0) call PrintError (ekey=2111, lstop=.true.)
    end if

  end subroutine ReadFromFile_gtfdch

  subroutine WriteToFile_gtfdch (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: F115, F232
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(ConfigHeader),   intent(in)    :: self
    type(GenericTextFile), intent(inout) :: file

    integer :: code

    procname = 'CoordinateFileFormats::WriteToFile_gtfdch'
    if (ltrace) call PrintTrace()

    !There are only 5 lines. Line 1 contains geometry/job title.
    !Line 2 contains 3 entries for file-format, periodic-box type, and total number of atoms.
    !Lines 3-5 contain periodic-box dimensions.
    file%numl = 5
    call file%Resize()
    file%line(1) = self%name
    write (file%line(2),F115,iostat=code) self%kfrm, self%kpbc, self%numa
      if (code>0) call PrintError (ekey=2121, lstop=.true.)
    write (file%line(3),F232,iostat=code) self%box(1,1), self%box(1,2), self%box(1,3)
      if (code>0) call PrintError (ekey=2121, lstop=.true.)
    write (file%line(4),F232,iostat=code) self%box(2,1), self%box(2,2), self%box(2,3)
      if (code>0) call PrintError (ekey=2121, lstop=.true.)
    write (file%line(5),F232,iostat=code) self%box(3,1), self%box(3,2), self%box(3,3)
      if (code>0) call PrintError (ekey=2121, lstop=.true.)

  end subroutine WriteToFile_gtfdch
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                               COORDDATA PROCEDURES                                               !
  !******************************************************************************************************************!
  subroutine Resize_tcd (self)
    use Debugger,         only: PrintError, procname, ltrace, PrintTrace
    use MemoryManagement, only: ResizeArray
    implicit none

    class(CoordData), intent(inout) :: self

    procname = 'CoordinateFileFormats::Resize_tcd'
    if (ltrace) call PrintTrace()

    self%ksiz = (self%numa==size(self%type)) .and. (self%numa==size(self%fix)) .and.  &
                (self%numa==size(self%cx))   .and. (self%numa==size(self%cy))  .and.  &
                (self%numa==size(self%cz))   .and. (self%numa==size(self%fx))  .and.  &
                (self%numa==size(self%fy))   .and. (self%numa==size(self%fz))

    if (.not. self%ksiz) then
      call ResizeArray (nume=self%numa, ida=self%type)
      call ResizeArray (nume=self%numa, ida=self%fix)
      call ResizeArray (nume=self%numa, ida=self%cx)
      call ResizeArray (nume=self%numa, ida=self%cy)
      call ResizeArray (nume=self%numa, ida=self%cz)
      call ResizeArray (nume=self%numa, ida=self%fx)
      call ResizeArray (nume=self%numa, ida=self%fy)
      call ResizeArray (nume=self%numa, ida=self%fz)
      self%ksiz = .true.
    end if

  end subroutine Resize_tcd

  subroutine Erase_tcd (self)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(CoordData), intent(inout) :: self

    procname = 'CoordinateFileFormats::Erase_tcd'
    if (ltrace) call PrintTrace()

    self%numa = 0
    call self%Resize()
    self%ksiz = .false.

  end subroutine Erase_tcd

  subroutine Duplicate_tcdX2 (self, dupl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(CoordData), intent(in)    :: self
    type(CoordData),  intent(inout) :: dupl

    procname = 'CoordinateFileFormats::Duplicate_tcdX2'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    dupl%numa = self%numa
    call dupl%Resize()
    dupl%type = self%type
    dupl%fix  = self%fix
    dupl%cx   = self%cx
    dupl%cy   = self%cy
    dupl%cz   = self%cz
    dupl%fx   = self%fx
    dupl%fy   = self%fy
    dupl%fz   = self%fz

  end subroutine Duplicate_tcdX2

  subroutine ReadFromFile_gtftcd (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: sl
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(CoordData),      intent(inout) :: self
    type(GenericTextFile), intent(in)    :: file

    character(len=sl) :: buff
    character(len=1)  :: aa
    integer           :: ii, jj, code

    procname = 'CoordinateFileFormats::ReadFromFile_gtftcd'
    if (ltrace) call PrintTrace()

    !Locate $coord section.
    if ((.not. file%ksiz) .or. (file%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    jj = 0
    do ii = 1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:6)=='$coord') then
        jj = ii
        exit
      end if
    end do
    if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: $coord')

    !Count number of atoms in $coord section. There must be at least 1 atom.
    self%numa = 0
    do ii = jj+1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:1)=='$') exit
        self%numa = self%numa + 1
    end do
    if (self%numa==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: $coord')

    !Each record is written on 1 line.
    !There are 4 (or 5) entries per record: (3) coordinates, (1) atomic symbol, and optionally (1) permission to
    !move.
    call self%Resize()
    self%fix = .false.
    do ii = 1, self%numa
      aa = ' '
      read (file%line(jj+ii),*,iostat=code) self%cx(ii), self%cy(ii), self%cz(ii), self%type(ii), aa
        if (code>0) then
          read (file%line(jj+ii),*,iostat=code) self%cx(ii), self%cy(ii), self%cz(ii), self%type(ii)
            if (code>0) call PrintError (ekey=2111, lstop=.true.)
        end if
      if ((aa/='f') .and. (aa/='F') .and. (aa/=' ')) call PrintError (ekey=3401, lstop=.true.)
      if ((aa=='f') .or. (aa=='F')) self%fix(ii) = .true.
    end do

  end subroutine ReadFromFile_gtftcd

  subroutine WriteToFile_gtftcd (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: sl, F233
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(CoordData),      intent(in)    :: self
    type(GenericTextFile), intent(inout) :: file

    character(len=sl) :: buff
    character(len=1)  :: aa
    integer           :: ii, jj, code

    procname = 'CoordinateFileFormats::WriteToFile_gtftcd'
    if (ltrace) call PrintTrace()

    !There must be at least 1 atom. Each record is written on 1 line.
    !There are 4 (or 5) entries per record: (3) coordinates, (1) atomic symbol, and optionally (1) permission to
    !move.
    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    file%numl = self%numa + 2
    call file%Resize()
    file%line(1) = '$coord'
    do ii = 1, self%numa
      buff = '   ' // self%type(ii)
      if (self%fix(ii)) buff = trim(buff) // '   f'
      write (file%line(ii+1),F233,iostat=code) self%cx(ii), self%cy(ii), self%cz(ii), trim(buff)
        if (code>0) call PrintError (ekey=2121, lstop=.true.)
    end do
    file%line(file%numl) = '$end'

  end subroutine WriteToFile_gtftcd

  subroutine Translate_dpX3tcd (self, dx, dy, dz)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp
    implicit none

    class(CoordData),   intent(inout) :: self
    real(dp), optional, intent(in)    :: dx, dy, dz     !Shift in each coordinate.

    procname = 'CoordinateFileFormats::Translate_dpX3tcd'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    if (present(dx)) self%cx = self%cx + dx
    if (present(dy)) self%cy = self%cy + dy
    if (present(dz)) self%cz = self%cz + dz

  end subroutine Translate_dpX3tcd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                              POSCARDATA PROCEDURES                                               !
  !******************************************************************************************************************!
  subroutine Resize_vpd (self)
    use Debugger,         only: PrintError, procname, ltrace, PrintTrace
    use MemoryManagement, only: ResizeArray
    implicit none

    class(PoscarData), intent(inout) :: self

    procname = 'CoordinateFileFormats::Resize_vpd'
    if (ltrace) call PrintTrace()

    self%ksiz = (self%numa==size(self%cx)) .and. (self%numa==size(self%cy)) .and.  &
                (self%numa==size(self%cz)) .and. (self%numa==size(self%mx)) .and.  &
                (self%numa==size(self%my)) .and. (self%numa==size(self%mz)) .and.  &
                (self%numa==size(self%fx)) .and. (self%numa==size(self%my)) .and.  &
                (self%numa==size(self%fz))

    if (.not. self%ksiz) then
      call ResizeArray (nume=self%numa, ida=self%cx)
      call ResizeArray (nume=self%numa, ida=self%cy)
      call ResizeArray (nume=self%numa, ida=self%cz)
      call ResizeArray (nume=self%numa, ida=self%mx)
      call ResizeArray (nume=self%numa, ida=self%my)
      call ResizeArray (nume=self%numa, ida=self%mz)
      call ResizeArray (nume=self%numa, ida=self%fx)
      call ResizeArray (nume=self%numa, ida=self%fy)
      call ResizeArray (nume=self%numa, ida=self%fz)
      self%ksiz = .true.
    end if

  end subroutine Resize_vpd

  subroutine Erase_vpd (self)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(PoscarData), intent(inout) :: self

    procname = 'CoordinateFileFormats::Erase_vpd'
    if (ltrace) call PrintTrace()

    self%numa = 0
    call self%Resize()
    self%ksiz = .false.

  end subroutine Erase_vpd

  subroutine Duplicate_vpdX2 (self, dupl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(PoscarData), intent(in)    :: self
    type(PoscarData),  intent(inout) :: dupl

    procname = 'CoordinateFileFormats::Duplicate_vpdX2'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    dupl%numa = self%numa
    call dupl%Resize()
    dupl%cx   = self%cx
    dupl%cy   = self%cy
    dupl%cz   = self%cz
    dupl%mx   = self%mx
    dupl%my   = self%my
    dupl%mz   = self%mz
    dupl%fx   = self%fx
    dupl%fy   = self%fy
    dupl%fz   = self%fz

  end subroutine Duplicate_vpdX2

  subroutine ReadFromFile_gtfvpd (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(PoscarData),     intent(inout) :: self
    type(GenericTextFile), intent(in)    :: file

    integer :: ii, code

    procname = 'CoordinateFileFormats::ReadFromFile_gtfvpd'
    if (ltrace) call PrintTrace()

    !There must be at least 1 atom. Each record is written on 1 line.
    !There are 3 (or 6) entries per record: (3) coordinates and optionally (3) atomic movements.
    !If the atomic movements are not explicitly specified, they are treated as free.
    if ((.not. file%ksiz) .or. (file%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    self%numa = file%numl
    call self%Resize()
    do ii = 1, self%numa
      read (file%line(ii),*,iostat=code) self%cx(ii), self%cy(ii), self%cz(ii),  &
                                         self%mx(ii), self%my(ii), self%mz(ii)
        if (code>0) then
          read (file%line(ii),*,iostat=code) self%cx(ii), self%cy(ii), self%cz(ii)
            if (code>0) call PrintError (ekey=2111, lstop=.true.)
        end if
      if (self%mx(ii)==' ') self%mx(ii) = 'T'
      if (self%my(ii)==' ') self%my(ii) = 'T'
      if (self%mz(ii)==' ') self%mz(ii) = 'T'
      if ((self%mx(ii)/='f') .and. (self%mx(ii)/='F') .and. (self%mx(ii)/='t') .and. (self%mx(ii)/='T'))  &
        call PrintError (ekey=3211, lstop=.true.)
      if ((self%my(ii)/='f') .and. (self%my(ii)/='F') .and. (self%my(ii)/='t') .and. (self%my(ii)/='T'))  &
        call PrintError (ekey=3211, lstop=.true.)
      if ((self%mz(ii)/='f') .and. (self%mz(ii)/='F') .and. (self%mz(ii)/='t') .and. (self%mz(ii)/='T'))  &
        call PrintError (ekey=3211, lstop=.true.)
    end do

  end subroutine ReadFromFile_gtfvpd

  subroutine WriteToFile_gtfvpd (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: sl, F233
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(PoscarData),     intent(in)    :: self
    type(GenericTextFile), intent(inout) :: file

    character(len=sl) :: buff
    integer           :: ii, code

    procname = 'CoordinateFileFormats::WriteToFile_gtfvpd'
    if (ltrace) call PrintTrace()

    !There must be at least 1 atom. Each record is written on 1 line.
    !There are 3 (or 6) entries per record: (3) coordinates and optionally (3) atomic movements.
    !If the atomic movements are not explicitly specified, they are treated as free.
    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    file%numl = self%numa
    call file%Resize()
    do ii = 1, self%numa
      buff = '   ' // self%mx(ii) // '   ' // self%my(ii) // '   ' // self%mz(ii)
      write (file%line(ii),F233,iostat=code) self%cx(ii), self%cy(ii), self%cz(ii), trim(buff)
        if (code>0) call PrintError (ekey=2121, lstop=.true.)
    end do

  end subroutine WriteToFile_gtfvpd

  subroutine Translate_dpX3vpd (self, dx, dy, dz)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp
    implicit none

    class(PoscarData),  intent(inout) :: self
    real(dp), optional, intent(in)    :: dx, dy, dz     !Shift in each coordinate.

    procname = 'CoordinateFileFormats::Translate_dpX3vpd'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    if (present(dx)) self%cx = self%cx + dx
    if (present(dy)) self%cy = self%cy + dy
    if (present(dz)) self%cz = self%cz + dz

  end subroutine Translate_dpX3vpd

  subroutine ApplyPeriodicBoundary_dpX3vpd (self, dx, dy, dz)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp, zero
    implicit none

    class(PoscarData),  intent(inout) :: self
    real(dp), optional, intent(in)    :: dx, dy, dz     !Periodic box dimensions.

    procname = 'CoordinateFileFormats::ApplyPeriodicBoundary_dpX3vpd'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<0)) call PrintError (ekey=1103, lstop=.true.)
    if (present(dx)) then
      where (self%cx<zero) self%cx = self%cx + dx
      where (self%cx>dx)   self%cx = self%cx - dx
    end if
    if (present(dy)) then
      where (self%cy<zero) self%cy = self%cy + dy
      where (self%cy>dy)   self%cy = self%cy - dy
    end if
    if (present(dz)) then
      where (self%cz<zero) self%cz = self%cz + dz
      where (self%cz>dz)   self%cz = self%cz - dz
    end if

  end subroutine ApplyPeriodicBoundary_dpX3vpd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                             POSCARHEADER PROCEDURES                                              !
  !******************************************************************************************************************!
  subroutine Resize_vph (self)
    use Debugger,         only: PrintError, procname, ltrace, PrintTrace
    use MemoryManagement, only: ResizeArray
    implicit none

    class(PoscarHeader), intent(inout) :: self

    procname = 'CoordinateFileFormats::Resize_vph'
    if (ltrace) call PrintTrace()

    self%ksiz = (self%numt==size(self%typs)) .and. (self%numt==size(self%nums))

    if (.not. self%ksiz) then
      call ResizeArray (nume=self%numt, ida=self%typs)
      call ResizeArray (nume=self%numt, ida=self%nums)
      self%ksiz = .true.
    end if

  end subroutine Resize_vph

  subroutine Erase_vph (self)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(PoscarHeader), intent(inout) :: self

    procname = 'CoordinateFileFormats::Erase_vph'
    if (ltrace) call PrintTrace()

    self%numt = 0
    call self%Resize()
    self%ksiz = .false.

  end subroutine Erase_vph

  subroutine Duplicate_vphX2 (self, dupl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(PoscarHeader), intent(in)    :: self
    type(PoscarHeader),  intent(inout) :: dupl

    procname = 'CoordinateFileFormats::Duplicate_vphX2'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numt<1)) call PrintError (ekey=1103, lstop=.true.)
    dupl%numt = self%numt
    call dupl%Resize()
    dupl%name = self%name
    dupl%mult = self%mult
    dupl%box  = self%box
    dupl%typs = self%typs
    dupl%nums = self%nums
    dupl%numa = self%numa
    dupl%mtyp = self%mtyp
    dupl%ctyp = self%ctyp

  end subroutine Duplicate_vphX2

  subroutine ReadFromFile_gtfvph (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(PoscarHeader),   intent(inout) :: self
    type(GenericTextFile), intent(in)    :: file

    integer          :: ii, jj, code
    character(len=2) :: typs(1:10) = ' '     !VASP has a limit of 10 unique atom types.
    integer          :: nums(1:10) = 0

    procname = 'CoordinateFileFormats::ReadFromFile_vph'
    if (ltrace) call PrintTrace()

    !There are only 9 lines.
    !Line 1 contains geometry/job title. Line 2 contains a universal scaling factor.
    !Lines 3-5 contain periodic-box dimensions.
    !Line 6 contains atomic symbols for all unique atom types.
    !Line 7 contains corresponding number of atoms of each type.
    !Line 8 contains keyword for movement of atoms. Line 9 contains keyword for format of atomic coordinates.
    if ((.not. file%ksiz) .or. (file%numl/=9)) call PrintError (ekey=1103, lstop=.true.)
    !Read lines 1-5.
    self%name = file%line(1)
    read (file%line(2),*,iostat=code) self%mult
      if (code>0) call PrintError (ekey=2111, lstop=.true.)
    read (file%line(3),*,iostat=code) self%box(1,1), self%box(1,2), self%box(1,3)
      if (code>0) call PrintError (ekey=2111, lstop=.true.)
    read (file%line(4),*,iostat=code) self%box(2,1), self%box(2,2), self%box(2,3)
      if (code>0) call PrintError (ekey=2111, lstop=.true.)
    read (file%line(5),*,iostat=code) self%box(3,1), self%box(3,2), self%box(3,3)
      if (code>0) call PrintError (ekey=2111, lstop=.true.)

    !Read lines 6-7 in temporary arrays. Then copy to respective members of the object.
    !There must be at least 1 atom type, but no more than 10.
    do ii = 1, 10
      read (file%line(6),*,iostat=code) typs(1:ii)
        if (code/=0) exit
    end do
    if (typs(1)==' ') call PrintError (ekey=3201, lstop=.true.)
    do jj = 1, 10
      read (file%line(7),*,iostat=code) nums(1:jj)
        if (code/=0) exit
    end do
    if (ii/=jj) call PrintError (ekey=3202, lstop=.true.)
    self%numt = ii - 1
    call self%Resize()
    self%typs(1:self%numt) = typs(1:self%numt)
    self%nums(1:self%numt) = nums(1:self%numt)
    if (any(self%nums<1)) call PrintError (ekey=1304, lstop=.true., msg1='POSCAR: number of atoms')
    self%numa = sum(self%nums)

    !Read lines 8-9.
    self%mtyp = file%line(8)
    self%ctyp = file%line(9)

  end subroutine ReadFromFile_gtfvph

  subroutine WriteToFile_gtfvph (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: sl, F101, F221, F232
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(PoscarHeader),   intent(in)    :: self
    type(GenericTextFile), intent(inout) :: file

    character(len=sl) :: buff
    integer           :: ii, jj, kk1, kk2, code

    procname = 'CoordinateFileFormats::WriteToFile_gtfvph'
    if (ltrace) call PrintTrace()

    !There are only 9 lines.
    !Line 1 contains geometry/job title. Line 2 contains a universal scaling factor.
    !Lines 3-5 contain periodic-box dimensions.
    !Line 6 contains atomic symbols for all unique atom types.
    !Line 7 contains corresponding number of atoms of each type.
    !Line 8 contains keyword for movement of atoms. Line 9 contains keyword for format of atomic coordinates.
    if (.not. self%ksiz) call PrintError (ekey=1103, lstop=.true.)
    if (self%numt<1)  call PrintError (ekey=3201, lstop=.true.)
    if (self%numt>10) call PrintError (ekey=3202, lstop=.true.)
    file%numl = 9
    call file%Resize()

    !Write lines 1-5.
    file%line(1) = self%name
    write (file%line(2),F221,iostat=code) self%mult
      if (code>0) call PrintError (ekey=2121, lstop=.true.)
    write (file%line(3),F232,iostat=code) self%box(1,1), self%box(1,2), self%box(1,3)
      if (code>0) call PrintError (ekey=2121, lstop=.true.)
    write (file%line(4),F232,iostat=code) self%box(2,1), self%box(2,2), self%box(2,3)
      if (code>0) call PrintError (ekey=2121, lstop=.true.)
    write (file%line(5),F232,iostat=code) self%box(3,1), self%box(3,2), self%box(3,3)
      if (code>0) call PrintError (ekey=2121, lstop=.true.)

    !Write lines 6-7 element-by-element to temporary strings. Pad using '+' symbols to keep elements on both
    !left-aligned. Finally, replace '+' characters with spaces.
    file%line(6) = ' '
    file%line(7) = ' '
    do ii = 1, self%numt
      file%line(6) = trim(file%line(6)) // '+++' // adjustl(self%typs(ii))
      write (buff,F101,iostat=code) self%nums(ii)
        if (code>0) call PrintError (ekey=2121, lstop=.true.)
      file%line(7) = trim(file%line(7)) // '+++' // trim(adjustl(buff))
      kk1 = len_trim(file%line(6))
      kk2 = len_trim(file%line(7))
      if (kk1<kk2) then
        do jj = 1, kk2-kk1
          file%line(6) = trim(file%line(6)) // '+'
        end do
      else if (kk2<kk1) then
        do jj = 1, kk1-kk2
          file%line(7) = trim(file%line(7)) // '+'
        end do
      end if
    end do
    do jj = 1, kk1
      if (file%line(6)(jj:jj)=='+') file%line(6)(jj:jj) = ' '
      if (file%line(7)(jj:jj)=='+') file%line(7)(jj:jj) = ' '
    end do

    !Write lines 8-9.
    file%line(8) = self%mtyp
    file%line(9) = self%ctyp

  end subroutine WriteToFile_gtfvph
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                                XYZDATA PROCEDURES                                                !
  !******************************************************************************************************************!
  subroutine Resize_exd (self)
    use Debugger,         only: PrintError, procname, ltrace, PrintTrace
    use MemoryManagement, only: ResizeArray
    implicit none

    class(XyzData), intent(inout) :: self

    procname = 'CoordinateFileFormats::Resize_exd'
    if (ltrace) call PrintTrace()

    self%ksiz = (self%numa==size(self%type)) .and. (self%numa==size(self%cx)) .and.  &
                (self%numa==size(self%cy))   .and. (self%numa==size(self%cz)) .and.  &
                (self%numa==size(self%fx))   .and. (self%numa==size(self%fy)) .and.  &
                (self%numa==size(self%fz))

    if (.not. self%ksiz) then
      call ResizeArray (nume=self%numa, ida=self%type)
      call ResizeArray (nume=self%numa, ida=self%cx)
      call ResizeArray (nume=self%numa, ida=self%cy)
      call ResizeArray (nume=self%numa, ida=self%cz)
      call ResizeArray (nume=self%numa, ida=self%fx)
      call ResizeArray (nume=self%numa, ida=self%fy)
      call ResizeArray (nume=self%numa, ida=self%fz)
      self%ksiz = .true.
    end if

  end subroutine Resize_exd

  subroutine Erase_exd (self)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(XyzData), intent(inout) :: self

    procname = 'CoordinateFileFormats::Erase_exd'
    if (ltrace) call PrintTrace()

    self%numa = 0
    call self%Resize()
    self%ksiz = .false.

  end subroutine Erase_exd

  subroutine Duplicate_exdX2 (self, dupl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(XyzData), intent(in)    :: self
    type(XyzData),  intent(inout) :: dupl

    procname = 'CoordinateFileFormats::Duplicate_exdX2'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    dupl%numa = self%numa
    call dupl%Resize()
    dupl%type = self%type
    dupl%cx   = self%cx
    dupl%cy   = self%cy
    dupl%cz   = self%cz
    dupl%fx   = self%fx
    dupl%fy   = self%fy
    dupl%fz   = self%fz

  end subroutine Duplicate_exdX2

  subroutine ReadFromFile_gtfexd (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(XyzData),        intent(inout) :: self
    type(GenericTextFile), intent(in)    :: file

    integer :: ii, code

    procname = 'CoordinateFileFormats::ReadFromFile_gtfexd'
    if (ltrace) call PrintTrace()

    !There must be at least 1 atom. Each record is written on 1 line.
    !There are 4 entries per record: atomic symbol and (3) coordinates.
    if ((.not. file%ksiz) .or. (file%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    self%numa = file%numl
    call self%Resize()
    do ii = 1, self%numa
      read (file%line(ii),*,iostat=code) self%type(ii), self%cx(ii), self%cy(ii), self%cz(ii)
        if (code>0) call PrintError (ekey=2111, lstop=.true.)
    end do

  end subroutine ReadFromFile_gtfexd

  subroutine WriteToFile_gtfexd (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: F021
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(XyzData),        intent(in)    :: self
    type(GenericTextFile), intent(inout) :: file

    integer :: ii, code

    procname = 'CoordinateFileFormats::WriteToFile_gtfexd'
    if (ltrace) call PrintTrace()

    !There must be at least 1 atom. Each record is written on 1 line.
    !There are 4 entries per record: atomic symbol and (3) coordinates.
    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    file%numl = self%numa
    call file%Resize()
    do ii = 1, self%numa
      write (file%line(ii),F021,iostat=code) self%type(ii), self%cx(ii), self%cy(ii), self%cz(ii)
        if (code>0) call PrintError (ekey=2121, lstop=.true.)
    end do

  end subroutine WriteToFile_gtfexd

  subroutine Translate_dpX3exd (self, dx, dy, dz)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp
    implicit none

    class(XyzData),     intent(inout) :: self
    real(dp), optional, intent(in)    :: dx, dy, dz     !Shift in each coordinate.

    procname = 'CoordinateFileFormats::Translate_dpX3exd'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    if (present(dx)) self%cx = self%cx + dx
    if (present(dy)) self%cy = self%cy + dy
    if (present(dz)) self%cz = self%cz + dz

  end subroutine Translate_dpX3exd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                               XYZHEADER PROCEDURES                                               !
  !******************************************************************************************************************!
  subroutine Duplicate_exhX2 (self, dupl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(XyzHeader), intent(in)    :: self
    type(XyzHeader),  intent(inout) :: dupl

    procname = 'CoordinateFileFormats::Duplicate_exhX2'
    if (ltrace) call PrintTrace()

    dupl%name = self%name
    dupl%numa = self%numa

  end subroutine Duplicate_exhX2

  subroutine ReadFromFile_gtfexh (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(XyzHeader),      intent(inout) :: self
    type(GenericTextFile), intent(in)    :: file

    integer :: code

    procname = 'CoordinateFileFormats::ReadFromFile_gtfexh'
    if (ltrace) call PrintTrace()

    !There are only 2 lines. Line 1 contains number of atoms. Line 2 contains geometry/job title.
    if ((.not. file%ksiz) .or. (file%numl/=2)) call PrintError (ekey=1103, lstop=.true.)
    read (file%line(1),*,iostat=code) self%numa
      if (code>0) call PrintError (ekey=2111, lstop=.true.)
    if (self%numa<1) call PrintError (ekey=1304, lstop=.true., msg1='XYZ: number of atoms')
    self%name = file%line(2)

  end subroutine ReadFromFile_gtfexh

  subroutine WriteToFile_gtfexh (self, file)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: F111
    use GenericInputOutput, only: GenericTextFile
    implicit none

    class(XyzHeader),      intent(in)    :: self
    type(GenericTextFile), intent(inout) :: file

    integer :: code

    procname = 'CoordinateFileFormats::WriteToFile_gtfexh'
    if (ltrace) call PrintTrace()

    !There are only 2 lines. Line 1 contains number of atoms. Line 2 contains geometry/job title.
    file%numl = 2
    call file%Resize()
    write (file%line(1),F111,iostat=code) self%numa
      if (code>0) call PrintError (ekey=2121, lstop=.true.)
    file%line(1) = adjustl(file%line(1))
    file%line(2) = self%name

  end subroutine WriteToFile_gtfexh
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                           INTERFACE CONVERTCOORDINATES                                           !
  !******************************************************************************************************************!
  subroutine ConvertCoordinates_sdpavpd (mode, box, gms)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: dp, sl
    use UtilityProcedures, only: ConvertToUpperCase
    implicit none

    character(len=*), intent(in)    :: mode             !Direction of coordinate conversion.
    real(dp),         intent(in)    :: box(1:3,1:3)     !Periodic box dimensions.
    type(PoscarData), intent(inout) :: gms              !Coordinates in given format.

    character(len=sl) :: tmode

    procname = 'CoordinateFileFormats::ConvertCoordinates_sdpavpd'
    if (ltrace) call PrintTrace()

    !Validate requested conversion mode.
    if (len(tmode)<len(mode)) call PrintError (ekey=1122, lstop=.true.)
    tmode = adjustl(mode)
    call ConvertToUpperCase (line=tmode)
    if ((tmode/='C2F') .and. (tmode/='CARTESIAN2FRACTIONAL') .and. (tmode/='CARTESIAN-TO-FRACTIONAL') .and.  &
        (tmode/='F2C') .and. (tmode/='FRACTIONAL2CARTESIAN') .and. (tmode/='FRACTIONAL-TO-CARTESIAN'))       &
      call PrintError (ekey=1302, lstop=.true., msg1='Keyword: mode')
    if ((.not. gms%ksiz)  .or. (gms%numa<1)) call PrintError (ekey=1103, lstop=.true.)

    !Convert coordinates.
    if (.not. PeriodicBoxIsAligned(box=box)) call PrintError (ekey=3031, lstop=.true.)
    if ((tmode=='C2F') .or. (tmode=='CARTESIAN2FRACTIONAL') .or. (tmode=='CARTESIAN-TO-FRACTIONAL')) then
      gms%cx = gms%cx / box(1,1)
      gms%cy = gms%cy / box(2,2)
      gms%cz = gms%cz / box(3,3)
    else if ((tmode=='F2C') .or. (tmode=='FRACTIONAL2CARTESIAN') .or. (tmode=='FRACTIONAL-TO-CARTESIAN')) then
      gms%cx = gms%cx * box(1,1)
      gms%cy = gms%cy * box(2,2)
      gms%cz = gms%cz * box(3,3)
    end if

  end subroutine ConvertCoordinates_sdpavpd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                          INTERFACE PERIODICBOXISALIGNED                                          !
  !******************************************************************************************************************!
  function PeriodicBoxIsAligned_dpa_l (box) result (lalign)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp, dtol, zero, one
    implicit none

    real(dp), intent(in) :: box(1:3,1:3)
    logical              :: lalign

    procname = 'CoordinateFileFormats::PeriodicBoxIsAligned_dpa_l'
    if (ltrace) call PrintTrace()

    lalign = (box(1,1)>one) .and. (box(2,2)>one) .and. (box(3,3)>one)  .and.  &
             (abs(box(1,2)-zero)<dtol) .and. (abs(box(1,3)-zero)<dtol) .and.  &
             (abs(box(2,1)-zero)<dtol) .and. (abs(box(2,3)-zero)<dtol) .and.  &
             (abs(box(3,1)-zero)<dtol) .and. (abs(box(3,2)-zero)<dtol)

  end function PeriodicBoxIsAligned_dpa_l
  !******************************************************************************************************************!

end module CoordinateFileFormats
!**********************************************************************************************************************!
