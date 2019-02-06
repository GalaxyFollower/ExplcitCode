!**********************************************************************************************************************!
!This module provides the definition and supporting functionality for a generic text-file object for disc I/O          !
!operations.                                                                                                           !
!**********************************************************************************************************************!
module GenericInputOutput

  use Parameters, only: sl
  implicit none

  public

  !******************************************************************************************************************!
  !                                     GENERICTEXTFILE (GTF) OBJECT DEFINITION                                      !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'ksiz' is the status of allocatable arrays.                                                                       !
  !'numl' is the number of lines of text in file.                                                                    !
  !'line' is an allocatable array containing file contents.                                                          !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'Resize' resizes all allocatable arrays.                                                                          !
  !'Erase' release memory from all allocatable arrays.                                                               !
  !'Duplicate' creates a complete copy of the object.                                                                !
  !'GetHead' extracts specified number of lines from the beginning of file.                                          !
  !'GetTail' extracts specified number of lines from the end of file.                                                !
  !'GetSection' extracts a specified section from anywhere in the file.                                              !
  !'InsertFile' inserts a complete file at specified location.                                                       !
  !'ReadFromDisc' reads data from disc storage into memory (overloaded).                                             !
  !'WriteToDisc' writes the object's contents from memory onto disc storage (overloaded).                            !
  !******************************************************************************************************************!
  type GenericTextFile
      logical,                        public :: ksiz = .false.
      integer,                        public :: numl = 0
      character(len=sl), allocatable, public :: line(:)
    contains
      procedure, public :: Resize       => Resize_gtf
      procedure, public :: Erase        => Erase_gtf
      procedure, public :: Duplicate    => Duplicate_gtfX2
      procedure, public :: GetHead      => GetHead_gtfX2
      procedure, public :: GetTail      => GetTail_gtfX2
      procedure, public :: GetSection   => GetSection_gtfX2
      procedure, public :: InsertFile   => InsertFile_igtfX2
      generic,   public :: ReadFromDisc => ReadFromDisc_iX2gtf, ReadFromDisc_iX2sgtf, ReadFromDisc_sgtf
      generic,   public :: WriteToDisc  => WriteToDisc_igtf, WriteToDisc_iX2sX2gtf, WriteToDisc_iX3gtf,  &
                                           WriteToDisc_sX2gtf
      procedure, private :: ReadFromDisc_iX2gtf
      procedure, private :: ReadFromDisc_iX2sgtf
      procedure, private :: ReadFromDisc_sgtf
      procedure, private :: WriteToDisc_igtf
      procedure, private :: WriteToDisc_iX2sX2gtf
      procedure, private :: WriteToDisc_iX3gtf
      procedure, private :: WriteToDisc_sX2gtf
  end type GenericTextFile

  !******************************************************************************************************************!
  !                                               CONTAINED PROCEDURES                                               !
  !                                                                                                                  !
  !'CountNumberOfLines' returns the number of lines in a file.                                                       !
  !******************************************************************************************************************!

  interface CountNumberOfLines
    module procedure CountNumberOfLines_s_i
  end interface CountNumberOfLines

  contains

  !******************************************************************************************************************!
  !                                            GENERICTEXTFILE PROCEDURES                                            !
  !******************************************************************************************************************!
  subroutine Resize_gtf (self)
    use Debugger,         only: PrintError, procname, ltrace, PrintTrace
    use MemoryManagement, only: ResizeArray
    implicit none

    class(GenericTextFile), intent(inout) :: self

    procname = 'GenericInputOutput::Resize_gtf'
    if (ltrace) call PrintTrace()

    self%ksiz = (self%numl==size(self%line))
    if (.not. self%ksiz) then
      call ResizeArray (nume=self%numl, ida=self%line)
      self%ksiz = .true.
    end if

  end subroutine Resize_gtf

  subroutine Erase_gtf (self)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(GenericTextFile), intent(inout) :: self

    procname = 'GenericInputOutput::Erase_gtf'
    if (ltrace) call PrintTrace()

    self%numl = 0
    call self%Resize()
    self%ksiz = .false.

  end subroutine Erase_gtf

  subroutine Duplicate_gtfX2 (self, dupl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(GenericTextFile), intent(in)    :: self
    type(GenericTextFile),  intent(inout) :: dupl

    procname = 'GenericInputOutput::Duplicate_gtfX2'
    if (ltrace) call PrintTrace()

    if ((.not. self%ksiz) .or. (self%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    dupl%numl = self%numl
    call dupl%Resize()
    dupl%line = self%line

  end subroutine Duplicate_gtfX2

  subroutine GetHead_gtfX2 (self, head, numl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(GenericTextFile), intent(in)    :: self
    type(GenericTextFile),  intent(inout) :: head
    integer,                intent(in)    :: numl     !How many lines (from beginning of file)?

    procname = 'GenericInputOutput::GetHead_gtfX2'
    if (ltrace) call PrintTrace()

    !At least 1 line must be extracted.
    if ((.not. self%ksiz) .or. (self%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    if ((numl<1) .or. (numl>self%numl)) call PrintError (ekey=2132, lstop=.true.)
    head%numl = numl
    call head%Resize()
    head%line = self%line(1:numl)

  end subroutine GetHead_gtfX2

  subroutine GetTail_gtfX2 (self, tail, numl)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(GenericTextFile), intent(in)    :: self
    type(GenericTextFile),  intent(inout) :: tail
    integer,                intent(in)    :: numl     !How many lines (from end of file)?

    procname = 'GenericInputOutput::GetTail_gtfX2'
    if (ltrace) call PrintTrace()

    !At least 1 line must be extracted.
    if ((.not. self%ksiz) .or. (self%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    if ((numl<1) .or. (numl>self%numl)) call PrintError (ekey=2132, lstop=.true.)
    tail%numl = numl
    call tail%Resize()
    tail%line = self%line(self%numl-numl+1:self%numl)

  end subroutine GetTail_gtfX2

  subroutine GetSection_gtfX2 (self, esec, numli, numlf)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(GenericTextFile), intent(in)    :: self
    type(GenericTextFile),  intent(inout) :: esec
    integer,                intent(in)    :: numli, numlf     !Which lines (from/to)?

    procname = 'GenericInputOutput::GetSection_gtfX2'
    if (ltrace) call PrintTrace()

    !At least 1 line must be extracted.
    if ((.not. self%ksiz) .or. (self%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    if ((numli<1) .or. (numli>numlf) .or. (numlf>self%numl)) call PrintError (ekey=2132, lstop=.true.)
    esec%numl = numlf - numli + 1
    call esec%Resize()
    esec%line = self%line(numli:numlf)

  end subroutine GetSection_gtfX2

  subroutine InsertFile_igtfX2 (self, file, loc)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(GenericTextFile), intent(inout) :: self     !Base object.
    type(GenericTextFile),  intent(in)    :: file     !Source contents to be inserted.
    integer, optional,      intent(in)    :: loc      !After which line?

    type(GenericTextFile) :: selfd     !Temporary copy of base object.
    integer               :: iloc

    procname = 'GenericInputOutput::InsertFile_igtfX2'
    if (ltrace) call PrintTrace()

    !Get location in base file to insert contents. If no location is specified, default is to insert at the end
    !of base file.
    if ((.not. self%ksiz) .or. (self%numl<1) .or.  &
        (.not. file%ksiz) .or. (file%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    if (present(loc)) then
      if ((loc<0) .or. (loc>self%numl)) call PrintError (ekey=2132, lstop=.true.)
      iloc = loc
    else
      iloc = self%numl
    end if

    !Create a temporary copy of base object. Calculate new size of file object and resize.
    call self%Duplicate (dupl=selfd)
    self%numl = self%numl + file%numl
    call self%Resize()

    !Copy back all sections at appropriate locations.
    !0 is a special case for beginning of base file.
    if (iloc==0) then
      self%line(1:file%numl) = file%line(1:file%numl)
      self%line(file%numl+1:self%numl) = selfd%line(1:selfd%numl)

    !Insert at the end of base file.
    else if (iloc==self%numl) then
      self%line(1:selfd%numl) = selfd%line(1:selfd%numl)
      self%line(selfd%numl+1:self%numl) = file%line(1:file%numl)

    !Insert anywhere between beginning and end of base file.
    else
      self%line(1:iloc) = selfd%line(1:iloc)
      self%line(iloc+1:iloc+file%numl) = file%line(1:file%numl)
      self%line(iloc+file%numl+1:self%numl) = selfd%line(iloc+1:selfd%numl)
    end if

    !Clean up memory.
    call selfd%Erase()

  end subroutine InsertFile_igtfX2

  subroutine ReadFromDisc_iX2gtf (self, unit, numl)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: F001
    use UtilityProcedures, only: FileIsOpen
    implicit none

    class(GenericTextFile), intent(inout) :: self
    integer,                intent(in)    :: unit
    integer,                intent(in)    :: numl     !Number of lines to read.

    integer :: ii, code

    procname = 'GenericInputOutput::ReadFromDisc_iX2gtf'
    if (ltrace) call PrintTrace()

    !At least 1 line must be read.
    if (numl<1) call PrintError (ekey=2132, lstop=.true.)
    if (.not. FileIsOpen(unit=unit)) call PrintError (ekey=1202, lstop=.true.)

    !Resize file object and read specified number of lines.
    self%numl = numl
    call self%Resize()
    do ii = 1, self%numl
      read (unit,F001,iostat=code) self%line(ii)
        if (code>0) call PrintError (ekey=2111, lstop=.true.)
    end do

  end subroutine ReadFromDisc_iX2gtf

  subroutine ReadFromDisc_iX2sgtf (self, file, numli, numlf)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: sl, F001
    use UtilityProcedures, only: AvailableLogicalUnit
    implicit none

    class(GenericTextFile), intent(inout) :: self
    character(len=*),       intent(in)    :: file
    integer,                intent(in)    :: numli, numlf     !Which lines (from/to)?

    character(len=sl) :: buff
    integer           :: unit, ii, code

    procname = 'GenericInputOutput::ReadFromDisc_iX2sgtf'
    if (ltrace) call PrintTrace()

    !Calculate the required size of file object and resize.
    if (len_trim(file)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: file')
    if ((numli<1) .or. (numli>numlf)) call PrintError (ekey=2132, lstop=.true., msg1='File: '//file)
    self%numl = CountNumberOfLines (file=file)
    if (numlf>self%numl) call PrintError (ekey=2132, lstop=.true., msg1='File: '//file)
    self%numl = numlf - numli + 1
    call self%Resize()

    !Open the file and skip until the specified section is reached.
    unit = AvailableLogicalUnit (guess=11)
    open (unit=unit, file=trim(adjustl(file)), status='OLD', action='READ', position='REWIND', iostat=code)
      if (code>0) call PrintError (ekey=2101, lstop=.true., msg1='File: '//file)
    do ii = 1, numli-1
      read (unit,F001,iostat=code) buff
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='File: '//file)
    end do

    !Read the specified section.
    do ii = 1, self%numl
      read (unit,F001,iostat=code) self%line(ii)
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='File: '//file)
    end do

    close (unit=unit)

  end subroutine ReadFromDisc_iX2sgtf

  subroutine ReadFromDisc_sgtf (self, file)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: F001
    use UtilityProcedures, only: AvailableLogicalUnit
    implicit none

    class(GenericTextFile), intent(inout) :: self
    character(len=*),       intent(in)    :: file

    integer :: unit, ii, code

    procname = 'GenericInputOutput::ReadFromDisc_sgtf'
    if (ltrace) call PrintTrace()

    !Calculate the required size of file object and resize.
    if (len_trim(file)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: file')
    self%numl = CountNumberOfLines (file=file)
    if (self%numl<1) call PrintError (ekey=2131, lstop=.true., msg1='File: '//file)
    call self%Resize()

    !Open the file and read through end.
    unit = AvailableLogicalUnit (guess=12)
    open (unit=unit, file=trim(adjustl(file)), status='OLD', action='READ', position='REWIND', iostat=code)
      if (code>0) call PrintError (ekey=2101, lstop=.true., msg1='File: '//file)
    do ii = 1, self%numl
      read (unit,F001,iostat=code) self%line(ii)
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='File: '//file)
    end do

    close (unit=unit)

  end subroutine ReadFromDisc_sgtf

  subroutine WriteToDisc_igtf (self, unit)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: F001
    use UtilityProcedures, only: FileIsOpen
    implicit none

    class(GenericTextFile), intent(in) :: self
    integer,                intent(in) :: unit

    integer :: ii, code

    procname = 'GenericInputOutput::WriteToDisc_igtf'
    if (ltrace) call PrintTrace()

    !At least 1 line must be written.
    if ((.not. self%ksiz) .or. (self%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    if (.not. FileIsOpen(unit=unit)) call PrintError (ekey=1202, lstop=.true.)

    !Write whole contents of the file object.
    do ii = 1, self%numl
      write (unit,F001,iostat=code) trim(self%line(ii))
        if (code>0) call PrintError (ekey=2121, lstop=.true.)
    end do

  end subroutine WriteToDisc_igtf

  subroutine WriteToDisc_iX2sX2gtf (self, file, numli, numlf, mode)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: sl, F001
    use UtilityProcedures, only: AvailableLogicalUnit, ConvertToUpperCase, FileExists
    implicit none

    class(GenericTextFile),     intent(in) :: self
    character(len=*),           intent(in) :: file
    integer,                    intent(in) :: numli, numlf     !Which lines (from/to)?
    character(len=*), optional, intent(in) :: mode             !Action (append/replace)?

    character(len=sl) :: action = 'REPLACE'     !Default action is 'REPLACE'.
    integer           :: unit, ii, code

    procname = 'GenericInputOutput::WriteToDisc_iX2sX2gtf'
    if (ltrace) call PrintTrace()

    !Override default action?
    if (len_trim(file)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: file')
    if (present(mode)) then
      if (len(action)<len(mode)) call PrintError (ekey=1122, lstop=.true.)
      action = adjustl(mode)
      call ConvertToUpperCase (line=action)
      if ((action/='APPEND') .and. (action/='REPLACE'))  &
        call PrintError (ekey=1302, lstop=.true., msg1='Keyword: mode')
    end if

    !At least 1 line must be written.
    if ((.not. self%ksiz) .or. (self%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    if ((numli<1) .or. (numli>numlf) .or. (numlf>self%numl)) call PrintError (ekey=2132, lstop=.true.)

    !Open the file based on requested action.
    unit = AvailableLogicalUnit (guess=21)
    if (action=='APPEND') then
      open (unit=unit, file=trim(adjustl(file)), status='UNKNOWN', action='WRITE', position='APPEND', iostat=code)
    else if (action=='REPLACE') then
      open (unit=unit, file=trim(adjustl(file)), status='REPLACE', action='WRITE', position='REWIND', iostat=code)
    end if
    if (code>0) call PrintError (ekey=2101, lstop=.true., msg1='File: '//file)

    !Write specified contents of the file object.
    do ii = numli, numlf
      write (unit,F001,iostat=code) trim(self%line(ii))
        if (code>0) call PrintError (ekey=2121, lstop=.true., msg1='File: '//file)
    end do

    close (unit=unit)

  end subroutine WriteToDisc_iX2sX2gtf

  subroutine WriteToDisc_iX3gtf (self, unit, numli, numlf)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: F001
    use UtilityProcedures, only: FileIsOpen
    implicit none

    class(GenericTextFile), intent(in) :: self
    integer,                intent(in) :: unit
    integer,                intent(in) :: numli, numlf     !Which lines (from/to)?

    integer :: ii, code

    procname = 'GenericInputOutput::WriteToDisc_iX3gtf'
    if (ltrace) call PrintTrace()

    !At least 1 line must be written.
    if ((.not. self%ksiz) .or. (self%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    if ((numli<1) .or. (numli>numlf) .or. (numlf>self%numl)) call PrintError (ekey=2132, lstop=.true.)
    if (.not. FileIsOpen(unit=unit)) call PrintError (ekey=1202, lstop=.true.)

    !Write specified contents of the file object.
    do ii = numli, numlf
      write (unit,F001,iostat=code) trim(self%line(ii))
        if (code>0) call PrintError (ekey=2121, lstop=.true.)
    end do

  end subroutine WriteToDisc_iX3gtf

  subroutine WriteToDisc_sX2gtf (self, file, mode)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: sl, F001
    use UtilityProcedures, only: AvailableLogicalUnit, ConvertToUpperCase, FileExists
    implicit none

    class(GenericTextFile),     intent(in) :: self
    character(len=*),           intent(in) :: file
    character(len=*), optional, intent(in) :: mode     !Action (append/replace)?

    character(len=sl) :: action = 'REPLACE'     !Default action is 'REPLACE'.
    integer           :: unit, ii, code

    procname = 'GenericInputOutput::WriteToDisc_sX2gtf'
    if (ltrace) call PrintTrace()

    !Override default action?
    if (len_trim(file)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: file')
    if (present(mode)) then
      if (len(action)<len(mode)) call PrintError (ekey=1122, lstop=.true.)
      action = adjustl(mode)
      call ConvertToUpperCase (line=action)
      if ((action/='APPEND') .and. (action/='REPLACE'))  &
        call PrintError (ekey=1302, lstop=.true., msg1='Keyword: mode')
    end if

    !At least 1 line must be written.
    if ((.not. self%ksiz) .or. (self%numl<1)) call PrintError (ekey=1103, lstop=.true.)

    !Open the file based on requested action.
    unit = AvailableLogicalUnit (guess=22)
    if (action=='APPEND') then
      open (unit=unit, file=trim(adjustl(file)), status='UNKNOWN', action='WRITE', position='APPEND', iostat=code)
    else if (action=='REPLACE') then
      open (unit=unit, file=trim(adjustl(file)), status='REPLACE', action='WRITE', position='REWIND', iostat=code)
    end if
    if (code>0) call PrintError (ekey=2101, lstop=.true., msg1='File: '//file)

    !Write whole contents of the file object.
    do ii = 1, self%numl
      write (unit,F001,iostat=code) trim(self%line(ii))
        if (code>0) call PrintError (ekey=2121, lstop=.true., msg1='File: '//file)
    end do

    close (unit=unit)

  end subroutine WriteToDisc_sX2gtf
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                           INTERFACE COUNTNUMBEROFLINES                                           !
  !******************************************************************************************************************!
  function CountNumberOfLines_s_i (file) result (numl)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: sl, F001
    use UtilityProcedures, only: AvailableLogicalUnit, FileExists
    implicit none

    character(len=*), intent(in) :: file
    integer                      :: numl

    character(len=sl) :: buff
    integer           :: unit, code

    procname = 'GenericInputOutput::CountNumberOfLines_s_i'
    if (ltrace) call PrintTrace()

    !Ensure that the specified file exists. Then open it.
    if (len_trim(file)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: file')
    if (.not. FileExists(file=file)) call PrintError (ekey=2002, lstop=.true., msg1='File: '//file)
    unit = AvailableLogicalUnit (guess=31)
    open (unit=unit, file=trim(adjustl(file)), status='OLD', action='READ', position='REWIND', iostat=code)
      if (code>0) call PrintError (ekey=2101, lstop=.true., msg1='File: '//file)

    !Read the whole file while counting number of lines.
    numl = 0
    do while (.true.)
      read (unit,F001,end=100,iostat=code) buff
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='File: '//file)
      numl = numl + 1
    end do
    100 continue

    close (unit=unit)

  end function CountNumberOfLines_s_i
  !******************************************************************************************************************!

end module GenericInputOutput
!**********************************************************************************************************************!
