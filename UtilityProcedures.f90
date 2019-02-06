!**********************************************************************************************************************!
!This module provides general utility procedures.                                                                      !
!**********************************************************************************************************************!
module UtilityProcedures

  implicit none

  public

  !******************************************************************************************************************!
  !                                               CONTAINED PROCEDURES                                               !
  !                                                                                                                  !
  !'DirectoryExists' returns true if the specified directory is successfully located.                                !
  !'FileExists' returns true if the specified file is successfully located.                                          !
  !'FileIsOpen' returns true if the specified file is currently open (overloaded).                                   !
  !'AvailableLogicalUnit' returns the index for a logical unit that is currently not connected.                      !
  !'IsAlphabet' returns true if the specified character is an alphabet.                                              !
  !'IsDigit' returns true if the specified character is a numeral.                                                   !
  !'ConvertToLowerCase' converts all uppercase letters in a string to lower case.                                    !
  !'ConvertToUpperCase' converts all lowercase letters in a string to upper case.                                    !
  !'AppendCharacter' appends a specified character at the end of a string.                                           !
  !'ExtractNumbersFromSting' parses a string into an array of numbers.                                               !
  !******************************************************************************************************************!

  interface DirectoryExists
    module procedure DirectoryExists_s_l
  end interface DirectoryExists

  interface FileExists
    module procedure FileExists_s_l
  end interface FileExists

  interface FileIsOpen
    module procedure FileIsOpen_i_l
    module procedure FileIsOpen_s_l
  end interface FileIsOpen

  interface AvailableLogicalUnit
    module procedure AvailableLogicalUnit_i_i
  end interface AvailableLogicalUnit

  interface IsAlphabet
    module procedure IsAlphabet_c_l
  end interface IsAlphabet

  interface IsDigit
    module procedure IsDigit_c_l
  end interface IsDigit

  interface ConvertToLowerCase
    module procedure ConvertToLowerCase_s
  end interface ConvertToLowerCase

  interface ConvertToUpperCase
    module procedure ConvertToUpperCase_s
  end interface ConvertToUpperCase

  interface AppendCharacter
    module procedure AppendCharacter_scl
  end interface AppendCharacter

  interface ExtractNumbersFromString
    module procedure ExtractNumbersFromString_sia
  end interface ExtractNumbersFromString

  contains

  !******************************************************************************************************************!
  !                                            INTERFACE DIRECTORYEXISTS                                             !
  !******************************************************************************************************************!
  function DirectoryExists_s_l (dir) result (lexist)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    character(len=*), intent(in) :: dir
    logical                      :: lexist

    procname = 'UtilityProcedures::DirectoryExists_s_l'
    if (ltrace) call PrintTrace()

    if (len_trim(dir)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: dir')
    inquire (directory=trim(adjustl(dir)), exist=lexist)

  end function DirectoryExists_s_l
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                               INTERFACE FILEEXISTS                                               !
  !******************************************************************************************************************!
  function FileExists_s_l (file) result (lexist)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    character(len=*), intent(in) :: file
    logical                      :: lexist

    procname = 'UtilityProcedures::FileExists_s_l'
    if (ltrace) call PrintTrace()

    if (len_trim(file)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: file')
    inquire (file=trim(adjustl(file)), exist=lexist)

  end function FileExists_s_l
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                               INTERFACE FILEISOPEN                                               !
  !******************************************************************************************************************!
  function FileIsOpen_i_l (unit) result (lopen)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    integer, intent(in) :: unit
    logical             :: lopen

    procname = 'UtilityProcedures::FileIsOpen_i_l'
    if (ltrace) call PrintTrace()

    inquire (unit=unit, opened=lopen)

  end function FileIsOpen_i_l

  function FileIsOpen_s_l (file) result (lopen)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    character(len=*), intent(in) :: file
    logical                      :: lopen

    procname = 'UtilityProcedures::FileIsOpen_s_l'
    if (ltrace) call PrintTrace()

    if (len_trim(file)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: file')
    inquire (file=trim(adjustl(file)), opened=lopen)

  end function FileIsOpen_s_l
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                          INTERFACE AVAILABLELOGICALUNIT                                          !
  !******************************************************************************************************************!
  function AvailableLogicalUnit_i_i (guess) result (unit)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    integer, optional, intent(in) :: guess
    integer                       :: unit

    integer :: ii
    logical :: lfree = .false.

    procname = 'UtilityProcedures::AvailableLogicalUnit_i_i'
    if (ltrace) call PrintTrace()

    !Logical units are permitted in the range 11-90. Default is 11.
    unit = 11
    if (present(guess)) then
      if ((guess>=11) .and. (guess<=90)) unit = guess
    end if

    !Find the next logical unit that is currently not connected.
    do ii = unit, 90
      if (.not. FileIsOpen(unit=ii)) then
        unit = ii
        lfree = .true.
        exit
      end if
    end do

    if (.not. lfree) call PrintError (ekey=1201, lstop=.true.)

  end function AvailableLogicalUnit_i_i
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                               INTERFACE ISALPHABET                                               !
  !******************************************************************************************************************!
  function IsAlphabet_c_l (sym) result (ltype)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    character(len=1), intent(in) :: sym       !Ignore beyond first character.
    logical                      :: ltype

    procname = 'UtilityProcedures::IsAlphabet_c_l'
    if (ltrace) call PrintTrace()

    select case (sym)
      case ('a':'z', 'A':'Z')
        ltype = .true.
      case default
        ltype = .false.
    end select

  end function IsAlphabet_c_l
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                                INTERFACE ISDIGIT                                                 !
  !******************************************************************************************************************!
  function IsDigit_c_l (sym) result (ltype)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    character(len=1), intent(in) :: sym       !Ignore beyond first character.
    logical                      :: ltype

    procname = 'UtilityProcedures::IsDigit_c_l'
    if (ltrace) call PrintTrace()

    select case (sym)
      case ('0':'9')
        ltype = .true.
      case default
        ltype = .false.
    end select

  end function IsDigit_c_l
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                           INTERFACE CONVERTTOLOWERCASE                                           !
  !******************************************************************************************************************!
  subroutine ConvertToLowerCase_s (line)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    character(len=*), intent(inout) :: line

    integer :: jj, kk, offset

    procname = 'UtilityProcedures::ConvertToLowerCase_s'
    if (ltrace) call PrintTrace()

    !Get the offset between ASCII codes of characters 'a' and 'A'. Then shift all uppercase letters to lower case.
    offset = iachar('a') - iachar('A')
    kk = len_trim(line)
    do jj = 1, kk
      if ((LGE(line(jj:jj),'A')) .and. (LLE(line(jj:jj),'Z'))) line(jj:jj) = achar(iachar(line(jj:jj))+offset)
    end do

  end subroutine ConvertToLowerCase_s
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                           INTERFACE CONVERTTOUPPERCASE                                           !
  !******************************************************************************************************************!
  subroutine ConvertToUpperCase_s (line)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    character(len=*), intent(inout) :: line

    integer :: jj, kk, offset

    procname = 'UtilityProcedures::ConvertToUpperCase_s'
    if (ltrace) call PrintTrace()

    !Get the offset between ASCII codes of characters 'a' and 'A'. Then shift all lowercase letters to upper case.
    offset = iachar('a') - iachar('A')
    kk = len_trim(line)
    do jj = 1, kk
      if ((LGE(line(jj:jj),'a')) .and. (LLE(line(jj:jj),'z'))) line(jj:jj) = achar(iachar(line(jj:jj))-offset)
    end do

  end subroutine ConvertToUpperCase_s
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                            INTERFACE APPENDCHARACTER                                             !
  !******************************************************************************************************************!
  subroutine AppendCharacter_scl (line, sym, lskip)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    character(len=*),  intent(inout) :: line      !Base string.
    character(len=1),  intent(in)    :: sym       !What to append? Ignore beyond first character.
    logical, optional, intent(in)    :: lskip     !Skip action if the last character matches?

    integer :: kk

    procname = 'UtilityProcedures::AppendCharacter_scl'
    if (ltrace) call PrintTrace()

    !Skip everything if last character matches and should not be appended.
    kk = len_trim(line)
    if (present(lskip)) then
      if ((line(kk:kk)==sym) .and. lskip) return
    end if

    !For all other cases, character should be appended.
    kk = kk + 1
    if (kk>len(line)) call PrintError (ekey=1123, lstop=.true.)
    line(kk:kk) = sym

  end subroutine AppendCharacter_scl
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                        INTERFACE EXTRACTNUMBERSFROMSTRING                                        !
  !******************************************************************************************************************!
  subroutine ExtractNumbersFromString_sia (line, list)
    use Debugger,         only: PrintError, procname, ltrace, PrintTrace
    use Parameters,       only: pl
    use MemoryManagement, only: ResizeArray
    implicit none

    character(len=*),     intent(in)    :: line
    integer, allocatable, intent(inout) :: list(:)

    character(len=pl)    :: buff, tbuff
    integer              :: numc, numd, nume, numr
    integer              :: ii, jj, kk, nn, code
    logical              :: lneg
    integer, allocatable :: tlist(:)
    logical, allocatable :: lrange(:)

    procname = 'UtilityProcedures::ExtractNumbersFromString_sia'
    if (ltrace) call PrintTrace()

    !0-9, ',', and '-' are the only permitted characters.
    !A comma indicates next group of numbers. 2 commas cannot appear together.
    !A dash cannot be followed by a comma. 3 dashes cannot appear together.
    !First character may be a number or a dash. Last character must be a number.
    if (len(buff)<len(line)) call PrintError (ekey=1122, lstop=.true.)
    buff = adjustl(line)
    kk   = len_trim(buff)
    if (kk<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: line')
    if ((buff(1:1)==',') .or. (buff(kk:kk)==','))  &
      call PrintError (ekey=1124, lstop=.true., msg1='Character: ","')
    if (buff(kk:kk)=='-') call PrintError (ekey=1124, lstop=.true., msg1='Character: "-"')
    numc = 0
    numd = 0
    numr = 0
    do jj = 1, kk
      select case (buff(jj:jj))
        case ('0':'9')
          numc = 0
          numd = 0
        case (',')
          if ((numc>0) .or. (numd>0)) call PrintError (ekey=1124, lstop=.true., msg1='Character: ","')
          numc = 1
          numr = numr + 1
        case ('-')
          if (numd>=2) call PrintError (ekey=1124, lstop=.true., msg1='Character: "-"')
          numd = numd + 1
        case default
          call PrintError (ekey=1124, lstop=.true., msg1='Character: "'//buff(jj:jj)//'"')
      end select
    end do

    !Resize temporary lists assuming all groups are ranges.
    numr = (numr+1) * 2
    call ResizeArray (nume=numr, ida=tlist)
    call ResizeArray (nume=numr, ida=lrange)

    !Parse the string into numbers and ranges of numbers.
    lrange = .false.
    nn     = 0
    do while (.true.)
      lneg = .false.
      buff = adjustl(buff)
      kk   = len_trim(buff)

      !',' indicates next group.
      jj = 0
      jj = index (buff(1:kk), ',', .false.)
      if (jj>0) then
        tbuff = buff(1:jj-1)
        buff(1:jj) = ' '

        !'-' at first position is a negative number. Otherwise, it indicates a range of numbers.
        kk = len_trim(tbuff)
        jj = 0
        if (tbuff(1:1)=='-') then
          lneg = .true.
          jj   = index (tbuff(2:kk), '-', .false.)
        else
          jj = index (tbuff(1:kk), '-', .false.)
        end if

        !'-' found? Replace with space, read the range limits, and set appropriate flag.
        if (jj>0) then
          if (lneg) jj = jj + 1
          tbuff(jj:jj) = ' '
          read (tbuff,*,iostat=code) tlist(nn+1), tlist(nn+2)
            if (code>0) call PrintError (ekey=2112, lstop=.true.)
          if (tlist(nn+2)<=tlist(nn+1)) call PrintError (ekey=5002, lstop=.true.)
          lrange(nn+1) = .true.
          nn = nn + 2

        !'-' not found? Read a single number.
        else
          read (tbuff,*,iostat=code) tlist(nn+1)
            if (code>0) call PrintError (ekey=2112, lstop=.true.)
          nn = nn + 1
        end if

      !',' not found? Read the last group.
      !'-' at first position is a negative number. Otherwise, it indicates a range of numbers.
      else
        jj = 0
        if (buff(1:1)=='-') then
          lneg = .true.
          jj   = index (buff(2:kk), '-', .false.)
        else
          jj = index (buff(1:kk), '-', .false.)
        end if

        !'-' found? Replace with space, read the range limits, and set appropriate flag.
        if (jj>0) then
          if (lneg) jj = jj + 1
          buff(jj:jj) = ' '
          read (buff,*,iostat=code) tlist(nn+1), tlist(nn+2)
            if (code>0) call PrintError (ekey=2112, lstop=.true.)
          if (tlist(nn+2)<=tlist(nn+1)) call PrintError (ekey=5002, lstop=.true.)
          lrange(nn+1) = .true.
          nn = nn + 2

        !'-' not found? Read a single number.
        else
          read (buff,*,iostat=code) tlist(nn+1)
            if (code>0) call PrintError (ekey=2112, lstop=.true.)
          nn = nn + 1
        end if

        exit
      end if
    end do

     !Calculate the size of array.
     if (nn<1) call PrintError (ekey=1102, lstop=.true.)
     nume = 0
     do ii = 1, nn
       if (lrange(ii)) then
         nume = nume + tlist(ii+1) - tlist(ii)
       else
         nume = nume + 1
       end if
     end do

     !Resize array and fill with numbers.
     call ResizeArray (nume=nume, ida=list)
     kk = 1
     do ii = 1, nn
       if (lrange(ii)) then
         do jj = tlist(ii), tlist(ii+1)-1
           list(kk) = jj
           kk = kk + 1
         end do
       else
         list(kk) = tlist(ii)
         kk = kk + 1
       end if
     end do

  end subroutine ExtractNumbersFromString_sia
  !******************************************************************************************************************!

end module UtilityProcedures
!**********************************************************************************************************************!
