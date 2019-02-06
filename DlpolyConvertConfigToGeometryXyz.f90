!**********************************************************************************************************************!
!Main program file for converting a file from DLPOLY CONFIG format to XYZ format.                                      !
!**********************************************************************************************************************!
program DlpolyConvertConfigToGeometryXyz

  use Parameters,            only: pl, luout
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: ConfigData, ConfigHeader, XyzData, XyzHeader, PeriodicBoxIsAligned
  implicit none

  character(len=pl) :: path
  integer           :: kfrm
  logical           :: lhead, lpbc

  type(GenericTextFile) :: file0
  type(GenericTextFile) :: filed, fileh
  type(ConfigData)      :: datam
  type(ConfigHeader)    :: headm
  type(XyzData)         :: datax
  type(XyzHeader)       :: headx

  procname = 'DlpolyConvertConfigToGeometryXyz::DlpolyConvertConfigToGeometryXyz'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (file=path, kfrm=kfrm, lhead=lhead, lpbc=lpbc)

  !Read input file.
  call file0%ReadFromDisc (file=path)
  if (lhead) then
    call file0%GetHead (head=fileh, numl=5)
    call headm%ReadFromFile (file=fileh)
    call file0%GetSection (esec=filed, numli=5+1, numlf=5+headm%numa*(headm%kfrm+2))
    call datam%ReadFromFile (file=filed, kfrm=headm%kfrm)
  else
    call file0%GetSection (esec=filed, numli=1, numlf=file0%numl)
    call datam%ReadFromFile (file=filed, kfrm=kfrm)
  end if

  !Relocate coordinates to positive quadrants.
  if (lpbc) then
    if ((headm%kpbc/=1) .and. (headm%kpbc/=2))  &
      call PrintError (ekey=1304, lstop=.true., msg1='CONFIG: periodic-box type')
    if (.not. PeriodicBoxIsAligned(box=headm%box)) call PrintError (ekey=3031, lstop=.true.)
    call datam%ApplyPeriodicBoundary (dx=headm%box(1,1), dy=headm%box(2,2), dz=headm%box(3,3))
  end if

  !Extract header section in specified format.
  if (lhead) then
    headx%name = headm%name
    headx%numa = headm%numa
  end if

  !Extract data section in specified format.
  datax%numa = datam%numa
  call datax%Resize()
  datax%type = datam%type
  datax%cx   = datam%cx
  datax%cy   = datam%cy
  datax%cz   = datam%cz

  !Write output file.
  if (lhead) then
    call headx%WriteToFile (file=fileh)
    call fileh%WriteToDisc (unit=luout)
  end if
  call datax%WriteToFile (file=filed)
  call filed%WriteToDisc (unit=luout)

  !Clean up memory.
  call file0%Erase()
  call filed%Erase()
  call fileh%Erase()
  call datam%Erase()
  call datax%Erase()

  contains

  !******************************************************************************************************************!
  !                                        PRIVATE PROCEDURE PARSECOMMANDLINE                                        !
  !******************************************************************************************************************!
  subroutine ParseCommandLine (file, kfrm, lhead, lpbc)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: pl, F001
    implicit none

    character(len=*), intent(inout) :: file
    integer,          intent(inout) :: kfrm
    logical,          intent(inout) :: lhead, lpbc

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'DlpolyConvertConfigToGeometryXyz::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    file  = 'CONFIG'
    kfrm  = -1
    lhead = .true.
    lpbc  = .false.
    lhelp = .false.

    !Read command-line arguments and update corresponding variables.
    nn = IARGC()
    ekey = 0
    do ii = 1, nn
      call GETARG (ii, buff)
      buff = adjustl(buff)

      if ((buff=='-f') .or. (buff=='--file')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) file
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-h') .or. (buff=='--help')) then
        lhelp = .true.

      else if ((buff=='-k') .or. (buff=='--format-key')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,*,iostat=code) kfrm
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-p') .or. (buff=='--apply-periodic-boundary')) then
        lpbc = .false.

      else if ((buff=='-s') .or. (buff=='--skip-header')) then
        lhead = .false.
      end if
    end do

    !Check command-line arguments for conflicts.
    if (lhelp) call PrintHelp()
    if (ekey>0) call PrintHelp (ekey=ekey)
    if (.not. lhead) then
      if (lpbc) call PrintHelp (ekey=9004)
      if (kfrm==-1) then
        call PrintHelp (ekey=9001)
      else if ((kfrm/=0) .and. (kfrm/=1) .and. (kfrm/=2)) then
        call PrintHelp (ekey=9003)
      end if
    end if

  end subroutine ParseCommandLine
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                           PRIVATE PROCEDURE PRINTHELP                                            !
  !******************************************************************************************************************!
  subroutine PrintHelp (ekey)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: luerr, F001
    Implicit none

    integer, optional, intent(in) :: ekey

    character(len=80) :: msgs(1:100)
    integer           :: ii, nn

    procname = 'DlpolyConvertConfigToGeometryXyz::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********       PROGRAM: DLPOLY-CONVERT-CONFIG-TO-GEOMETRY-XYZ       **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program converts a DLPOLY CONFIG file to XYZ format.                       '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: dlpoly-convert-config-to-geometry-xyz -fhkps                             '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -f     --file                        Input CONFIG file.                    '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/CONFIG                                          '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                        Print this help.                      '
    nn = nn + 1 ; msgs(nn) = '     -k     --format-key                  CONFIG file-format key. Required with '
    nn = nn + 1 ; msgs(nn) = '                                          -s. Ignored (without warning) if -s is'
    nn = nn + 1 ; msgs(nn) = '                                          not specified.                        '
    nn = nn + 1 ; msgs(nn) = '                 = 0                      coordinates only.                     '
    nn = nn + 1 ; msgs(nn) = '                 = 1                      coordinates and velocities.           '
    nn = nn + 1 ; msgs(nn) = '                 = 2                      coordinates, velocities, and forces.  '
    nn = nn + 1 ; msgs(nn) = '     -p     --apply-periodic-boundary     Relocate all coordinates to positive  '
    nn = nn + 1 ; msgs(nn) = '                                          quadrants. Cannot be combined with -s.'
    nn = nn + 1 ; msgs(nn) = '     -s     --skip-header                 Input file has no header. Format key  '
    nn = nn + 1 ; msgs(nn) = '                                          (-k) must be manually specified.      '
    nn = nn + 1 ; msgs(nn) = '                                          Cannot be combined with -p.           '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program DlpolyConvertConfigToGeometryXyz
!**********************************************************************************************************************!
