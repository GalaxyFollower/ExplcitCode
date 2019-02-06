!**********************************************************************************************************************!
!Main program file for relocating DLPOLY CONFIG coordinates to positive quadrants.                                     !
!**********************************************************************************************************************!
program DlpolyRelocateConfigCoordinates

  use Parameters,            only: pl, luout
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: ConfigData, ConfigHeader, PeriodicBoxIsAligned
  implicit none

  character(len=pl) :: path

  type(GenericTextFile) :: file0
  type(GenericTextFile) :: filed, fileh
  type(ConfigData)      :: datam
  type(ConfigHeader)    :: headm

  procname = 'DlpolyRelocateConfigCoordinates::DlpolyRelocateConfigCoordinates'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (file=path)

  !Read input file.
  call file0%ReadFromDisc (file=path)
  call file0%GetHead (head=fileh, numl=5)
  call headm%ReadFromFile (file=fileh)
  call file0%GetSection (esec=filed, numli=5+1, numlf=5+headm%numa*(headm%kfrm+2))
  call datam%ReadFromFile (file=filed, kfrm=headm%kfrm)

  !Relocate coordinates to positive quadrants.
  if ((headm%kpbc/=1) .and. (headm%kpbc/=2))  &
    call PrintError (ekey=1304, lstop=.true., msg1='CONFIG: periodic-box type')
  if (.not. PeriodicBoxIsAligned(box=headm%box)) call PrintError (ekey=3031, lstop=.true.)
  call datam%ApplyPeriodicBoundary (dx=headm%box(1,1), dy=headm%box(2,2), dz=headm%box(3,3))

  !Write output file.
  call headm%WriteToFile (file=fileh)
  call fileh%WriteToDisc (unit=luout)
  call datam%WriteToFile (file=filed, kfrm=headm%kfrm)
  call filed%WriteToDisc (unit=luout)

  !Clean up memory.
  call file0%Erase()
  call filed%Erase()
  call fileh%Erase()
  call datam%Erase()

  contains

  !******************************************************************************************************************!
  !                                        PRIVATE PROCEDURE PARSECOMMANDLINE                                        !
  !******************************************************************************************************************!
  subroutine ParseCommandLine (file)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: pl, F001
    implicit none

    character(len=*), intent(inout) :: file

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'DlpolyRelocateConfigCoordinates::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    file  = 'CONFIG'
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
      end if
    end do

    !Check command-line arguments for conflicts.
    if (lhelp) call PrintHelp()
    if (ekey>0) call PrintHelp (ekey=ekey)

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

    procname = 'DlpolyRelocateConfigCoordinates::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********        PROGRAM: DLPOLY-RELOCATE-CONFIG-COORDINATES         **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program relocates DLPOLY CONFIG coordinates to positive quadrants.         '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: dlpoly-relocate-config-coordinates -fh                                   '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -f     --file     Input CONFIG file.                                       '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/CONFIG                                          '
    nn = nn + 1 ; msgs(nn) = '     -h     --help     Print this help.                                         '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program DlpolyRelocateConfigCoordinates
!**********************************************************************************************************************!
