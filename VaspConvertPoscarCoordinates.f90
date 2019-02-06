!**********************************************************************************************************************!
!Main program file for converting between Cartesian and fractional coordinates for VASP.                               !
!**********************************************************************************************************************!
program VaspConvertPoscarCoordinates

  use Parameters,            only: dp, pl, sl, one, luout
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use UtilityProcedures,     only: ConvertToUpperCase
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: PoscarData, PoscarHeader, ConvertCoordinates, PeriodicBoxIsAligned
  implicit none

  character(len=pl) :: path
  character(len=sl) :: mode
  logical           :: lpbc

  type(GenericTextFile) :: file0
  type(GenericTextFile) :: filed, fileh
  type(PoscarData)      :: datas
  type(PoscarHeader)    :: heads

  procname = 'VaspConvertPoscarCoordinates::VaspConvertPoscarCoordinates'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (file=path, mode=mode, lpbc=lpbc)

  !Read input file.
  call file0%ReadFromDisc (file=path)
  call file0%GetHead (head=fileh, numl=9)
  call heads%ReadFromFile (file=fileh)
  call file0%GetSection (esec=filed, numli=9+1, numlf=9+heads%numa)
  call datas%ReadFromFile (file=filed)

  !Convert coordinates only if not already in correct format. Relocate to positive quadrants as required.
  call ConvertToUpperCase (line=heads%ctyp)
  if (heads%ctyp=='CARTESIAN') then
    if ((mode=='C2F') .or. (mode=='CARTESIAN2FRACTIONAL') .or. (mode=='CARTESIAN-TO-FRACTIONAL')) then
      if (lpbc) then
        if (.not. PeriodicBoxIsAligned(box=heads%box)) call PrintError (ekey=3031, lstop=.true.)
        call datas%ApplyPeriodicBoundary (dx=heads%box(1,1), dy=heads%box(2,2), dz=heads%box(3,3))
      end if
      call ConvertCoordinates (mode=mode, box=heads%box, gms=datas)
      heads%ctyp = 'Direct'
    end if
  else if (heads%ctyp=='DIRECT') then
    if ((mode=='F2C') .or. (mode=='FRACTIONAL2CARTESIAN') .or. (mode=='FRACTIONAL-TO-CARTESIAN')) then
      if (lpbc) call datas%ApplyPeriodicBoundary (dx=one, dy=one, dz=one)
      call ConvertCoordinates (mode=mode, box=heads%box, gms=datas)
      heads%ctyp = 'Cartesian'
    end if
  else
    call PrintError (ekey=1304, lstop=.true., msg1='POSCAR: type of coordinates')
  end if

  !Write output file.
  call heads%WriteToFile (file=fileh)
  call fileh%WriteToDisc (unit=luout)
  call datas%WriteToFile (file=filed)
  call filed%WriteToDisc (unit=luout)

  !Clean up memory.
  call file0%Erase()
  call filed%Erase()
  call fileh%Erase()
  call datas%Erase()
  call heads%Erase()

  contains

  !******************************************************************************************************************!
  !                                        PRIVATE PROCEDURE PARSECOMMANDLINE                                        !
  !******************************************************************************************************************!
  subroutine ParseCommandLine (file, mode, lpbc)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: pl, F001
    use UtilityProcedures, only: ConvertToUpperCase
    implicit none

    character(len=*), intent(inout) :: file, mode
    logical,          intent(inout) :: lpbc

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'VaspConvertPoscarCoordinates::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    file  = 'POSCAR'
    mode  = ' '
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

      else if ((buff=='-m') .or. (buff=='--mode')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) mode
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-p') .or. (buff=='--apply-periodic-boundary')) then
        lpbc = .false.
      end if
    end do

    !Check command-line arguments for conflicts.
    if (lhelp) call PrintHelp()
    if (ekey>0) call PrintHelp (ekey=ekey)
    call ConvertToUpperCase (line=mode)
    if ((mode/='C2F') .and. (mode/='CARTESIAN2FRACTIONAL') .and. (mode/='CARTESIAN-TO-FRACTIONAL') .and.  &
        (mode/='F2C') .and. (mode/='FRACTIONAL2CARTESIAN') .and. (mode/='FRACTIONAL-TO-CARTESIAN'))       &
      call PrintHelp (ekey=9003)

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

    procname = 'VaspConvertPoscarCoordinates::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********          PROGRAM: VASP-CONVERT-POSCAR-COORDINATES          **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program converts between Cartesian and fractional coordinates for VASP.    '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: vasp-convert-poscar-coordinates -fhmp                                    '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -f     --file                        VASP POSCAR file.                     '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/POSCAR                                          '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                        Print this help.                      '
    nn = nn + 1 ; msgs(nn) = '     -m     --mode                        (required) Direction of coordinate    '
    nn = nn + 1 ; msgs(nn) = '                                          conversion.                           '
    nn = nn + 1 ; msgs(nn) = '                                          (C2F/CARTESIAN2FRACTIONAL/            '
    nn = nn + 1 ; msgs(nn) = '                                           CARTESIAN-TO-FRACTIONAL/             '
    nn = nn + 1 ; msgs(nn) = '                                           F2C/FRACTIONAL2CARTESIAN/            '
    nn = nn + 1 ; msgs(nn) = '                                           FRACTIONAL-TO-CARTESIAN)             '
    nn = nn + 1 ; msgs(nn) = '     -p     --apply-periodic-boundary     Relocate all coordinates to positive  '
    nn = nn + 1 ; msgs(nn) = '                                          quadrants.                            '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program VaspConvertPoscarCoordinates
!**********************************************************************************************************************!
