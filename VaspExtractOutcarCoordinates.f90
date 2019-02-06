!**********************************************************************************************************************!
!Main program file for extracting converged coordinates from VASP OUTCAR file in XYZ format.                           !
!**********************************************************************************************************************!
program VaspExtractOutcarCoordinates

  use Parameters,            only: pl, luout
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use UtilityProcedures,     only: ConvertToLowerCase, ConvertToUpperCase
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: PoscarData, PoscarHeader, XyzData, XyzHeader, PeriodicBoxIsAligned
  use FileParsers,           only: ExtractVaspGeometry
  implicit none

  character(len=pl) :: pathc, patho
  logical           :: lhead, lpbc

  type(GenericTextFile) :: file1, file2
  type(GenericTextFile) :: filed, fileh
  type(PoscarData)      :: datas
  type(PoscarHeader)    :: heads
  type(XyzData)         :: datax
  type(XyzHeader)       :: headx
  integer               :: ii, idxi, idxf

  procname = 'VaspExtractOutcarCoordinates::VaspExtractOutcarCoordinates'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (filec=pathc, fileo=patho, lhead=lhead, lpbc=lpbc)

  !Read atomic coordinates.
  call file1%ReadFromDisc (file=pathc)
  call file1%GetHead (head=fileh, numl=9)
  call heads%ReadFromFile (file=fileh)
  call file1%GetSection (esec=filed, numli=9+1, numlf=9+heads%numa)
  call datas%ReadFromFile (file=filed)

  !Read last cycle of coordinates.
  call file2%ReadFromDisc (file=patho)
  call ExtractVaspGeometry (file=file2, gms=datas)

  !Relocate coordinates to positive quadrants.
  if (lpbc) then
    if (.not. PeriodicBoxIsAligned(box=heads%box)) call PrintError (ekey=3031, lstop=.true.)
    call datas%ApplyPeriodicBoundary (dx=heads%box(1,1), dy=heads%box(2,2), dz=heads%box(3,3))
  end if

  !Extract header section in specified format.
  if (lhead) then
    headx%name = heads%name
    headx%numa = heads%numa
  end if

  !Extract coordinates in specified format.
  datax%numa = datas%numa
  call datax%Resize()
  datax%cx = datas%cx
  datax%cy = datas%cy
  datax%cz = datas%cz

  !Extract atomic symbols in sentence case.
  idxi = 0
  idxf = 0
  do ii = 1, heads%numt
    call ConvertToUpperCase (line=heads%typs(ii)(1:1))
    call ConvertToLowerCase (line=heads%typs(ii)(2:2))
    idxi = idxf + 1
    idxf = idxf + heads%nums(ii)
    datax%type(idxi:idxf) = heads%typs(ii)
  end do

  !Write output file.
  if (lhead) then
    call headx%WriteToFile (file=fileh)
    call fileh%WriteToDisc (unit=luout)
  end if
  call datax%WriteToFile (file=filed)
  call filed%WriteToDisc (unit=luout)

  !Clean up memory.
  call file1%Erase()
  call file2%Erase()
  call filed%Erase()
  call fileh%Erase()
  call datas%Erase()
  call heads%Erase()
  call datax%Erase()

  contains

  !******************************************************************************************************************!
  !                                        PRIVATE PROCEDURE PARSECOMMANDLINE                                        !
  !******************************************************************************************************************!
  subroutine ParseCommandLine (filec, fileo, lhead, lpbc)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: pl, F001
    implicit none

    character(len=*), intent(inout) :: filec, fileo
    logical,          intent(inout) :: lhead, lpbc

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'VaspExtractOutcarCoordinates::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    filec = 'POSCAR'
    fileo = 'OUTCAR'
    lhead = .true.
    lpbc  = .false.
    lhelp = .false.

    !Read command-line arguments and update corresponding variables.
    nn = IARGC()
    ekey = 0
    do ii = 1, nn
      call GETARG (ii, buff)
      buff = adjustl(buff)

      if ((buff=='-c') .or. (buff=='--coordinate-file')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) filec
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-h') .or. (buff=='--help')) then
        lhelp = .true.

      else if ((buff=='-o') .or. (buff=='--output-file')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) fileo
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

    procname = 'VaspExtractOutcarCoordinates::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********          PROGRAM: VASP-EXTRACT-OUTCAR-COORDINATES          **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program extracts the last converged geometry from VASP OUTCAR file in XYZ  '
    nn = nn + 1 ; msgs(nn) = 'format.                                                                         '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: vasp-extract-outcar-coordinates -chops                                   '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -c     --coordinate-file             VASP POSCAR file.                     '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/POSCAR                                          '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                        Print this help.                      '
    nn = nn + 1 ; msgs(nn) = '     -o     --output-file                 VASP OUTCAR file.                     '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/OUTCAR                                          '
    nn = nn + 1 ; msgs(nn) = '     -p     --apply-periodic-boundary     Relocate all coordinates to positive  '
    nn = nn + 1 ; msgs(nn) = '                                          quadrants.                            '
    nn = nn + 1 ; msgs(nn) = '     -s     --skip-header                 Do not write header in output file.   '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program VaspExtractOutcarCoordinates
!**********************************************************************************************************************!
