!**********************************************************************************************************************!
!Main program file for extracting the maximum absolute force component for a free atom from VASP OUTCAR file.          !
!**********************************************************************************************************************!
program VaspExtractMaximumAbsoluteForce

  use Parameters,            only: dp, pl, zero, luout, F233
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: PoscarData, PoscarHeader
  use FileParsers,           only: ExtractVaspGradients
  implicit none

  character(len=pl) :: pathc, pathg

  type(GenericTextFile) :: file1, file2
  type(GenericTextFile) :: filed, fileh
  type(PoscarData)      :: datas
  type(PoscarHeader)    :: heads
  integer               :: ii
  real(dp)              :: maxf, maxx, maxy, maxz

  procname = 'VaspExtractMaximumAbsoluteForce::VaspExtractMaximumAbsoluteForce'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (filec=pathc, fileg=pathg)

  !Read atomic coordinates.
  call file1%ReadFromDisc (file=pathc)
  call file1%GetHead (head=fileh, numl=9)
  call heads%ReadFromFile (file=fileh)
  call file1%GetSection (esec=filed, numli=9+1, numlf=9+heads%numa)
  call datas%ReadFromFile (file=filed)

  !Read last cycle of gradients.
  call file2%ReadFromDisc (file=pathg)
  call ExtractVaspGradients (file=file2, gms=datas)

  !Remove gradients from fixed atoms.
  do ii = 1, datas%numa
    if ((datas%mx(ii)=='f') .or. (datas%mx(ii)=='F')) datas%fx(ii) = zero
    if ((datas%my(ii)=='f') .or. (datas%my(ii)=='F')) datas%fy(ii) = zero
    if ((datas%mz(ii)=='f') .or. (datas%mz(ii)=='F')) datas%fz(ii) = zero
  end do

  !Find the maximum absolute force component and report.
  maxx = maxval (abs(datas%fx))
  maxy = maxval (abs(datas%fy))
  maxz = maxval (abs(datas%fz))
  maxf = max (maxx, maxy, maxz)
  write (luout,F233) maxf

  !Clean up memory.
  call file1%Erase()
  call file2%Erase()
  call filed%Erase()
  call fileh%Erase()
  call datas%Erase()
  call heads%Erase()

  contains

  !******************************************************************************************************************!
  !                                        PRIVATE PROCEDURE PARSECOMMANDLINE                                        !
  !******************************************************************************************************************!
  subroutine ParseCommandLine (filec, fileg)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: pl, F001
    implicit none

    character(len=*), intent(inout) :: filec, fileg

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'VaspExtractMaximumAbsoluteForce::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    filec = 'POSCAR'
    fileg = 'OUTCAR'
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

      else if ((buff=='-g') .or. (buff=='--gradient-file')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) fileg
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if
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

    procname = 'VaspExtractMaximumAbsoluteForce::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********        PROGRAM: VASP-EXTRACT-MAXIMUM-ABSOLUTE-FORCE        **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program extracts the maximum absolut force component for a free atom from  '
    nn = nn + 1 ; msgs(nn) = 'VASP OUTCAR file.                                                               '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: vasp-extract-maximum-absolute-force -chg                                 '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -c     --coordinate-file     VASP POSCAR file.                             '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/POSCAR                                          '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                Print this help.                              '
    nn = nn + 1 ; msgs(nn) = '     -g     --gradient-file       VASP OUTCAR file.                             '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/OUTCAR                                          '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program VaspExtractMaximumAbsoluteForce
!**********************************************************************************************************************!
