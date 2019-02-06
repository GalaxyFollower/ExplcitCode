!**********************************************************************************************************************!
!Main program file for extracting the maximum absolute force component for a free atom from TURBOMOLE gradient file.   !
!**********************************************************************************************************************!
program TurbomoleExtractMaximumAbsoluteForce

  use Parameters,            only: dp, pl, zero, luout, F233
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: CoordData
  use FileParsers,           only: ExtractTurbomoleGradients
  implicit none

  character(len=pl) :: pathc, pathg

  type(GenericTextFile) :: file1, file2
  type(CoordData)       :: datac
  integer               :: ii
  real(dp)              :: maxf, maxx, maxy, maxz

  procname = 'TurbomoleExtractMaximumAbsoluteForce::TurbomoleExtractMaximumAbsoluteForce'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (filec=pathc, fileg=pathg)

  !Read atomic coordinates.
  call file1%ReadFromDisc (file=pathc)
  call datac%ReadFromFile (file=file1)

  !Read last cycle of gradients.
  call file2%ReadFromDisc (file=pathg)
  call ExtractTurbomoleGradients (file=file2, gmc=datac)

  !Remove gradients from fixed atoms.
  do ii = 1, datac%numa
    if (datac%fix(ii)) then
      datac%fx(ii) = zero
      datac%fy(ii) = zero
      datac%fz(ii) = zero
    end if
  end do

  !Find the maximum absolute force component and report.
  maxx = maxval (abs(datac%fx))
  maxy = maxval (abs(datac%fy))
  maxz = maxval (abs(datac%fz))
  maxf = max (maxx, maxy, maxz)
  write (luout,F233) maxf

  !Clean up memory.
  call file1%Erase()
  call file2%Erase()
  call datac%Erase()

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

    procname = 'TurbomoleExtractMaximumAbsoluteForce::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    filec = 'coord'
    fileg = 'gradient'
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

    procname = 'TurbomoleExtractMaximumAbsoluteForce::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********     PROGRAM: TURBOMOLE-EXTRACT-MAXIMUM-ABSOLUTE-FORCE      **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program extracts the maximum absolut force component for a free atom from  '
    nn = nn + 1 ; msgs(nn) = 'TURBOMOLE gradient file.                                                        '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: turbomole-extract-maximum-absolute-force -chg                            '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -c     --coordinate-file     TURBOMOLE coord file.                         '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/coord                                           '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                Print this help.                              '
    nn = nn + 1 ; msgs(nn) = '     -g     --gradient-file       TURBOMOLE gradient file.                      '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/gradient                                        '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program TurbomoleExtractMaximumAbsoluteForce
!**********************************************************************************************************************!
