!**********************************************************************************************************************!
!Main program file for substituting TURBOMOLE-calculated charges in DLPOLY FIELD file.                                 !
!**********************************************************************************************************************!
program DlpolyReplaceFieldCharges

  use Parameters,            only: pl, sl, F102, F211
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use CommandExecutors,      only: ExecuteSystemCommand
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: CoordData
  use SolvationScheme,       only: SmssGeometry
  use FileParsers,           only: ExtractTurbomoleCharges
  implicit none

  character(len=pl) :: pathc, patho
  character(len=sl) :: mode

  type(GenericTextFile) :: file1, file2
  type(GenericTextFile) :: filee
  type(CoordData)       :: datac
  type(SmssGeometry)    :: sgmn
  character(len=sl)     :: buff1, buff2
  integer               :: ii, code

  procname = 'DlpolyReplaceFieldCharges::DlpolyReplaceFieldCharges'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (filec=pathc, fileo=patho, mode=mode)

  !Read QM cluster data from input file.
  call file1%ReadFromDisc (file=pathc)
  call datac%ReadFromFile (file=file1)

  !Import coordinates.
  sgmn%numa = datac%numa
  call sgmn%Initialize()
  call sgmn%ImportCoordinates (gmc=datac)

  !Extract charges.
  call file2%ReadFromDisc (file=patho)
  call ExtractTurbomoleCharges (file=file2, gmi=sgmn, mode=mode)
  call sgmn%NormalizeCharges()

  !Create sed script and insert commands for substituting atomic charges in FIELD file.
  filee%numl = sgmn%numa
  call filee%Resize()
  do ii = 1, sgmn%numa
    write (buff1,F102,iostat=code) ii
      if (code>0) call PrintError (ekey=2121, lstop=.true.)
    buff1 = adjustl(buff1)
    write (buff2,F211,iostat=code) sgmn%pc(ii)
      if (code>0) call PrintError (ekey=2121, lstop=.true.)
    buff2 = adjustl(buff2)
    if (buff2(1:1)=='-') then
      filee%line(ii) = 's/MMS_ATOM_' // trim(buff1) // '/' // trim(buff2) // '/g'
    else
      filee%line(ii) = 's/MMS_ATOM_' // trim(buff1) // '/ ' // trim(buff2) // '/g'
    end if
  end do

  !Write the script, make it executable and execute.
  call filee%WriteToDisc (file='sed-script', mode='replace')
  call ExecuteSystemCommand (cmdl='sed -f sed-script < FIELD_TEMPLATE > FIELD')

  !Clean up memory.
  call file1%Erase()
  call file2%Erase()
  call filee%Erase()
  call datac%Erase()
  call sgmn%Erase()

  contains

  !******************************************************************************************************************!
  !                                        PRIVATE PROCEDURE PARSECOMMANDLINE                                        !
  !******************************************************************************************************************!
  subroutine ParseCommandLine (filec, fileo, mode)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: pl, F001
    use UtilityProcedures, only: ConvertToUpperCase
    implicit none

    character(len=*), intent(inout) :: filec, fileo, mode

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'DlpolyReplaceFieldCharges::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    filec = 'coord'
    fileo = 'ridft.out'
    mode  = 'NATURAL'
    lhelp = .false.

    !Read command-line arguments and update corresponding variables.
    nn = IARGC()
    ekey = 0
    do ii = 1, nn
      call GETARG (ii, buff)
      buff = adjustl(buff)

      if ((buff=='-c') .or. (buff=='--coord-file')) then
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

      else if ((buff=='-m') .or. (buff=='--method')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) mode
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-o') .or. (buff=='--output-file')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) fileo
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if
      end if
    end do

    !Check command-line arguments for conflicts.
    if (lhelp) call PrintHelp()
    if (ekey>0) call PrintHelp (ekey=ekey)
    call ConvertToUpperCase (line=mode)
    if ((mode/='ESPFIT') .and. (mode/='MULLIKAN') .and. (mode/='NATURAL')) call PrintHelp (ekey=9003)

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

    procname = 'DlpolyReplaceFieldCharges::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********           PROGRAM: DLPOLY-REPLACE-FIELD-CHARGES            **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program substitutes atomic charges in a DLPOLY FIELD file.                 '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: dlpoly-replace-field-charges -chmo                                       '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -c     --coord-file      TURBOMOLE coord file.                             '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/coord                                           '
    nn = nn + 1 ; msgs(nn) = '     -h     --help            Print this help.                                  '
    nn = nn + 1 ; msgs(nn) = '     -m     --method          Method for calculating charges                    '
    nn = nn + 1 ; msgs(nn) = '                              (ESPFIT/NATURAL/MULLIKAN).                        '
    nn = nn + 1 ; msgs(nn) = '                 Default = NATURAL                                              '
    nn = nn + 1 ; msgs(nn) = '     -o     --output-file     TURBOMOLE output file for reading charges.        '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/ridft.out                                       '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program DlpolyReplaceFieldCharges
!**********************************************************************************************************************!
