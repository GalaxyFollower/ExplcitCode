!**********************************************************************************************************************!
!Main program file for calculating energy/gradients for all images in a DLPOLY HISTORY file.                           !
!**********************************************************************************************************************!
program DlpolyReplayHistory

  use Parameters,            only: pl, sl, F101
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use Timers,                only: StopWatch
  use UtilityProcedures,     only: AvailableLogicalUnit
  use CommandExecutors,      only: ExecuteSystemCommand
  use GenericInputOutPut,    only: GenericTextFile
  use CoordinateFileFormats, only: ConfigData, ConfigHeader
  use SolvationScheme,       only: SmssGeometry, SynchronizeSmssCoordinates
  use FileParsers,           only: ReadSmssEnsemble
  implicit none

  integer :: ncores, offset

  type(StopWatch)       :: timer
  type(GenericTextFile) :: file1, file2, file3
  type(GenericTextFile) :: filed, fileh
  type(ConfigData)      :: datam
  type(ConfigHeader)    :: headm
  type(SmssGeometry)    :: sgmn
  character(len=pl)     :: cmdl
  character(len=sl)     :: buff1, buff2
  integer               :: ntot
  integer               :: ii, unit, code
  integer               :: aa, bb, cc

  procname = 'DlpolyReplayHistory::DlpolyReplayHistory'
  if (ltrace) call PrintTrace

  call timer%Start()
  timer%name = 'REPLAY Timer'

  !Read command-line arguments and create command-line for execution.
  call ParseCommandLine (ncores=ncores, offset=offset)
  write (buff1,F101,iostat=code) ncores
    if (code>0) call PrintError (ekey=2121, lstop=.true.)
  buff1 = adjustl(buff1)
  write (buff2,F101,iostat=code) offset
    if (code>0) call PrintError (ekey=2121, lstop=.true.)
  buff2 = adjustl(buff2)
  cmdl = 'mpirun -np 16 /home/mzare/DL_POLY/dlpoly-4.03.4-SH-Corrected/execute/dlpoly-psr 1> stdout 2> stderr'

  !Read matching indices and coordinate shifts.
  call file1%ReadFromDisc (file='MMS_ENSEMBLE')
  call ReadSmssEnsemble (file=file1, gmi=sgmn)

  !Open HISTORY file and read first 2 lines. Extract number of frames from second line (ignore file-format,
  !periodic-box type, and total number of atoms).
  unit = AvailableLogicalUnit (guess=11)
  open (unit=unit, file='HISTORY', status='OLD', action='READ', position='REWIND', iostat=code)
    if (code>0) call PrintError (ekey=2101, lstop=.true., msg1='File: HISTORY')
  ntot = 0
  call file2%ReadFromDisc (unit=unit, numl=2)
  read (file2%line(2),*,iostat=code) aa, bb, cc, ntot
    if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='File: HISTORY')
  if (ntot<1) call PrintError (ekey=1304, lstop=.true., msg1='HISTORY: number of frames')

  !Loop over all frames in HISTORY file.
  do ii = 1, ntot
    !Extract header from lines 1-4 starting at current position.
    call file2%ReadFromDisc (unit=unit, numl=4)
    call headm%ReadFromFile (file=file2)
    headm%name = 'HISTORY frame'
    call headm%WriteToFile (file=fileh)

    !Determine size of data section using header section. Then extract data section.
    aa = 0
    if ((headm%numa>0) .and. ((headm%kfrm==0) .or. (headm%kfrm==1) .or. (headm%kfrm==2)) .and.  &
       ((headm%kpbc==1) .or. (headm%kpbc==2))) aa = headm%numa * (headm%kfrm+2)
    if (aa<2) call PrintError (ekey=1304, lstop=.true., msg1='CONFIG: header for SMSS')
    call file3%ReadFromDisc (unit=unit, numl=aa)
    call datam%ReadFromFile (file=file3, kfrm=headm%kfrm)

    !Update coordinates of SMSS atoms and write CONFIG file in coordinates-only format.
    call SynchronizeSmssCoordinates (gmm=datam, gmi=sgmn)
    call datam%WriteToFile (file=filed, kfrm=0)
    call fileh%WriteToDisc (file='CONFIG', mode='replace')
    call filed%WriteToDisc (file='CONFIG', mode='append')

    !Execute command-line.
    call ExecuteSystemCommand (cmdl=cmdl)
  end do

  close (unit=unit)

  !Clean up memory.
  call file1%Erase()
  call file2%Erase()
  call file3%Erase()
  call filed%Erase()
  call fileh%Erase()
  call datam%Erase()
  call sgmn%Erase()

  call timer%Stop()
  call timer%Print()

  contains

  !******************************************************************************************************************!
  !                                        PRIVATE PROCEDURE PARSECOMMANDLINE                                        !
  !******************************************************************************************************************!
  subroutine ParseCommandLine (ncores, offset)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: pl
    implicit none

    integer, intent(inout) :: ncores, offset

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'DlpolyReplayHistory::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    ncores = 1
    offset = 0
    lhelp  = .false.

    !Read command-line arguments and update corresponding variables.
    nn = IARGC()
    ekey = 0
    do ii = 1, nn
      call GETARG (ii, buff)
      buff = adjustl(buff)

      if ((buff=='-h') .or. (buff=='--help')) then
        lhelp = .true.

      else if ((buff=='-n') .or. (buff=='--number-of-cores')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,*,iostat=code) ncores
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-o') .or. (buff=='--offset')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,*,iostat=code) offset
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if
      end if
    end do

    !Check command-line arguments for conflicts.
    if (lhelp) call PrintHelp()
    if (ekey>0) call PrintHelp (ekey=ekey)
    if ((ncores<1) .or. (offset<0)) call PrintHelp (ekey=9003)

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

    procname = 'DlpolyReplayHistory::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********               PROGRAM: DLPOLY-REPLAY-HISTORY               **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program calculates energy/gradients for all images in a DLPOLY HISTORY     '
    nn = nn + 1 ; msgs(nn) = 'file.                                                                           '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: dlpoly-replay-history -hno                                               '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                Print this help.                              '
    nn = nn + 1 ; msgs(nn) = '     -n     --number-of-cores     Number of cores used for execution.           '
    nn = nn + 1 ; msgs(nn) = '                 Default = 1                                                    '
    nn = nn + 1 ; msgs(nn) = '     -o     --offset              Offset (cores) for parallel execution streams.'
    nn = nn + 1 ; msgs(nn) = '                 Default = 0                                                    '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program DlpolyReplayHistory
!**********************************************************************************************************************!
