!**********************************************************************************************************************!
!Main program file for matching atoms/indices for SMSS.                                                                !
!**********************************************************************************************************************!
program SmssMatchIndices

  use Parameters,            only: pl, luout, F113, F115
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use MemoryManagement,      only: ResizeArray
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: CoordData, ConfigData, ConfigHeader, PoscarData, PoscarHeader
  use SolvationScheme,       only: FindSmssIndices
  implicit none

  character(len=pl) :: pathc, pathm, paths
  integer           :: pivc, pivm, pivs

  type(GenericTextFile) :: file1, file2, file3
  type(GenericTextFile) :: filed, fileh
  type(CoordData)       :: datac
  type(ConfigData)      :: datam
  type(ConfigHeader)    :: headm
  type(PoscarData)      :: datas
  type(PoscarHeader)    :: heads
  integer, allocatable  :: idxm(:), idxs(:)
  integer               :: ii, code

  procname = 'SmssMatchIndices::SmssMatchIndices'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (filec=pathc, files=paths, filem=pathm, pivc=pivc, pivs=pivs, pivm=pivm)

  !Read QM cluster data.
  call file1%ReadFromDisc (file=pathc)
  call datac%ReadFromFile (file=file1)

  !Read QM surface data.
  if (pivs>0) then
    call file2%ReadFromDisc (file=paths)
    call file2%GetHead (head=fileh, numl=9)
    call heads%ReadFromFile (file=fileh)
    call file2%GetSection (esec=filed, numli=9+1, numlf=9+heads%numa)
    call datas%ReadFromFile (file=filed)
  end if

  !Read MM system data.
  if (pivm>0) then
    call file3%ReadFromDisc (file=pathm)
    call file3%GetHead (head=fileh, numl=5)
    call headm%ReadFromFile (file=fileh)
    call file3%GetSection (esec=filed, numli=5+1, numlf=5+headm%numa*(headm%kfrm+2))
    call datam%ReadFromFile (file=filed, kfrm=headm%kfrm)
  end if

  !Initialize index arrays and find matching indices.
  call ResizeArray (nume=datac%numa, ida=idxm)
  call ResizeArray (nume=datac%numa, ida=idxs)
  idxm = -1
  idxs = -1
  if (pivm>0) call FindSmssIndices (gmc=datac, gmm=datam, pivc=pivc, pivm=pivm, idxm=idxm)
  if (pivs>0) call FindSmssIndices (gmc=datac, gms=datas, pivc=pivc, pivs=pivs, idxs=idxs)

  !Write output file.
  file1%numl = datac%numa + 1
  call file1%Resize()
  file1%line(1) = '$SMSS_MATCH'
  if ((pivm>0) .and. (pivs>0)) then
    do ii = 1, datac%numa
      write (file1%line(ii+1),F115,iostat=code) ii, idxs(ii), idxm(ii)
        if (code>0) call PrintError (ekey=2121, lstop=.true.)
    end do
  else if (pivm>0) then
    do ii = 1, datac%numa
      write (file1%line(ii+1),F113,iostat=code) ii, idxm(ii)
        if (code>0) call PrintError (ekey=2121, lstop=.true.)
    end do
  else if (pivs>0) then
    do ii = 1, datac%numa
      write (file1%line(ii+1),F113,iostat=code) ii, idxs(ii)
        if (code>0) call PrintError (ekey=2121, lstop=.true.)
    end do
  end if
  call file1%WriteToDisc (unit=luout)

  !Clean up memory.
  call file1%Erase()
  call file2%Erase()
  call file3%Erase()
  call filed%Erase()
  call fileh%Erase()
  call datac%Erase()
  call datam%Erase()
  call datas%Erase()
  call heads%Erase()
  call ResizeArray (nume=0, ida=idxs)
  call ResizeArray (nume=0, ida=idxm)

  contains

  !******************************************************************************************************************!
  !                                        PRIVATE PROCEDURE PARSECOMMANDLINE                                        !
  !******************************************************************************************************************!
  subroutine ParseCommandLine (filec, filem, files, pivc, pivm, pivs)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: pl, F001
    implicit none

    character(len=*), intent(inout) :: filec, files, filem
    integer,          intent(inout) :: pivc, pivs, pivm

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'SmssMatchIndices::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    filec = ' '
    filem = ' '
    files = ' '
    pivc  = -1
    pivm  = -1
    pivs  = -1
    lhelp = .false.

    !Read command-line arguments and update corresponding variables.
    nn = IARGC()
    ekey = 0
    do ii = 1, nn
      call GETARG (ii, buff)
      buff = adjustl(buff)

      if ((buff=='-c') .or. (buff=='--qm-cluster-coordinates')) then
        if (ii+2<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) filec
            if (code>0) ekey = 9011
          call GETARG (ii+2, buff)
          buff = adjustl(buff)
          read (buff,*,iostat=code) pivc
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-h') .or. (buff=='--help')) then
        lhelp = .true.

      else if ((buff=='-m') .or. (buff=='--mm-system-coordinates')) then
        if (ii+2<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) filem
            if (code>0) ekey = 9011
          call GETARG (ii+2, buff)
          buff = adjustl(buff)
          read (buff,*,iostat=code) pivm
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-s') .or. (buff=='--qm-surface-coordinates')) then
        if (ii+2<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) files
            if (code>0) ekey = 9011
          call GETARG (ii+2, buff)
          buff = adjustl(buff)
          read (buff,*,iostat=code) pivs
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if
      end if
    end do

    !Check command-line arguments for conflicts.
    if (lhelp) call PrintHelp()
    if (ekey>0) call PrintHelp (ekey=ekey)
    if ((filec==' ') .or. (pivc<1)) call PrintHelp (ekey=9001)
    if ((filem==' ') .and. (files==' ')) call PrintHelp (ekey=9001)
    if ((filem/=' ') .and. (pivm<1)) call PrintHelp (ekey=9001)
    if ((files/=' ') .and. (pivs<1)) call PrintHelp (ekey=9001)

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

    procname = 'SmssMatchIndices::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********                PROGRAM: SMSS-MATCH-INDICES                 **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program matches atoms/indices for SMSS.                                    '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: smss-match-indices -chms                                                 '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -c     --qm-cluster-coordinates     (required) Input coord file followed by'
    nn = nn + 1 ; msgs(nn) = '                                         index of pivot atom.                   '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                       Print this help.                       '
    nn = nn + 1 ; msgs(nn) = '     -m     --mm-system-coordinates      (optional) Input CONFIG file followed  '
    nn = nn + 1 ; msgs(nn) = '                                         by index of pivot atom. If -m is not   '
    nn = nn + 1 ; msgs(nn) = '                                         used, -s must be specified.            '
    nn = nn + 1 ; msgs(nn) = '     -s     --qm-surface-coordinates     (optional) Input POSCAR file followed  '
    nn = nn + 1 ; msgs(nn) = '                                         by index of pivot atom. If -s is not   '
    nn = nn + 1 ; msgs(nn) = '                                         used, -m must be specified.            '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program SmssMatchIndices
!**********************************************************************************************************************!
