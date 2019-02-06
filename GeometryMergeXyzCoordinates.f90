!**********************************************************************************************************************!
!Main program file for merging 2 XYZ files.                                                                            !
!**********************************************************************************************************************!
program GeometryMergeXyzCoordinates

  use Parameters,            only: dp, pl, mtol, zero, luout
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use MemoryManagement,      only: ResizeArray
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: XyzData, XyzHeader
  implicit none

  character(len=pl) :: pathb, pathm
  logical           :: lrip

  type(GenericTextFile) :: file1, file2
  type(GenericTextFile) :: filed, fileh
  type(XyzData)         :: datab, datam, datau
  type(XyzHeader)       :: headb, headm
  integer, allocatable  :: idxb(:), idxm(:)
  integer               :: ii, jj, nn
  real(dp)              :: dx, dy, dz

  procname = 'GeometryMergeXyzCoordinates::GeometryMergeXyzCoordinates'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (fileb=pathb, filem=pathm, lrip=lrip)

  !Read base input file.
  call file1%ReadFromDisc (file=pathb)
  call file1%GetHead (head=fileh, numl=2)
  call headb%ReadFromFile (file=fileh)
  call file1%GetSection (esec=filed, numli=2+1, numlf=2+headb%numa)
  call datab%ReadFromFile (file=filed)

  !Read file to be merged into base file.
  call file2%ReadFromDisc (file=pathm)
  call file2%GetHead (head=fileh, numl=2)
  call headm%ReadFromFile (file=fileh)
  call file2%GetSection (esec=filed, numli=2+1, numlf=2+headm%numa)
  call datam%ReadFromFile (file=filed)

  !Resize arrays for matching atoms/indices and identify matching atoms.
  call ResizeArray (nume=datab%numa, ida=idxb)
  call ResizeArray (nume=datam%numa, ida=idxm)
  idxm = -1
  nn   = 0
  do ii = 1, datam%numa
    do jj = 1, datab%numa
      idxb(jj) = jj
      dx = datam%cx(ii) - datab%cx(jj)
      dy = datam%cy(ii) - datab%cy(jj)
      dz = datam%cz(ii) - datab%cz(jj)
      if ((abs(dx)<mtol) .and. (abs(dy)<mtol) .and. (abs(dz)<mtol)) then
        idxm(ii) = jj
        nn = nn + 1
        exit
      end if
    end do
  end do

  !Allocate memory for combined structure.
  datau%numa = datab%numa + datam%numa - nn
  call datau%Resize()

  !Update (in-place) data for matching atoms in base file.
  if (lrip) then
    do ii = 1, datam%numa
      jj = idxm(ii)
      if (jj/=-1) then
        datab%type(jj) = datam%type(ii)
        datab%cx(jj)   = datam%cx(ii)
        datab%cy(jj)   = datam%cy(ii)
        datab%cz(jj)   = datam%cz(ii)
      end if
    end do
  !Mark matching atoms in base file for deletion.
  else
    do ii = 1, datam%numa
      jj = idxm(ii)
      if (jj/=-1) idxb(jj) = -1
    end do
  end if

  !Extract updated data from base file.
  if (lrip) then
    nn = datab%numa
    datau%type(1:nn) = datab%type(1:nn)
    datau%cx(1:nn)   = datab%cx(1:nn)
    datau%cy(1:nn)   = datab%cy(1:nn)
    datau%cz(1:nn)   = datab%cz(1:nn)
  else
    nn = 0
    do ii = 1, datab%numa
      if (idxb(ii)/=-1) then
        nn = nn + 1
        datau%type(nn) = datab%type(ii)
        datau%cx(nn)   = datab%cx(ii)
        datau%cy(nn)   = datab%cy(ii)
        datau%cz(nn)   = datab%cz(ii)
      end if
    end do
  end if

  !Finally, append all remaining atoms.
  if (lrip) then
    do ii = 1, datam%numa
      jj = idxm(ii)
      if (jj==-1) then
        nn = nn + 1
        datau%type(nn) = datam%type(ii)
        datau%cx(nn)   = datam%cx(ii)
        datau%cy(nn)   = datam%cy(ii)
        datau%cz(nn)   = datam%cz(ii)
      end if
    end do
  else
    datau%type(nn+1:nn+datam%numa) = datam%type(1:datam%numa)
    datau%cx(nn+1:nn+datam%numa)   = datam%cx(1:datam%numa)
    datau%cy(nn+1:nn+datam%numa)   = datam%cy(1:datam%numa)
    datau%cz(nn+1:nn+datam%numa)   = datam%cz(1:datam%numa)
    nn = nn + datam%numa
  end if

  !As a check, the combined array must be completely filled at this point.
  if (nn/=datau%numa) call PrintError (ekey=3312, lstop=.true.)

  !Write output file.
  headb%numa = datau%numa
  call headb%WriteToFile (file=fileh)
  call fileh%WriteToDisc (unit=luout)
  call datau%WriteToFile (file=filed)
  call filed%WriteToDisc (unit=luout)

  !Clean up memory.
  call file1%Erase()
  call file2%Erase()
  call filed%Erase()
  call fileh%Erase()
  call datab%Erase()
  call datam%Erase()
  call datau%Erase()

  contains

  !******************************************************************************************************************!
  !                                        PRIVATE PROCEDURE PARSECOMMANDLINE                                        !
  !******************************************************************************************************************!
  subroutine ParseCommandLine (fileb, filem, lrip)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: pl, F001
    implicit none

    character(len=*), intent(inout) :: fileb, filem
    logical,          intent(inout) :: lrip

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'GeometryMergeXyzCoordinates::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    fileb = ' '
    filem = ' '
    lrip  = .false.
    lhelp = .false.

    !Read command-line arguments and update corresponding variables.
    nn = IARGC()
    ekey = 0
    do ii = 1, nn
      call GETARG (ii, buff)
      buff = adjustl(buff)

      if ((buff=='-b') .or. (buff=='--base-file')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) fileb
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-h') .or. (buff=='--help')) then
        lhelp = .true.

      else if ((buff=='-m') .or. (buff=='--merge-file')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) filem
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-r') .or. (buff=='--replace-inplace')) then
        lrip = .true.
      end if
    end do

    !Check command-line arguments for conflicts.
    if (lhelp) call PrintHelp()
    if (ekey>0) call PrintHelp (ekey=ekey)
    if ((fileb==' ') .or. (filem==' ')) call PrintHelp (ekey=9001)

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

    procname = 'GeometryMergeXyzCoordinates::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********          PROGRAM: GEOMETRY-MERGE-XYZ-COORDINATES           **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program merges 2 XYZ files.                                                '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: geometry-merge-xyz-coordinates -bhmr                                     '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -b     --base-file           (required) Input base XYZ file.               '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                Print this help.                              '
    nn = nn + 1 ; msgs(nn) = '     -m     --merge-file          (required) Input XYZ file to be merged in the '
    nn = nn + 1 ; msgs(nn) = '                                  base structure.                               '
    nn = nn + 1 ; msgs(nn) = '     -r     --replace-inplace     Replace matching atoms in-place? or delete    '
    nn = nn + 1 ; msgs(nn) = '                                  matching atoms in the base structure and add  '
    nn = nn + 1 ; msgs(nn) = '                                  corresponding atoms at the end?               '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program GeometryMergeXyzCoordinates
!**********************************************************************************************************************!
