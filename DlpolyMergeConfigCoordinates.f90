!**********************************************************************************************************************!
!Main program file for merging 2 DLPOLY CONFIG files.                                                                  !
!**********************************************************************************************************************!
program DlpolyMergeConfigCoordinates

  use Parameters,            only: dp, pl, dtol, mtol, zero, luout
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use MemoryManagement,      only: ResizeArray
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: ConfigData, ConfigHeader
  implicit none

  character(len=pl) :: pathb, pathm
  logical           :: lrip

  type(GenericTextFile) :: file1, file2
  type(GenericTextFile) :: filed, fileh
  type(ConfigData)      :: datab, datam, datau
  type(ConfigHeader)    :: headb, headm
  integer, allocatable  :: idx(:)
  integer               :: ii, jj, nn
  real(dp)              :: dx, dy, dz

  procname = 'DlpolyMergeConfigCoordinates::DlpolyMergeConfigCoordinates'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (fileb=pathb, filem=pathm, lrip=lrip)

  !Read base input file.
  call file1%ReadFromDisc (file=pathb)
  call file1%GetHead (head=fileh, numl=5)
  call headb%ReadFromFile (file=fileh)
  call file1%GetSection (esec=filed, numli=5+1, numlf=5+headb%numa*(headb%kfrm+2))
  call datab%ReadFromFile (file=filed, kfrm=headb%kfrm)

  !Read file to be merged into base file.
  call file2%ReadFromDisc (file=pathm)
  call file2%GetHead (head=fileh, numl=5)
  call headm%ReadFromFile (file=fileh)
  call file2%GetSection (esec=filed, numli=5+1, numlf=5+headm%numa*(headm%kfrm+2))
  call datam%ReadFromFile (file=filed, kfrm=headm%kfrm)

  !Verify valid inputs.
  if ((headb%kpbc/=headm%kpbc) .or. any(abs(headb%box-headm%box)>dtol)) call PrintError (ekey=3311, lstop=.true.)

  !Resize array for matching atoms/indices and identify matching atoms.
  call ResizeArray (nume=datam%numa, ida=idx)
  idx = -1
  nn  = 0
  do ii = 1, datam%numa
    do jj = 1, datab%numa
      dx = datam%cx(ii) - datab%cx(jj)
      dy = datam%cy(ii) - datab%cy(jj)
      dz = datam%cz(ii) - datab%cz(jj)
      if ((abs(dx)<mtol) .and. (abs(dy)<mtol) .and. (abs(dz)<mtol)) then
        idx(ii) = jj
        nn = nn + 1
        exit
      end if
    end do
  end do

  !Allocate memory for combined structure.
  datau%numa = datab%numa + datam%numa - nn
  call datau%Resize()
  datau%vx = zero
  datau%vy = zero
  datau%vz = zero
  datau%fx = zero
  datau%fy = zero
  datau%fz = zero

  !Update (in-place) data for matching atoms in base file.
  if (lrip) then
    do ii = 1, datam%numa
      jj = idx(ii)
      if (jj/=-1) then
        datab%type(jj) = datam%type(ii)
        datab%cx(jj)   = datam%cx(ii)
        datab%cy(jj)   = datam%cy(ii)
        datab%cz(jj)   = datam%cz(ii)
        if (headb%kfrm>0) then
          if (headm%kfrm>0) then
            datab%vx(jj) = datam%vx(ii)
            datab%vy(jj) = datam%vy(ii)
            datab%vz(jj) = datam%vz(ii)
          else
            datab%vx(jj) = zero
            datab%vy(jj) = zero
            datab%vz(jj) = zero
          end if
        end if
        if (headb%kfrm>1) then
          if (headm%kfrm>1) then
            datab%fx(jj) = datam%fx(ii)
            datab%fy(jj) = datam%fy(ii)
            datab%fz(jj) = datam%fz(ii)
          else
            datab%fx(jj) = zero
            datab%fy(jj) = zero
            datab%fz(jj) = zero
          end if
        end if
      end if
    end do
  !Mark matching atoms in base file for deletion.
  else
    do ii = 1, datam%numa
      jj = idx(ii)
      if (jj/=-1) datab%idx(jj) = -1
    end do
  end if

  !Extract updated data from base file.
  if (lrip) then
    nn = datab%numa
    datau%type(1:nn) = datab%type(1:nn)
    datau%idx(1:nn)  = datab%idx(1:nn)
    datau%cx(1:nn)   = datab%cx(1:nn)
    datau%cy(1:nn)   = datab%cy(1:nn)
    datau%cz(1:nn)   = datab%cz(1:nn)
    if (headb%kfrm>0) then
      datau%vx(1:nn) = datab%vx(1:nn)
      datau%vy(1:nn) = datab%vy(1:nn)
      datau%vz(1:nn) = datab%vz(1:nn)
    end if
    if (headb%kfrm>1) then
      datau%fx(1:nn) = datab%fx(1:nn)
      datau%fy(1:nn) = datab%fy(1:nn)
      datau%fz(1:nn) = datab%fz(1:nn)
    end if
  else
    nn = 0
    do ii = 1, datab%numa
      if (datab%idx(ii)/=-1) then
        nn = nn + 1
        datau%idx(nn)  = nn
        datau%type(nn) = datab%type(ii)
        datau%cx(nn)   = datab%cx(ii)
        datau%cy(nn)   = datab%cy(ii)
        datau%cz(nn)   = datab%cz(ii)
        if (headb%kfrm>0) then
          datau%vx(nn) = datab%vx(ii)
          datau%vy(nn) = datab%vy(ii)
          datau%vz(nn) = datab%vz(ii)
        end if
        if (headb%kfrm>1) then
          datau%fx(nn) = datab%fx(ii)
          datau%fy(nn) = datab%fy(ii)
          datau%fz(nn) = datab%fz(ii)
        end if
      end if
    end do
  end if

  !Finally, append all remaining atoms.
  if (lrip) then
    do ii = 1, datam%numa
      jj = idx(ii)
      if (jj==-1) then
        nn = nn + 1
        datau%idx(nn)  = nn
        datau%type(nn) = datam%type(ii)
        datau%cx(nn)   = datam%cx(ii)
        datau%cy(nn)   = datam%cy(ii)
        datau%cz(nn)   = datam%cz(ii)
        if (headm%kfrm>0) then
          datau%vx(nn) = datam%vx(ii)
          datau%vy(nn) = datam%vy(ii)
          datau%vz(nn) = datam%vz(ii)
        end if
        if (headm%kfrm>1) then
          datau%fx(nn) = datam%fx(ii)
          datau%fy(nn) = datam%fy(ii)
          datau%fz(nn) = datam%fz(ii)
        end if
      end if
    end do
  else
    do ii = 1, datam%numa
      datam%idx(ii) = nn + ii
    end do
    datau%type(nn+1:nn+datam%numa) = datam%type(1:datam%numa)
    datau%idx(nn+1:nn+datam%numa)  = datam%idx(1:datam%numa)
    datau%cx(nn+1:nn+datam%numa)   = datam%cx(1:datam%numa)
    datau%cy(nn+1:nn+datam%numa)   = datam%cy(1:datam%numa)
    datau%cz(nn+1:nn+datam%numa)   = datam%cz(1:datam%numa)
    if (headb%kfrm>0) then
      datau%vx(nn+1:nn+datam%numa) = datam%vx(1:datam%numa)
      datau%vy(nn+1:nn+datam%numa) = datam%vy(1:datam%numa)
      datau%vz(nn+1:nn+datam%numa) = datam%vz(1:datam%numa)
    end if
    if (headb%kfrm>1) then
      datau%fx(nn+1:nn+datam%numa) = datam%fx(1:datam%numa)
      datau%fy(nn+1:nn+datam%numa) = datam%fy(1:datam%numa)
      datau%fz(nn+1:nn+datam%numa) = datam%fz(1:datam%numa)
    end if
    nn = nn + datam%numa
  end if

  !As a check, the combined array must be completely filled at this point.
  if (nn/=datau%numa) call PrintError (ekey=3312, lstop=.true.)

  !Write output file.
  headb%numa = datau%numa
  call headb%WriteToFile (file=fileh)
  call fileh%WriteToDisc (unit=luout)
  call datau%WriteToFile (file=filed, kfrm=headb%kfrm)
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

    procname = 'DlpolyMergeConfigCoordinates::ParseCommandLine'
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

    procname = 'DlpolyMergeConfigCoordinates::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********          PROGRAM: DLPOLY-MERGE-CONFIG-COORDINATES          **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program merges 2 DLPOLY CONFIG files.                                      '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: dlpoly-merge-config-coordinates -bhmr                                    '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -b     --base-file           (required) Input base CONFIG file.            '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                Print this help.                              '
    nn = nn + 1 ; msgs(nn) = '     -m     --merge-file          (required) Input CONFIG file to be merged in  '
    nn = nn + 1 ; msgs(nn) = '                                  the base structure.                           '
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

end program DlpolyMergeConfigCoordinates
!**********************************************************************************************************************!
