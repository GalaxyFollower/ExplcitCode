!**********************************************************************************************************************!
!Main program file for calculating the moment-of-inertia matrix.                                                       !
!**********************************************************************************************************************!
program MomentOfInertia

  use Parameters,            only: dp, pl, zero, luout, B2A
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: CoordData
  implicit none

  character(len=pl) :: path

  type(GenericTextFile) :: file0
  type(CoordData)       :: datac
  integer               :: ii, jj, nn
  real(dp)              :: moim(3,3)
  real(dp)              :: totm, mass
  real(dp)              :: cmx, cmy, cmz
  real(dp)              :: Ixx, Iyy, Izz, Ixy, Ixz, Iyz
  real(dp)              :: dx, dy, dz

  procname = 'MomentOfInertia::MomentOfInertia'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (file=path)

  !Read base input file.
  call file0%ReadFromDisc (file=path)
  call datac%ReadFromFile (file=file0)

  !Convert all coordinates to Angstroms.
  datac%cx = datac%cx * B2A
  datac%cy = datac%cy * B2A
  datac%cz = datac%cz * B2A

  !Find center-of-mass of the molecule.
  totm = zero
  do ii = 1, datac%numa
    if (datac%type(ii) == 'c') mass = 12.0110_dp
    if (datac%type(ii) == 'o') mass = 16.0000_dp
    if (datac%type(ii) == 'h') mass = 1.00000_dp
    totm = totm + mass
    cmx = cmx + mass * datac%cx(ii)
    cmy = cmy + mass * datac%cy(ii)
    cmz = cmz + mass * datac%cz(ii)
  end do
  cmx = cmx / totm
  cmy = cmy / totm
  cmz = cmz / totm

  !Calculate elements of the moment-of-inertia matrix.
  Ixx = zero
  Iyy = zero
  Izz = zero
  Ixy = zero
  Iyy = zero
  Iyz = zero
  do ii = 1, datac%numa
    dx = datac%cx(ii) - cmx
    dy = datac%cy(ii) - cmy
    dz = datac%cz(ii) - cmz
    if (datac%type(ii) == 'c') mass = 12.0110_dp
    if (datac%type(ii) == 'o') mass = 16.0000_dp
    if (datac%type(ii) == 'h') mass = 1.00000_dp
    Ixx = Ixx + mass * (dy**2 + dz**2)
    Iyy = Iyy + mass * (dx**2 + dz**2)
    Izz = Izz + mass * (dx**2 + dy**2)
    Ixy = Ixy + mass * (dx * dy)
    Ixz = Ixz + mass * (dx * dz)
    Iyz = Iyz + mass * (dy * dz)
  end do

  !Build moment-of-inertia matrix.
  moim(1,1) = Ixx
  moim(1,2) = -Ixy
  moim(1,3) = -Ixz
  moim(2,1) = -Ixy
  moim(2,2) = Iyy
  moim(2,3) = -Iyz
  moim(3,1) = -Ixz
  moim(3,2) = -Iyz
  moim(3,3) = Izz

  !Write to output.
  write (luout,*) moim

  !Clean up memory.
  call file0%Erase()
  call datac%Erase()

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

    procname = 'MomentOfInertia::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    file = 'coord'
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
    if (file==' ') call PrintHelp (ekey=9001)

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

    procname = 'MomentOfInertia::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********                 PROGRAM: MOMENT-OF-INERTIA                 **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program calculates the moment-of-inertia matrix for a TURBOMOLE coord file.'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: moment-of-inertia -fh                                                    '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -f     --file                        TURBOMOLE coord file.                 '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/coord                                           '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                        Print this help.                      '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program MomentOfInertia
!**********************************************************************************************************************!
