!**********************************************************************************************************************!
!Main program file for aligning a cluster to the center of a periodic box.                                             !
!**********************************************************************************************************************!
program VaspMoveClusterToCenter

  use Parameters,            only: dp, pl, luout
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: PoscarData, PoscarHeader
  implicit none

  character(len=pl) :: path

  type(GenericTextFile) :: file0
  type(GenericTextFile) :: filed, fileh
  type(PoscarData)      :: datas
  type(PoscarHeader)    :: heads
  real(dp)              :: dx, dy

  procname = 'VaspMoveClusterToCenter::VaspMoveClusterToCenter'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (file=path)

  !Read input file.
  call file0%ReadFromDisc (file=path)
  call file0%GetHead (head=fileh, numl=9)
  call heads%ReadFromFile (file=fileh)
  call file0%GetSection (esec=filed, numli=9+1, numlf=9+heads%numa)
  call datas%ReadFromFile (file=filed)

  !Set all atoms to free. Then fix outer circles (indices 1-13 and 22-37).
  datas%mx = 'T'
  datas%my = 'T'
  datas%mz = 'T'
  datas%mx(1:13) = 'F'
  datas%my(1:13) = 'F'
  datas%mz(1:13) = 'F'
  datas%mx(22:37) = 'F'
  datas%my(22:37) = 'F'
  datas%mz(22:37) = 'F'

  !Change periodic-box size.
  heads%box(1,1) = 25.0_dp
  heads%box(2,2) = 25.0_dp
  heads%box(3,3) = 22.0_dp

  !Calculate X and Y centers of top layer (indices 22-51).
  dx = sum(datas%cx(22:51)) / 30.0_dp
  dy = sum(datas%cy(22:51)) / 30.0_dp

  !Align X and Y centers of cluster with X and Y centers of periodic-box.
  dx = dx - 12.5_dp
  dy = dy - 12.5_dp
  call datas%Translate (dx=-dx, dy=-dy)

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
  subroutine ParseCommandLine (file)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: pl, F001
    implicit none

    character(len=*), intent(inout) :: file

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'VaspMoveClusterToCenter::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    file  = 'POSCAR'
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

    procname = 'VaspMoveClusterToCenter::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********            PROGRAM: VASP-MOVE-CLUSTER-TO-CENTER            **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program aligns a cluster with the center of the periodic box.              '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: vasp-move-cluster-to-center -fh                                          '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -f     --file                        VASP POSCAR file.                     '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/POSCAR                                          '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                        Print this help.                      '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program VaspMoveClusterToCenter
!**********************************************************************************************************************!
