!**********************************************************************************************************************!
!Main program file for converting a file from XYZ format to DLPOLY CONFIG format.                                      !
!**********************************************************************************************************************!
program GeometryConvertXyzToDlpolyConfig

  use Parameters,            only: pl, zero, luout
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: ConfigData, ConfigHeader, XyzData, XyzHeader
  implicit none

  character(len=pl) :: path
  integer           :: kfrm, sidx
  logical           :: lhead

  type(GenericTextFile) :: file0
  type(GenericTextFile) :: filed, fileh
  type(ConfigData)      :: datam
  type(ConfigHeader)    :: headm
  type(XyzData)         :: datax
  type(XyzHeader)       :: headx
  integer               :: ii

  procname = 'GeometryConvertXyzToDlpolyConfig::GeometryConvertXyzToDlpolyConfig'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine (file=path, kfrm=kfrm, sidx=sidx, lhead=lhead)

  !Read input file.
  call file0%ReadFromDisc (file=path)
  if (lhead) then
    call file0%GetHead (head=fileh, numl=2)
    call headx%ReadFromFile (file=fileh)
    call file0%GetSection (esec=filed, numli=2+1, numlf=2+headx%numa)
    call datax%ReadFromFile (file=filed)
  else
    call file0%GetSection (esec=filed, numli=1, numlf=file0%numl)
    call datax%ReadFromFile (file=filed)
  end if

  !Extract header section in specified format.
  if (lhead) then
    headm%name = headx%name
    headm%numa = headx%numa
    headm%kfrm = kfrm
    headm%kpbc = 2
    headm%box(1,1) = maxval(datax%cx) - minval(datax%cx)
    headm%box(2,2) = maxval(datax%cy) - minval(datax%cy)
    headm%box(3,3) = maxval(datax%cz) - minval(datax%cz)
  end if

  !Extract data section in specified format.
  datam%numa = datax%numa
  call datam%Resize()
  datam%type = datax%type
  datam%cx   = datax%cx
  datam%cy   = datax%cy
  datam%cz   = datax%cz
  if (kfrm>0) then
    datam%vx = zero
    datam%vy = zero
    datam%vz = zero
  end if
  if (kfrm>1) then
    datam%fx = zero
    datam%fy = zero
    datam%fz = zero
  end if
  sidx = sidx - 1
  do ii = 1, datam%numa
    datam%idx(ii) = ii + sidx
  end do

  !Write output file.
  if (lhead) then
    call headm%WriteToFile (file=fileh)
    call fileh%WriteToDisc (unit=luout)
  end if
  call datam%WriteToFile (file=filed, kfrm=kfrm)
  call filed%WriteToDisc (unit=luout)

  !Clean up memory.
  call file0%Erase()
  call filed%Erase()
  call fileh%Erase()
  call datam%Erase()
  call datax%Erase()

  contains

  !******************************************************************************************************************!
  !                                        PRIVATE PROCEDURE PARSECOMMANDLINE                                        !
  !******************************************************************************************************************!
  subroutine ParseCommandLine (file, kfrm, sidx, lhead)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: pl, F001
    implicit none

    character(len=*), intent(inout) :: file
    integer,          intent(inout) :: kfrm, sidx
    logical,          intent(inout) :: lhead

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'GeometryConvertXyzToDlpolyConfig::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    file  = 'geometry.xyz'
    kfrm  = 0
    sidx  = 1
    lhead = .true.
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

      else if ((buff=='-i') .or. (buff=='--starting-index')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,*,iostat=code) sidx
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-k') .or. (buff=='--format-key')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,*,iostat=code) kfrm
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-s') .or. (buff=='--skip-header')) then
        lhead = .false.
      end if
    end do

    !Check command-line arguments for conflicts.
    if (lhelp) call PrintHelp()
    if (ekey>0) call PrintHelp (ekey=ekey)
    if ((kfrm/=0) .and. (kfrm/=1) .and. (kfrm/=2)) call PrintHelp (ekey=9003)
    if (sidx<1) sidx = 1

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

    procname = 'GeometryConvertXyzToDlpolyConfig::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********       PROGRAM: GEOMETRY-CONVERT-XYZ-TO-DLPOLY-CONFIG       **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program converts an XYZ file to DLPOLY CONFIG format.                      '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: geometry-convert-xyz-to-dlpoly-config -fhiks                             '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -f     --file               Input XYZ file.                                '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/geometry.xyz                                    '
    nn = nn + 1 ; msgs(nn) = '     -h     --help               Print this help.                               '
    nn = nn + 1 ; msgs(nn) = '     -i     --starting-index     Starting atom index for CONFIG file. Reset     '
    nn = nn + 1 ; msgs(nn) = '                                 (without warning) if smaller than 1.           '
    nn = nn + 1 ; msgs(nn) = '                 Default = 1                                                    '
    nn = nn + 1 ; msgs(nn) = '     -k     --format-key         CONFIG file-format key.                        '
    nn = nn + 1 ; msgs(nn) = '                 = 0             coordinates only.                              '
    nn = nn + 1 ; msgs(nn) = '                 = 1             coordinates and velocities.                    '
    nn = nn + 1 ; msgs(nn) = '                 = 2             coordinates, velocities, and forces.           '
    nn = nn + 1 ; msgs(nn) = '                 Default = 0                                                    '
    nn = nn + 1 ; msgs(nn) = '     -s     --skip-header        Input file has no header.                      '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program GeometryConvertXyzToDlpolyConfig
!**********************************************************************************************************************!
