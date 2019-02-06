!**********************************************************************************************************************!
!Main program file for multiplying a VASP geometry.                                                                    !
!**********************************************************************************************************************!
program VaspBuildSupercellFromPoscar

  use Parameters,            only: dp, pl, sl, dtol, zero, one, luout
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use MemoryManagement,      only: ResizeArray
  use UtilityProcedures,     only: ConvertToLowerCase, ConvertToUpperCase
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: ConfigData, ConfigHeader, PoscarData, PoscarHeader, XyzData, XyzHeader,  &
                                   ConvertCoordinates, PeriodicBoxIsAligned
  implicit none

  character(len=pl)    :: path
  character(len=sl)    :: fout, mode
  integer              :: nimgx, nimgy, nimgz
  logical              :: lpbc
  integer, allocatable :: list(:)

  type(GenericTextFile)         :: file0
  type(GenericTextFile)         :: filed, fileh
  type(ConfigData)              :: datam
  type(ConfigHeader)            :: headm
  type(PoscarData)              :: datas, datase
  type(PoscarHeader)            :: heads, headst
  type(XyzData)                 :: datax
  type(XyzHeader)               :: headx
  integer                       :: nimgt
  integer                       :: idxi, idxf, idxie, idxfe
  integer                       :: ii, jj, kk, ll, nn
  character(len=2), allocatable :: typs(:), typse(:)
  real(dp),         allocatable :: dx(:), dy(:), dz(:)

  procname = 'VaspBuildSupercellFromPoscar::VaspBuildSupercellFromPoscar'
  if (ltrace) call PrintTrace()

  !Read command-line arguments.
  call ParseCommandLine  &
    (file=path, format=fout, mode=mode, nimgx=nimgx, nimgy=nimgy, nimgz=nimgz, lpbc=lpbc, list=list)

  !Read input file.
  call file0%ReadFromDisc (file=path)
  call file0%GetHead (head=fileh, numl=9)
  call heads%ReadFromFile (file=fileh)
  call file0%GetSection (esec=filed, numli=9+1, numlf=9+heads%numa)
  call datas%ReadFromFile (file=filed)

  !Verify valid inputs.
  if (.not. PeriodicBoxIsAligned(box=heads%box)) call PrintError (ekey=3031, lstop=.true.)
  nimgt       = nimgx * nimgy * nimgz
  datase%numa = datas%numa * nimgt
  if (allocated(list)) then
    if ((any(list<1)) .or. (any(list>datase%numa))) call PrintError (ekey=3011, lstop=.true.)
  end if

  !Extract coordinates in Cartesian format. Relocate to positive quadrants as required.
  call ConvertToUpperCase (line=heads%ctyp)
  call heads%Duplicate (dupl=headst)
  if (heads%ctyp=='CARTESIAN') then
    if (lpbc) call datas%ApplyPeriodicBoundary (dx=heads%box(1,1), dy=heads%box(2,2), dz=heads%box(3,3))
  else if (heads%ctyp=='DIRECT') then
    if (lpbc) call datas%ApplyPeriodicBoundary (dx=one, dy=one, dz=one)
    call ConvertCoordinates (mode='FRACTIONAL-TO-CARTESIAN', box=heads%box, gms=datas)
  else
    call PrintError (ekey=1304, lstop=.true., msg1='POSCAR: type of coordinates')
  end if

  !Extract atomic symbols in sentence case.
  call ResizeArray (nume=datas%numa, ida=typs)
  idxi = 0
  idxf = 0
  do ll = 1, heads%numt
    call ConvertToUpperCase (line=heads%typs(ll)(1:1))
    call ConvertToLowerCase (line=heads%typs(ll)(2:2))
    idxi = idxf + 1
    idxf = idxf + heads%nums(ll)
    typs(idxi:idxf) = heads%typs(ll)
  end do

  !Calculate coordinate shifts for unitcells in the supercell.
  call ResizeArray (nume=nimgt, ida=dx)
  call ResizeArray (nume=nimgt, ida=dy)
  call ResizeArray (nume=nimgt, ida=dz)
  nn = 0
  do kk = 1, nimgz
    do jj = 1, nimgy
      do ii = 1, nimgx
        nn = nn + 1
        dx(nn) = heads%box(1,1) * (ii-1)
        dy(nn) = heads%box(2,2) * (jj-1)
        dz(nn) = heads%box(3,3) * (kk-1)
      end do
    end do
  end do

  !Generate supercell.
  call datase%Resize()
  call ResizeArray (nume=datase%numa, ida=typse)
  idxi  = 0
  idxf  = 0
  idxie = 0
  idxfe = 0
  do ll = 1, heads%numt
    nn   = 0
    idxi = idxf + 1
    idxf = idxf + heads%nums(ll)
    do kk = 1, nimgz
      do jj = 1, nimgy
        do ii = 1, nimgx
          nn    = nn + 1
          idxie = idxfe + 1
          idxfe = idxfe + heads%nums(ll)
          typse(idxie:idxfe)     = typs(idxi:idxf)
          datase%cx(idxie:idxfe) = datas%cx(idxi:idxf) + dx(nn)
          datase%cy(idxie:idxfe) = datas%cy(idxi:idxf) + dy(nn)
          datase%cz(idxie:idxfe) = datas%cz(idxi:idxf) + dz(nn)
          datase%mx(idxie:idxfe) = datas%mx(idxi:idxf)
          datase%my(idxie:idxfe) = datas%my(idxi:idxf)
          datase%mz(idxie:idxfe) = datas%mz(idxi:idxf)
        end do
      end do
    end do
  end do

  !Extract specified atoms from supercell.
  if (allocated(list)) then
    datas%numa = size(list)
    call datas%Resize()
    call ResizeArray (nume=datas%numa, ida=typs)
    do ii = 1, datas%numa
      nn = list(ii)
      typs(ii)     = typse(nn)
      datas%cx(ii) = datase%cx(nn)
      datas%cy(ii) = datase%cy(nn)
      datas%cz(ii) = datase%cz(nn)
      datas%mx(ii) = datase%mx(nn)
      datas%my(ii) = datase%my(nn)
      datas%mz(ii) = datase%mz(nn)
    end do

    !Copy back extracted atoms ordered by type. Count atoms of each type.
    datase%numa = datas%numa
    call datase%Resize()
    call ResizeArray (nume=datase%numa, ida=typse)
    headst%nums = 0
    nn = 0
    do ll = 1, heads%numt
      do ii = 1, datas%numa
        if (typs(ii)==heads%typs(ll)) then
          nn = nn + 1
          typse(nn)       = typs(ii)
          datase%cx(nn)   = datas%cx(ii)
          datase%cy(nn)   = datas%cy(ii)
          datase%cz(nn)   = datas%cz(ii)
          datase%mx(nn)   = datas%mx(ii)
          datase%my(nn)   = datas%my(ii)
          datase%mz(nn)   = datas%mz(ii)
          headst%nums(ll) = headst%nums(ll) + 1
        end if
      end do
    end do
  end if

  !Update count of each atom type in header.
  if (allocated(list)) then
    nn = 0
    do ll = 1, headst%numt
      if (headst%nums(ll)>0) nn = nn + 1
    end do
    heads%numt = nn
    call heads%Resize()
    nn = 0
    do ll = 1, headst%numt
      if (headst%nums(ll)>0) then
        nn = nn + 1
        heads%typs(nn) = headst%typs(ll)
        heads%nums(nn) = headst%nums(ll)
      end if
    end do
  else
    heads%nums = heads%nums * nimgt
  end if
  heads%numa = sum(heads%nums)
  if (heads%numa/=datase%numa) call PrintError (ekey=3203, lstop=.true.)

  !Extract header section in specified format.
  if ((fout=='CONFIG') .or. (fout=='DLPOLY')) then
    headm%name = heads%name
    headm%kfrm = 0
    headm%numa = heads%numa
    headm%box(1,1) = heads%box(1,1) * nimgx
    headm%box(2,2) = heads%box(2,2) * nimgy
    headm%box(3,3) = heads%box(3,3) * nimgz
    if ((abs(headm%box(1,1)-headm%box(2,2))<dtol) .and. ((abs(headm%box(1,1)-headm%box(3,3))<dtol))) then
      headm%kpbc = 1
    else
      headm%kpbc = 2
    end if
  else if ((fout=='POSCAR') .or. (fout=='VASP')) then
    heads%box(1,1) = heads%box(1,1) * nimgx
    heads%box(2,2) = heads%box(2,2) * nimgy
    heads%box(3,3) = heads%box(3,3) * nimgz
    if (mode=='CARTESIAN') then
      heads%ctyp = 'Cartesian'
    else if (mode=='FRACTIONAL') then
      heads%ctyp = 'Direct'
    end if
  else if (fout=='XYZ') then
    headx%name = heads%name
    headx%numa = heads%numa
  end if

  !Extract data section in specified format.
  if ((fout=='CONFIG') .or. (fout=='DLPOLY')) then
    datam%numa = datase%numa
    call datam%Resize()
    datam%type = typse
    datam%cx   = datase%cx
    datam%cy   = datase%cy
    datam%cz   = datase%cz
    do ii = 1, datam%numa
      datam%idx(ii) = ii
    end do
  else if ((fout=='POSCAR') .or. (fout=='VASP')) then
    if ((mode=='FRACTIONAL') .or. ((mode=='UNCHANGED') .and. (headst%ctyp=='DIRECT')))  &
      call ConvertCoordinates (mode='CARTESIAN-TO-FRACTIONAL', box=heads%box, gms=datase)
  else if (fout=='XYZ') then
    datax%numa = datase%numa
    call datax%Resize()
    datax%type = typse
    datax%cx   = datase%cx
    datax%cy   = datase%cy
    datax%cz   = datase%cz
  end if

  !Write output file in specified format.
  if ((fout=='CONFIG') .or. (fout=='DLPOLY')) then
    call headm%WriteToFile (file=fileh)
    call datam%WriteToFile (file=filed, kfrm=headm%kfrm)
  else if ((fout=='POSCAR') .or. (fout=='VASP')) then
    call heads%WriteToFile (file=fileh)
    call datase%WriteToFile (file=filed)
  else if (fout=='XYZ') then
    call headx%WriteToFile (file=fileh)
    call datax%WriteToFile (file=filed)
  end if
  call fileh%WriteToDisc (unit=luout)
  call filed%WriteToDisc (unit=luout)

  !Clean up memory.
  call file0%Erase()
  call filed%Erase()
  call fileh%Erase()
  call datam%Erase()
  call datas%Erase()
  call datase%Erase()
  call heads%Erase()
  call headst%Erase()
  call datax%Erase()
  call ResizeArray (nume=0, ida=list)
  call ResizeArray (nume=0, ida=typs)
  call ResizeArray (nume=0, ida=typse)
  call ResizeArray (nume=0, ida=dx)
  call ResizeArray (nume=0, ida=dy)
  call ResizeArray (nume=0, ida=dz)

  contains

  !******************************************************************************************************************!
  !                                        PRIVATE PROCEDURE PARSECOMMANDLINE                                        !
  !******************************************************************************************************************!
  subroutine ParseCommandLine (file, format, mode, nimgx, nimgy, nimgz, lpbc, list)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: pl, F001
    use UtilityProcedures, only: ConvertToUpperCase, ExtractNumbersFromString
    implicit none

    character(len=*),     intent(inout) :: file, format, mode
    integer,              intent(inout) :: nimgx, nimgy, nimgz
    logical,              intent(inout) :: lpbc
    integer, allocatable, intent(inout) :: list(:)

    character(len=pl) :: buff
    integer           :: ii, nn, code, ekey
    logical           :: lhelp

    procname = 'VaspBuildSupercellFromPoscar::ParseCommandLine'
    if (ltrace) call PrintTrace()

    !Set defaults for all arguments.
    file   = 'POSCAR'
    format = 'POSCAR'
    mode   = 'UNCHANGED'
    nimgx  = 1
    nimgy  = 1
    nimgz  = 1
    lpbc   = .false.
    lhelp  = .false.

    !Read command-line arguments and update corresponding variables.
    nn = IARGC()
    ekey = 0
    do ii = 1, nn
      call GETARG (ii, buff)
      buff = adjustl(buff)

      if ((buff=='-e') .or. (buff=='--extract-atoms')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          call ExtractNumbersFromString (line=buff, list=list)
        else
          ekey = 9002
        end if

      else if ((buff=='-f') .or. (buff=='--file')) then
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

      else if ((buff=='-m') .or. (buff=='--mode')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) mode
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-o') .or. (buff=='--output-file-format')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,F001,iostat=code) format
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-p') .or. (buff=='--apply-periodic-boundary')) then
        lpbc = .false.

      else if ((buff=='-X') .or. (buff=='--number-of-x-units')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,*,iostat=code) nimgx
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-Y') .or. (buff=='--number-of-y-units')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,*,iostat=code) nimgy
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if

      else if ((buff=='-Z') .or. (buff=='--number-of-y-units')) then
        if (ii+1<=nn) then
          call GETARG (ii+1, buff)
          buff = adjustl(buff)
          read (buff,*,iostat=code) nimgz
            if (code>0) ekey = 9011
        else
          ekey = 9002
        end if
      end if
    end do

    !Check command-line arguments for conflicts.
    if (lhelp) call PrintHelp()
    if (ekey>0) call PrintHelp (ekey=ekey)
    call ConvertToUpperCase (line=format)
    if ((format=='POSCAR') .or. (format=='VASP')) then
      call ConvertToUpperCase (line=mode)
      if ((mode/='CARTESIAN') .and. (mode/='FRACTIONAL') .and. (mode/='UNCHANGED')) call PrintHelp (ekey=9003)
    else if ((format=='CONFIG') .or. (format=='DLPOLY') .or. (format=='XYZ')) then
      mode = ' '
    else
      call PrintHelp (ekey=9003)
    end if
    if (nimgx<1) nimgx = 1
    if (nimgy<1) nimgy = 1
    if (nimgz<1) nimgz = 1

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

    procname = 'VaspBuildSupercellFromPoscar::PrintHelp'
    if (ltrace) call PrintTrace()

    !Setup the help message.
    nn = 0
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '**********         PROGRAM: VASP-BUILD-SUPERCELL-FROM-POSCAR          **********'
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'This program builds a supercell from VASP POSCAR file.                          '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = 'USAGE: vasp-build-supercell-from-poscar -efhmopXYZ                              '
    nn = nn + 1 ; msgs(nn) = '                                                                                '
    nn = nn + 1 ; msgs(nn) = '     -e     --extract-atoms               (optional) Extract specified atoms    '
    nn = nn + 1 ; msgs(nn) = '                                          (comma-separated list) from the       '
    nn = nn + 1 ; msgs(nn) = '                                          expanded supercell. A range of indices'
    nn = nn + 1 ; msgs(nn) = '                                          may be provided in terms of initial   '
    nn = nn + 1 ; msgs(nn) = '                                          and final indices separated by a dash.'
    nn = nn + 1 ; msgs(nn) = '                                          Atoms in the expanded supercell are   '
    nn = nn + 1 ; msgs(nn) = '                                          ordered by type (as in POSCAR) with   '
    nn = nn + 1 ; msgs(nn) = '                                          unitcells ordered in X --> Y --> Z    '
    nn = nn + 1 ; msgs(nn) = '                                          sequence.                             '
    nn = nn + 1 ; msgs(nn) = '     -f     --file                        VASP POSCAR file.                     '
    nn = nn + 1 ; msgs(nn) = '                 Default = $PWD/POSCAR                                          '
    nn = nn + 1 ; msgs(nn) = '     -h     --help                        Print this help.                      '
    nn = nn + 1 ; msgs(nn) = '     -m     --mode                        Format of output coordinates (for     '
    nn = nn + 1 ; msgs(nn) = '                                          output (-o) in POSCAR/VASP format.    '
    nn = nn + 1 ; msgs(nn) = '                                          Ignored (without warning) for output  '
    nn = nn + 1 ; msgs(nn) = '                                          in CONFIG/DLPOLY, and XYZ formats.    '
    nn = nn + 1 ; msgs(nn) = '                                          (CARTESIAN/FRACTIONAL/UNCHANGED)      '
    nn = nn + 1 ; msgs(nn) = '                 Default = UNCHANGED                                            '
    nn = nn + 1 ; msgs(nn) = '     -o     --output-file-format          Format of output file.                '
    nn = nn + 1 ; msgs(nn) = '                                          (CONFIG/DLPOLY/POSCAR/VASP/XYZ)       '
    nn = nn + 1 ; msgs(nn) = '                 Default = POSCAR                                               '
    nn = nn + 1 ; msgs(nn) = '     -p     --apply-periodic-boundary     Relocate all coordinates to positive  '
    nn = nn + 1 ; msgs(nn) = '                                          quadrants.                            '
    nn = nn + 1 ; msgs(nn) = '     -X     --number-of-x-units           Number of times the unitcell should be'
    nn = nn + 1 ; msgs(nn) = '                                          multiplied in X-direction. Reset      '
    nn = nn + 1 ; msgs(nn) = '                                          (without warning) if smaller than 1.  '
    nn = nn + 1 ; msgs(nn) = '                 Default = 1                                                    '
    nn = nn + 1 ; msgs(nn) = '     -Y     --number-of-y-units           Number of times the unitcell should be'
    nn = nn + 1 ; msgs(nn) = '                                          multiplied in Y-direction. Reset      '
    nn = nn + 1 ; msgs(nn) = '                                          (without warning) if smaller than 1.  '
    nn = nn + 1 ; msgs(nn) = '                 Default = 1                                                    '
    nn = nn + 1 ; msgs(nn) = '     -Z     --number-of-z-units           Number of times the unitcell should be'
    nn = nn + 1 ; msgs(nn) = '                                          multiplied in Z-direction. Reset      '
    nn = nn + 1 ; msgs(nn) = '                                          (without warning) if smaller than 1.  '
    nn = nn + 1 ; msgs(nn) = '                 Default = 1                                                    '
    nn = nn + 1 ; msgs(nn) = '                                                                                '

    !Print help and terminate the program.
    if (present(ekey)) call PrintError (ekey=ekey, lstop=.false.)
    do ii = 1, nn
      write (luerr,F001) trim(msgs(ii))
    end do
    stop

  end subroutine PrintHelp
  !******************************************************************************************************************!

end program VaspBuildSupercellFromPoscar
!**********************************************************************************************************************!
