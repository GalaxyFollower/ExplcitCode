!**********************************************************************************************************************!
!This module provides procedures for extracting useful information from files.                                         !
!**********************************************************************************************************************!
module FileParsers

  implicit none

  !******************************************************************************************************************!
  !                                               CONTAINED PROCEDURES                                               !
  !                                                                                                                  !
  !'RiApproximationIsUsed' returns true if $rij keyword is found in TURBOMOLE control file.                          !
  !'ExtractTurbomoleEnergy' reads converged SCF energy from TURBOMOLE energy file.                                   !
  !'ExtractTurbomoleGradients' reads gradients from TURBOMOLE gradients file.                                        !
  !'ExtractTurbomoleCharges' reads atomic charges from a TURBOMOLE output file.                                      !
  !'ReplaceTurbomoleEnergy' replaces converged SCF energy in TURBOMOLE energy file.                                  !
  !'ReplaceTurbomoleGradients' replaces gradients in TURBOMOLE gradients file.                                       !
  !'ExtractVaspGeometry' reads atomic coordinates from VASP OUTCAR file.                                             !
  !'ExtractVaspEnergy' reads converged SCF energy from VASP OUTCAR file.                                             !
  !'ExtractVaspGradients' reads gradients from VASP OUTCAR file.                                                     !
  !'ExtractDlpolyEnergy' reads DLPOLY energy(ies) from MMS_ENERGY, MMS_REPLAY, or MMS_STEP file (overloaded).        !
  !'ExtractDlpolyGradients' reads DLPOLY gradients from MMS_REPLAY or MMS_STEP file (overloaded).                    !
  !'WriteDlpolyEnergy' writes MMS_ENERGY file.                                                                       !
  !'WriteDlpolyGradients' writes MMS_GRADIENTS file.                                                                 !
  !'WriteSmssEnsemble' writes MMS_ENSEMBLE file.                                                                     !
  !'ReadSmssEnsemble' parses MMS_ENSEMBLE file.                                                                      !
  !******************************************************************************************************************!

  interface RiApproximationIsUsed
    module procedure RiApproximationIsUsed_gtf_l
  end interface RiApproximationIsUsed

  interface ExtractTurbomoleEnergy
    module procedure ExtractTurbomoleEnergy_dpgtfl
  end interface ExtractTurbomoleEnergy

  interface ExtractTurbomoleGradients
    module procedure ExtractTurbomoleGradients_gtftcd
  end interface ExtractTurbomoleGradients

  interface ExtractTurbomoleCharges
    module procedure ExtractTurbomoleCharges_sgtfsg
  end interface ExtractTurbomoleCharges

  interface ReplaceTurbomoleEnergy
    module procedure ReplaceTurbomoleEnergy_dpgtf
  end interface ReplaceTurbomoleEnergy

  interface ReplaceTurbomoleGradients
    module procedure ReplaceTurbomoleGradients_gtftcd
  end interface ReplaceTurbomoleGradients

  interface ExtractVaspEnergy
    module procedure ExtractVaspEnergy_dpgtf
  end interface ExtractVaspEnergy

  interface ExtractVaspGeometry
    module procedure ExtractVaspGeometry_gtfvpd
  end interface ExtractVaspGeometry

  interface ExtractVaspGradients
    module procedure ExtractVaspGradients_gtfvpd
  end interface ExtractVaspGradients

  interface ExtractDlpolyEnergy
    module procedure ExtractDlpolyEnergy_dpdpalgtf
    module procedure ExtractDlpolyEnergy_dpX2gtf
  end interface ExtractDlpolyEnergy

  interface ExtractDlpolyGradients
    module procedure ExtractDlpolyGradients_dpdpaX2gtftcdX2
    module procedure ExtractDlpolyGradients_dpgtftcd
  end interface ExtractDlpolyGradients

  interface WriteDlpolyEnergy
    module procedure WriteDlpolyEnergy_dpdpagtf
  end interface WriteDlpolyEnergy

  interface WriteDlpolyGradients
    module procedure WriteDlpolyGradients_dpgtftcd
  end interface WriteDlpolyGradients

  interface WriteSmssEnsemble
    module procedure WriteSmssEnsemble_gtfsg
  end interface WriteSmssEnsemble

  interface ReadSmssEnsemble
    module procedure ReadSmssEnsemble_gtfsg
  end interface ReadSmssEnsemble

  contains

  !******************************************************************************************************************!
  !                                         INTERFACE RIAPPROXIMATIONISUSED                                          !
  !******************************************************************************************************************!
  function RiApproximationIsUsed_gtf_l (file) result (lrij)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: sl
    use GenericInputOutput, only: GenericTextFile
    implicit none

    type(GenericTextFile), intent(in) :: file
    logical                           :: lrij

    character(len=sl) :: buff
    integer           :: ii

    procname = 'FileParsers::RiApproximationIsUsed_gtf_l'
    if (ltrace) call PrintTrace()

    !Search for $rij keyword.
    if ((.not. file%ksiz) .or. (file%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    lrij = .false.
    do ii = 1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:4)=='$rij') lrij = .true.
    end do

  end function RiApproximationIsUsed_gtf_l
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                         INTERFACE EXTRACTTURBOMOLEENERGY                                         !
  !******************************************************************************************************************!
  subroutine ExtractTurbomoleEnergy_dpgtfl (file, scfe, lsum)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: dp, sl
    use GenericInputOutput, only: GenericTextFile
    implicit none

    type(GenericTextFile), intent(in)    :: file
    real(dp),              intent(inout) :: scfe     !Energy for QM cluster (Hartree).
    logical,               intent(in)    :: lsum     !Energy already extracted by TURBOMOLE?

    character(len=sl) :: buff
    integer           :: ii, jj, aa, ncyc, code

    procname = 'FileParsers::ExtractTurbomoleEnergy_dpgtfl'
    if (ltrace) call PrintTrace()

    if ((.not. file%ksiz) .or. (file%numl<1)) call PrintError (ekey=1103, lstop=.true.)

    !If the energy has already been extracted by TURBOMOLE, just locate $energy section and read the last cycle.
    if (lsum) then
      jj = 0
      do ii = 1, file%numl
        buff = adjustl(file%line(ii))
        if (buff(1:7)=='$energy') jj = ii
      end do
      if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: TURBOMOLE energy')

      !Count number of energy cycles in this section.
      ncyc = 0
      do ii = jj+1, file%numl
        buff = adjustl(file%line(ii))
        if (buff(1:1)=='$') exit
        ncyc = ncyc + 1
      end do
      if (ncyc==0) call PrintError (ekey=4002, lstop=.true., msg1='Section: TURBOMOLE energy')

      !Read converged SCF energy from last cycle (ignore step counter).
      jj = jj + ncyc
      read (file%line(jj),*,iostat=code) aa, scfe
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: TURBOMOLE energy')

      !As a check, this should be the last line.
      jj = jj + 1
      if ((jj/=file%numl) .or. (file%line(jj)/='$end')) call PrintError (ekey=1103, lstop=.true.)

    !For a complete output file, locate last converged SCF energy and extract the value.
    else
      jj = 0
      do ii = 1, file%numl
        buff = adjustl(file%line(ii))
        if (buff(1:22)=='|  total energy      =') jj = ii
      end do
      if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: TURBOMOLE energy')
      buff = adjustl(file%line(jj))
      jj = 0
      jj = index (buff, '=', .true.)
        if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
      buff (1:jj) = ' '
      buff = adjustl(buff)
      read (buff,*,iostat=code) scfe
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: TURBOMOLE energy')
    end if

  end subroutine ExtractTurbomoleEnergy_dpgtfl
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                       INTERFACE EXTRACTTURBOMOLEGRADIENTS                                        !
  !******************************************************************************************************************!
  subroutine ExtractTurbomoleGradients_gtftcd (file, gmc)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: sl
    use GenericInputOutput,    only: GenericTextFile
    use CoordinateFileFormats, only: CoordData
    implicit none

    type(GenericTextFile), intent(in)    :: file
    type(CoordData),       intent(inout) :: gmc      !QM cluster (gradients in a.u.).

    character(len=sl) :: buff
    integer           :: ii, jj, code

    procname = 'FileParsers::ExtractTurbomoleGradients_gtftcd'
    if (ltrace) call PrintTrace()

    !Locate last cycle of coordinates/gradients.
    if ((.not. file%ksiz) .or. (file%numl<1) .or.  &
        (.not. gmc%ksiz)  .or. (gmc%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    jj = 0
    do ii = 1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:5)=='cycle') jj = ii
    end do
    if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: cycle (TURBOMOLE gradients)')

    !Skip coordinates and extract gradients.
    jj = jj + gmc%numa
    if (jj+gmc%numa>file%numl) call PrintError (ekey=4002, lstop=.true., msg1='Section: TURBOMOLE gradients')
    do ii = 1, gmc%numa
      read (file%line(jj+ii),*,iostat=code) gmc%fx(ii), gmc%fy(ii), gmc%fz(ii)
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: TURBOMOLE gradients')
    end do

    !As a check, this should be the last line.
    jj = jj + gmc%numa + 1
    if ((jj/=file%numl) .or. (file%line(jj)/='$end')) call PrintError (ekey=1103, lstop=.true.)

  end subroutine ExtractTurbomoleGradients_gtftcd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                        INTERFACE EXTRACTTURBOMOLECHARGES                                         !
  !******************************************************************************************************************!
  subroutine ExtractTurbomoleCharges_sgtfsg (file, gmi, mode)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: dp, sl
    use UtilityProcedures,  only: ConvertToUpperCase
    use GenericInputOutput, only: GenericTextFile
    use SolvationScheme,    only: SmssGeometry
    implicit none

    type(GenericTextFile),      intent(in)    :: file
    type(SmssGeometry),         intent(inout) :: gmi      !SMSS geometry (point charges).
    character(len=*), optional, intent(in)    :: mode     !Type of atomic charges (Mullikan/Natural)?

    character(len=sl) :: buff
    character(len=sl) :: tmode = 'NATURAL'     !Default are 'NATURAL' charges.
    integer           :: ii, jj, code
    real(dp)          :: aa

    procname = 'FileParsers::ExtractTurbomoleCharges_sgtfsg'
    if (ltrace) call PrintTrace()

    !Override default type of charges?
    if (present(mode)) then
      if (len(tmode)<len(mode)) call PrintError (ekey=1122, lstop=.true.)
      tmode = adjustl(mode)
      call ConvertToUpperCase (line=tmode)
      if ((tmode/='ESPFIT') .and. (tmode/='MULLIKAN') .and. (tmode/='NATURAL'))  &
        call PrintError (ekey=1302, lstop=.true., msg1='Keyword: mode')
    end if

    !Search for last cycle of atomic charges. Skip extra header lines (ESPFit:16, Mullikan:7, Natural:4).
    if ((.not. file%ksiz) .or. (file%numl<1) .or.  &
        (.not. gmi%ksiz)  .or. (gmi%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    if (tmode=='ESPFIT') then
      jj = 0
      do ii = 1, file%numl
        buff = adjustl(file%line(ii))
        if (buff(1:51)=='fit of point charges due to electrostatic potential') jj = ii
      end do
      if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: ESP fitted charges')
      jj = jj + 16
      if (jj+gmi%numa>file%numl) call PrintError (ekey=4002, lstop=.true., msg1='Section: ESP fitted charges')

    else if (tmode=='MULLIKAN') then
      jj = 0
      do ii = 1, file%numl
        buff = adjustl(file%line(ii))
        if (buff(1:18)=='BRUTTO POPULATIONS') jj = ii
      end do
      if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: Brutto Populations')
      jj = jj + 7
      if (jj+gmi%numa>file%numl) call PrintError (ekey=4002, lstop=.true., msg1='Section: Brutto Populations')

    else if (tmode=='NATURAL') then
      jj = 0
      do ii = 1, file%numl
        buff = adjustl(file%line(ii))
        if (buff(1:39)=='Summary of Natural Population Analysis:') jj = ii
      end do
      if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: Natural Populations')
      jj = jj + 4
      if (jj+gmi%numa>file%numl) call PrintError (ekey=4002, lstop=.true., msg1='Section: Natural Populations')
    end if

    !Extract atomic charges.
    if (tmode=='ESPFIT') then
      do ii = 1, gmi%numa
        buff = file%line(jj+ii)
        buff(1:9) = ' '
        read (buff,*,iostat=code) aa, gmi%pc(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: TURBOMOLE charges')
      end do

    else if ((tmode=='MULLIKAN') .or. (tmode=='NATURAL')) then
      do ii = 1, gmi%numa
        buff = file%line(jj+ii)
        buff(1:9) = ' '
        read (buff,*,iostat=code) gmi%pc(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: TURBOMOLE charges')
      end do
    end if

  end subroutine ExtractTurbomoleCharges_sgtfsg
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                         INTERFACE REPLACETURBOMOLEENERGY                                         !
  !******************************************************************************************************************!
  subroutine ReplaceTurbomoleEnergy_dpgtf (file, scfe)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: dp, sl, zero, F104
    use GenericInputOutput, only: GenericTextFile
    implicit none

    type(GenericTextFile), intent(inout) :: file
    real(dp),              intent(in)    :: scfe     !Energy for ISMSS / ESMSS (Hartree).

    character(len=sl) :: buff
    integer           :: ii, jj, ncyc, code

    procname = 'FileParsers::ReplaceTurbomoleEnergy_dpgtf'
    if (ltrace) call PrintTrace()

    !Locate $energy section.
    if ((.not. file%ksiz) .or. (file%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    jj = 0
    do ii = 1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:7)=='$energy') jj = ii
    end do
    if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: TURBOMOLE energy')

    !Count number of energy cycles in this section.
    ncyc = 0
    do ii = jj+1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:1)=='$') exit
      ncyc = ncyc + 1
    end do
    if (ncyc==0) call PrintError (ekey=4002, lstop=.true., msg1='Section: TURBOMOLE energy')

    !Replace converged SCF energy of last cycle.
    jj = jj + ncyc
    read (file%line(jj),*,iostat=code) ii
      if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: TURBOMOLE energy')
    write (file%line(jj),F104,iostat=code) ii, scfe, zero, zero
      if (code>0) call PrintError (ekey=2121, lstop=.true., msg1='Section: TURBOMOLE energy')

    !As a check, this should be the last line.
    jj = jj + 1
    if ((jj/=file%numl) .or. (file%line(jj)/='$end')) call PrintError (ekey=1103, lstop=.true.)

  end subroutine ReplaceTurbomoleEnergy_dpgtf
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                       INTERFACE REPLACETURBOMOLEGRADIENTS                                        !
  !******************************************************************************************************************!
  subroutine ReplaceTurbomoleGradients_gtftcd (file, gmc)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: sl, F232
    use GenericInputOutput,    only: GenericTextFile
    use CoordinateFileFormats, only: CoordData
    implicit none

    type(GenericTextFile), intent(inout) :: file
    type(CoordData),       intent(in)    :: gmc      !SMSS geometry (gradients in a.u.).

    character(len=sl) :: buff
    integer           :: ii, jj, code

    procname = 'FileParsers::ReplaceTurbomoleGradients_gtftcd'
    if (ltrace) call PrintTrace()

    !Locate last cycle of coordinates/gradients.
    if ((.not. file%ksiz) .or. (file%numl<1) .or.  &
        (.not. gmc%ksiz)  .or. (gmc%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    jj = 0
    do ii = 1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:5)=='cycle') jj = ii
    end do
    if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: cycle (TURBOMOLE gradients)')

    !Skip coordinates and replace gradients.
    jj = jj + gmc%numa
    if (jj+gmc%numa>file%numl) call PrintError (ekey=4002, lstop=.true., msg1='Section: TURBOMOLE gradients')
    do ii = 1, gmc%numa
      write (file%line(jj+ii),F232,iostat=code) gmc%fx(ii), gmc%fy(ii), gmc%fz(ii)
        if (code>0) call PrintError (ekey=2121, lstop=.true., msg1='Section: TURBOMOLE gradients')
    end do

    !As a check, this should be the last line.
    jj = jj + gmc%numa + 1
    if ((jj/=file%numl) .or. (file%line(jj)/='$end')) call PrintError (ekey=1103, lstop=.true.)

  end subroutine ReplaceTurbomoleGradients_gtftcd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                           INTERFACE EXTRACTVASPENERGY                                            !
  !******************************************************************************************************************!
  subroutine ExtractVaspEnergy_dpgtf (file, scfe)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: dp, sl
    use GenericInputOutput, only: GenericTextFile
    implicit none

    type(GenericTextFile), intent(in)    :: file
    real(dp),              intent(inout) :: scfe     !Energy for QM surface (eV).

    character(len=sl) :: buff
    integer           :: ii, jj, code

    procname = 'FileParsers::ExtractVaspEnergy_dpgtf'
    if (ltrace) call PrintTrace()

    !Locate last converged SCF energy.
    if ((.not. file%ksiz) .or. (file%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    jj = 0
    do ii = 1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:24)=='energy  without entropy=') jj = ii
    end do
    if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: VASP energy')

    !Extract the energy value.
    buff = adjustl(file%line(jj))
    jj = 0
    jj = index (buff, '=', .true.)
      if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
    buff (1:jj) = ' '
    buff = adjustl(buff)
    read (buff,*,iostat=code) scfe
      if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: VASP energy')

  end subroutine ExtractVaspEnergy_dpgtf
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                          INTERFACE EXTRACTVASPGEOMETRY                                           !
  !******************************************************************************************************************!
  subroutine ExtractVaspGeometry_gtfvpd (file, gms)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: sl
    use GenericInputOutput,    only: GenericTextFile
    use CoordinateFileFormats, only: PoscarData
    implicit none

    type(GenericTextFile), intent(in)    :: file
    type(PoscarData),      intent(inout) :: gms      !QM surface (coordinates in Angstrom).

    character(len=sl) :: buff
    integer           :: ii, jj, code

    procname = 'FileParsers::ExtractVaspGeometry_gtfvpd'
    if (ltrace) call PrintTrace()

    !Locate last cycle of coordinates/gradients.
    if ((.not. file%ksiz) .or. (file%numl<1) .or.  &
        (.not. gms%ksiz)  .or. (gms%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    jj = 0
    do ii = 1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:8)=='POSITION') then
        buff(1:8) = ' '
        buff = adjustl(buff)
        if (buff(1:22)=='TOTAL-FORCE (eV/Angst)') jj = ii
      end if
    end do
    if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: VASP coordinates')

    !Skip 1 extra header line and extract coordinates.
    jj = jj + 1
    if (jj+gms%numa>file%numl) call PrintError (ekey=4002, lstop=.true., msg1='Section: VASP coordinates')
    do ii = 1, gms%numa
      read (file%line(jj+ii),*,iostat=code) gms%cx(ii), gms%cy(ii), gms%cz(ii)
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: VASP coordinates')
    end do

  end subroutine ExtractVaspGeometry_gtfvpd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                          INTERFACE EXTRACTVASPGRADIENTS                                          !
  !******************************************************************************************************************!
  subroutine ExtractVaspGradients_gtfvpd (file, gms)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: dp, sl
    use GenericInputOutput,    only: GenericTextFile
    use CoordinateFileFormats, only: PoscarData
    implicit none

    type(GenericTextFile), intent(in)    :: file
    type(PoscarData),      intent(inout) :: gms      !QM surface (gradients in eV/Angstrom).

    character(len=sl) :: buff
    integer           :: ii, jj, code
    real(dp)          :: aa, bb, cc

    procname = 'FileParsers::ExtractVaspGradients_gtfvpd'
    if (ltrace) call PrintTrace()

    !Locate last cycle of coordinates/gradients.
    if ((.not. file%ksiz) .or. (file%numl<1) .or.  &
        (.not. gms%ksiz)  .or. (gms%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    jj = 0
    do ii = 1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:8)=='POSITION') then
        buff(1:8) = ' '
        buff = adjustl(buff)
        if (buff(1:22)=='TOTAL-FORCE (eV/Angst)') jj = ii
      end if
    end do
    if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: VASP gradients')

    !Skip 1 extra header line and extract forces (ignore coordinates).
    jj = jj + 1
    if (jj+gms%numa>file%numl) call PrintError (ekey=4002, lstop=.true., msg1='Section: VASP gradients')
    do ii = 1, gms%numa
      read (file%line(jj+ii),*,iostat=code) aa, bb, cc, gms%fx(ii), gms%fy(ii), gms%fz(ii)
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: VASP gradients')
    end do

    !Change sign (forces -> gradients).
    gms%fx = -gms%fx
    gms%fy = -gms%fy
    gms%fz = -gms%fz

  end subroutine ExtractVaspGradients_gtfvpd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                          INTERFACE EXTRACTDLPOLYENERGY                                           !
  !******************************************************************************************************************!
  subroutine ExtractDlpolyEnergy_dpdpalgtf (file, tk, scfei, lnew)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: dp, sl, stol, zero
    use MemoryManagement,   only: ResizeArray
    use GenericInputOutput, only: GenericTextFile
    implicit none

    type(GenericTextFile), intent(in)    :: file
    real(dp),              intent(inout) :: tk           !Temperature (Kelvin).
    real(dp), allocatable, intent(inout) :: scfei(:)     !Energies for MM system frames (Hartree).
    logical,               intent(in)    :: lnew         !Energies already extracted?

    character(len=sl) :: buff
    integer           :: ii, jj, nn, aa, code
    real(dp)          :: tk1, tkn                 !Temperature (first/frame).
    real(dp)          :: emi(1:1000000)           !Energies for all frames.

    procname = 'FileParsers::ExtractDlpolyEnergy_dpdpalgtf'
    if (ltrace) call PrintTrace()

    if ((.not. file%ksiz) .or. (file%numl<3)) call PrintError (ekey=1103, lstop=.true.)

    !For a new file, verify that all energies are calculated at same temperature and extract them in a temporary
    !array.
    if (lnew) then
      nn = 0
      do ii = 1, file%numl
        !Locate next frame. Skip frame index on line 1.
        buff = adjustl(file%line(ii))
        if (buff(1:10)=='$MMS_FRAME') then
          if (ii+4>file%numl) call PrintError (ekey=4002, lstop=.true., msg1='Section: DLPOLY frame')
          nn = nn + 1

          !Extract temperature from line 2.
          buff = adjustl(file%line(ii+1))
          if (buff(1:12)/='$temperature')  &
            call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY temperature')
          jj = 0
          jj = index (buff, '=', .true.)
            if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
          buff(1:jj) = ' '
          read (buff,*,iostat=code) tkn
            if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: DLPOLY temperature')
          if (nn==1) tk1 = tkn
          if (abs(tkn-tk1)>stol) call PrintError (ekey=3041, lstop=.true., ikey=nn)

          !Extract energy from line 3.
          buff = adjustl(file%line(ii+2))
          if (buff(1:7)/='$energy') call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY energy')
          jj = 0
          jj = index (buff, '=', .true.)
            if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
          buff(1:jj) = ' '
          read (buff,*,iostat=code) emi(nn)
            if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: DLPOLY energy')
        end if
      end do

      !Resize output array and copy energies from temporary array.
      if (nn==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY frame')
      call ResizeArray (nume=nn, ida=scfei)
      scfei(1:nn) = emi(1:nn)
      tk = tk1

    !If the energies have already been extracted/corrected, just read them.
    else
      call ResizeArray (nume=file%numl-2, ida=scfei)
      scfei = zero

      !Extract temperature from line 1.
      buff = adjustl(file%line(1))
      if (buff(1:12)/='$temperature')  &
        call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY temperature')
      jj = 0
      jj = index (buff, '=', .true.)
        if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
      buff(1:jj) = ' '
      read (buff,*,iostat=code) tk
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: DLPOLY temperature')

      !Skip line 2 (keyword) and extract energies.
      buff = adjustl(file%line(2))
      if (buff(1:7)/='$energy') call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY energy')
      do ii = 1, size(scfei)
        read (file%line(ii+2),*,iostat=code) aa, scfei(ii)
          if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: DLPOLY energy')
      end do
    end if

  end subroutine ExtractDlpolyEnergy_dpdpalgtf

  subroutine ExtractDlpolyEnergy_dpX2gtf (file, tk, scfe)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: dp, sl
    use GenericInputOutput, only: GenericTextFile
    implicit none

    type(GenericTextFile), intent(in)    :: file
    real(dp),              intent(inout) :: tk       !Temperature (Kelvin).
    real(dp),              intent(inout) :: scfe     !Energy for MM system (Hartree).

    character(len=sl) :: buff
    integer           :: jj, code

    procname = 'FileParsers::ExtractDlpolyEnergy_dpX2gtf'
    if (ltrace) call PrintTrace()

    !Skip frame index on line 1.
    if ((.not. file%ksiz) .or. (file%numl<5)) call PrintError (ekey=1103, lstop=.true.)
    buff = adjustl(file%line(1))
    if (buff(1:10)/='$MMS_FRAME')  &
      call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY frame')

    !Extract temperature from line 2.
    buff = adjustl(file%line(2))
    if (buff(1:12)/='$temperature')  &
      call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY temperature')
    jj = 0
    jj = index (buff, '=', .true.)
      if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
    buff(1:jj) = ' '
    read (buff,*,iostat=code) tk
      if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: DLPOLY temperature')

    !Extract energy from line 3.
    buff = adjustl(file%line(3))
    if (buff(1:7)/='$energy') call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY energy')
    jj = 0
    jj = index (buff, '=', .true.)
      if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
    buff (1:jj) = ' '
    buff = adjustl(buff)
    read (buff,*,iostat=code) scfe
      if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: DLPOLY energy')

  end subroutine ExtractDlpolyEnergy_dpX2gtf
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                         INTERFACE EXTRACTDLPOLYGRADIENTS                                         !
  !******************************************************************************************************************!
  subroutine ExtractDlpolyGradients_dpdpaX2gtftcdX2 (file, tk, emin, emio, gmme, gmmi)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: dp, sl, stol, zero, one, BZHoK
    use MemoryManagement,      only: ResizeArray
    use GenericInputOutput,    only: GenericTextFile
    use CoordinateFileFormats, only: CoordData
    implicit none

    type(GenericTextFile), intent(in)    :: file
    real(dp),              intent(in)    :: tk          !Temperature (Kelvin).
    real(dp),              intent(in)    :: emin(:)     !Energies (current) for MM system frames (Hartree).
    real(dp),              intent(in)    :: emio(:)     !Energies (reference) for MM system frames (Hartree).
    type(CoordData),       intent(inout) :: gmme        !QM cluster (ensemble) in MM system (gradients in a.u.).
    type(CoordData),       intent(inout) :: gmmi        !QM cluster (first) in MM system (gradients in a.u.).

    character(len=sl)     :: buff
    real(dp)              :: tkn, beta, sbwt
    integer               :: ii, jj, nn, aa, bb, code
    real(dp), allocatable :: bwt(:), fx(:), fy(:), fz(:)

    procname = 'FileParsers::ExtractDlpolyGradients_dpdpaX2gtftcdX2'
    if (ltrace) call PrintTrace()

    !Resize arrays.
    if (tk<one) call PrintError (ekey=3042, lstop=.true.)
    if ((.not. gmme%ksiz) .or. (gmme%numa<1) .or.  &
        (.not. gmmi%ksiz) .or. (gmmi%numa/=gmme%numa)) call PrintError (ekey=1103, lstop=.true.)
    if ((size(emin)<1) .or. (size(emin)/=size(emio))) call PrintError (ekey=1101, lstop=.true.)
    nn = (gmme%numa+4) * size(emin)
    if ((.not. file%ksiz) .or. (file%numl/=nn)) call PrintError (ekey=1103, lstop=.true.)
    call ResizeArray (nume=size(emin), ida=bwt)
    call ResizeArray (nume=gmme%numa,  ida=fx)
    call ResizeArray (nume=gmme%numa,  ida=fy)
    call ResizeArray (nume=gmme%numa,  ida=fz)

    !Calculate Boltzmann weights for all frames.
    beta = one / (BZHoK*tk)
    do ii = 1, size(emin)
      bwt(ii) = one     !exp(beta*(emio(ii)-emin(ii)))
    end do
    sbwt = one / sum(bwt)
    bwt  = bwt * sbwt

    !Remove all forces from ensemble.
    gmme%fx = zero
    gmme%fy = zero
    gmme%fz = zero

    !Loop over number of frames.
    do ii = 1, size(emin)
      !Determine start of next frame. Skip frame index on line 1.
      nn = (gmme%numa+4) * (ii-1)
      buff = adjustl(file%line(nn+1))
      if (buff(1:10)/='$MMS_FRAME')  &
        call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY frame')

      !Extract temperature from line 2.
      buff = adjustl(file%line(nn+2))
      if (buff(1:12)/='$temperature')  &
        call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY temperature')
      jj = 0
      jj = index (buff, '=', .true.)
        if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
      buff(1:jj) = ' '
      read (buff,*,iostat=code) tkn
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: DLPOLY temperature')
      if (abs(tkn-tk)>stol) call PrintError (ekey=3041, lstop=.true., ikey=ii)

      !Skip lines 3-4.
      buff = adjustl(file%line(nn+3))
      if (buff(1:7)/='$energy') call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY energy')
      buff = adjustl(file%line(nn+4))
      if (buff(1:10)/='$gradients') call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY gradients')

      !Extract forces (ignore indices) in temporary arrays.
      do jj = 1, gmme%numa
        read (file%line(nn+4+jj),*,iostat=code) aa, bb, fx(jj), fy(jj), fz(jj)
          if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: DLPOLY gradients')
      end do

      !Change sign (forces -> gradients).
      fx = -fx
      fy = -fy
      fz = -fz

      !Save force for first image.
      if (ii==1) then
        gmmi%fx = fx
        gmmi%fy = fy
        gmmi%fz = fz
      end if

      !Calculate weighted gradients.
      gmme%fx = gmme%fx + fx*bwt(ii)
      gmme%fy = gmme%fy + fy*bwt(ii)
      gmme%fz = gmme%fz + fz*bwt(ii)
    end do

    !Clean up memory.
    call ResizeArray (nume=0, ida=bwt)
    call ResizeArray (nume=0, ida=fx)
    call ResizeArray (nume=0, ida=fy)
    call ResizeArray (nume=0, ida=fz)

  end subroutine ExtractDlpolyGradients_dpdpaX2gtftcdX2

  subroutine ExtractDlpolyGradients_dpgtftcd (file, tk, gmm)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: dp, sl
    use GenericInputOutput,    only: GenericTextFile
    use CoordinateFileFormats, only: CoordData
    implicit none

    type(GenericTextFile), intent(in)    :: file
    real(dp),              intent(inout) :: tk       !Temperature (Kelvin).
    type(CoordData),       intent(inout) :: gmm      !QM cluster in MM system (gradients in a.u.).

    character(len=sl) :: buff
    integer           :: ii, jj, aa, bb, code

    procname = 'ExtractDlpolyGradients_dpgtftcd'
    if (ltrace) call PrintTrace()

    !Skip frame index on line 1.
    if ((.not. gmm%ksiz)  .or. (gmm%numa<1) .or.  &
        (.not. file%ksiz) .or. (file%numl<gmm%numa+4)) call PrintError (ekey=1103, lstop=.true.)
    buff = adjustl(file%line(1))
    if (buff(1:10)/='$MMS_FRAME')  &
      call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY frame')

    !Extract temperature from line 2.
    buff = adjustl(file%line(2))
    if (buff(1:12)/='$temperature')  &
      call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY temperature')
    jj = 0
    jj = index (buff, '=', .true.)
      if (jj<=0) call PrintError (ekey=1125, lstop=.true.)
    buff(1:jj) = ' '
    read (buff,*,iostat=code) tk
      if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: DLPOLY temperature')

    !Skip lines 3-4.
    buff = adjustl(file%line(3))
    if (buff(1:7)/='$energy') call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY energy')
    buff = adjustl(file%line(4))
    if (buff(1:10)/='$gradients') call PrintError (ekey=4001, lstop=.true., msg1='Section: DLPOLY gradients')

    !Extract forces (ignore indices).
    do ii = 1, gmm%numa
      read (file%line(ii+4),*,iostat=code) aa, bb, gmm%fx(ii), gmm%fy(ii), gmm%fz(ii)
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: DLPOLY gradients')
    end do

    !Change sign (forces -> gradients).
    gmm%fx = -gmm%fx
    gmm%fy = -gmm%fy
    gmm%fz = -gmm%fz

  end subroutine ExtractDlpolyGradients_dpgtftcd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                           INTERFACE WRITEDLPOLYENERGY                                            !
  !******************************************************************************************************************!
  subroutine WriteDlpolyEnergy_dpdpagtf (file, tk, scfei)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: dp, F112, F211
    use GenericInputOutput, only: GenericTextFile
    implicit none

    type(GenericTextFile), intent(inout) :: file
    real(dp),              intent(in)    :: tk           !Temperature (Kelvin).
    real(dp),              intent(in)    :: scfei(:)     !Energies for MM system frames (Hartree).

    integer :: ii, code

    procname = 'FileParsers::WriteDlpolyEnergy_dpdpagtf'
    if (ltrace) call PrintTrace()

    !Resize file object.
    if (size(scfei)<1) call PrintError (ekey=1101, lstop=.true.)
    file%numl = size(scfei) + 2
    call file%Resize()

    !Write temperature.
    write (file%line(1),F211,iostat=code) tk
      if (code>0) call PrintError (ekey=2121, lstop=.true., msg1='Section: DLPOLY temperature')
    file%line(1) = '$temperature = ' // trim(file%line(1))

    !Write all energies.
    file%line(2) = '$energy'
    do ii = 1, size(scfei)
      write (file%line(ii+2),F112,iostat=code) ii, scfei(ii)
        if (code>0) call PrintError (ekey=2121, lstop=.true., msg1='Section: DLPOLY energy')
    end do

  end subroutine WriteDlpolyEnergy_dpdpagtf
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                          INTERFACE WRITEDLPOLYGRADIENTS                                          !
  !******************************************************************************************************************!
  subroutine WriteDlpolyGradients_dpgtftcd (file, tk, gmm)
    use Debugger,              only: PrintError, procname, ltrace, PrintTrace
    use Parameters,            only: dp, F104, F211
    use GenericInputOutput,    only: GenericTextFile
    use CoordinateFileFormats, only: CoordData
    implicit none

    type(GenericTextFile), intent(inout) :: file
    real(dp),              intent(in)    :: tk       !Temperature (Kelvin).
    type(CoordData),       intent(in)    :: gmm      !QM cluster in MM system (gradients in a.u.).

    integer :: ii, code

    procname = 'FileParsers::WriteDlpolyGradients_dpgtftcd'
    if (ltrace) call PrintTrace()

    !Resize file object.
    if ((.not. gmm%ksiz) .or. (gmm%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    file%numl = gmm%numa + 2
    call file%Resize()

    !Write temperature.
    write (file%line(1),F211,iostat=code) tk
      if (code>0) call PrintError (ekey=2121, lstop=.true., msg1='Section: DLPOLY temperature')
    file%line(1) = '$temperature = ' // trim(file%line(1))

    !Write all gradients.
    file%line(2) = '$gradients'
    do ii = 1, gmm%numa
      write (file%line(ii+2),F104,iostat=code) ii, gmm%fx(ii), gmm%fy(ii), gmm%fz(ii)
        if (code>0) call PrintError (ekey=2121, lstop=.true., msg1='Section: DLPOLY gradients')
    end do

  end subroutine WriteDlpolyGradients_dpgtftcd
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                           INTERFACE WRITESMSSENSEMBLE                                            !
  !******************************************************************************************************************!
  subroutine WriteSmssEnsemble_gtfsg (file, gmi)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: F114
    use GenericInputOutput, only: GenericTextFile
    use SolvationScheme,    only: SmssGeometry
    implicit none

    type(GenericTextFile), intent(inout) :: file
    type(SmssGeometry),    intent(in)    :: gmi      !SMSS geometry (matching indices and coordinate shifts).

    integer :: ii, code

    procname = 'FileParsers::WriteSmssEnsemble_gtfsg'
    if (ltrace) call PrintTrace()

    !Resize file object.
    if ((.not. gmi%ksiz) .or. (gmi%numa<1)) call PrintError (ekey=1103, lstop=.true.)
    file%numl = gmi%numa + 1
    call file%Resize()

    !Write matching indices and coordinate shifts.
    file%line(1) = '$MMS_SHIFTS'
    do ii = 1, gmi%numa
      write (file%line(ii+1),F114,iostat=code) gmi%idxc(ii), gmi%idxm(ii), gmi%cx(ii), gmi%cy(ii), gmi%cz(ii)
        if (code>0) call PrintError (ekey=2121, lstop=.true.)
    end do

  end subroutine WriteSmssEnsemble_gtfsg
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                            INTERFACE READSMSSENSEMBLE                                            !
  !******************************************************************************************************************!
  subroutine ReadSmssEnsemble_gtfsg (file, gmi)
    use Debugger,           only: PrintError, procname, ltrace, PrintTrace
    use Parameters,         only: sl
    use UtilityProcedures,  only: ConvertToUpperCase
    use GenericInputOutput, only: GenericTextFile
    use SolvationScheme,    only: SmssGeometry
    implicit none

    type(GenericTextFile), intent(in)    :: file
    type(SmssGeometry),    intent(inout) :: gmi      !SMSS geometry (matching indices and coordinate shifts).

    character(len=sl) :: buff
    integer           :: ii, jj, code

    procname = 'FileParsers::ReadSmssEnsemble_gtfsg'
    if (ltrace) call PrintTrace()

    !Locate $MMS_SHIFTS section for matching indices. This section must be present and contain at least 1 atom.
    if ((.not. file%ksiz) .or. (file%numl<1)) call PrintError (ekey=1103, lstop=.true.)
    jj = 0
    do ii = 1, file%numl
      buff = adjustl(file%line(ii))
      call ConvertToUpperCase (line=buff)
      if (buff(1:11)=='$MMS_SHIFTS') jj = ii
    end do
    if (jj==0) call PrintError (ekey=4001, lstop=.true., msg1='Section: $MMS_SHIFTS')

    !Count the number of SMSS atoms in this section. Counting continues till next $KEYWORD or end-of-file. Then
    !resize/initialize the object.
    gmi%numa = 0
    do ii = jj+1, file%numl
      buff = adjustl(file%line(ii))
      if (buff(1:1)=='$') exit
      gmi%numa = gmi%numa + 1
    end do
    if (gmi%numa==0) call PrintError (ekey=3001, lstop=.true.)
    call gmi%Initialize()

    !Read matching indices and coordinate shifts.
    do ii = 1, gmi%numa
      read (file%line(jj+ii),*,iostat=code) gmi%idxc(ii), gmi%idxm(ii), gmi%cx(ii), gmi%cy(ii), gmi%cz(ii)
        if (code>0) call PrintError (ekey=2111, lstop=.true., msg1='Section: $MMS_SHIFTS')
    end do

  end subroutine ReadSmssEnsemble_gtfsg
  !******************************************************************************************************************!

end module FileParsers
!***********************************************************************************************************************
