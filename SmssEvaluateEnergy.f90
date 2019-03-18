!**********************************************************************************************************************!
!Main program file for calculating total QM/MM energy using SMSS.                                                      !
!**********************************************************************************************************************!
program SmssEvaluateEnergy

  use Parameters,            only: dp, pl, stol, zero
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use Timers,                only: StopWatch
  use MemoryManagement,      only: ResizeArray
  use UtilityProcedures,     only: FileExists
  use CommandExecutors,      only: ChangeDirectory, CopyFile, ExecuteSystemCommand, SaveLogFile
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: ConfigData, ConfigHeader, CoordData, PoscarData, PoscarHeader
  use SolvationScheme,       only: SmssGeometry, SmssSetup, CalculateSmssEnergy, PrepareProgramJob,   &
                                   SynchronizeSmssCoordinates, ValidateSmssIndices
  use FileParsers,           only: ExtractDlpolyEnergy, ExtractTurbomoleEnergy, ExtractVaspEnergy,    &
                                   ReplaceTurbomoleEnergy, RiApproximationIsUsed, WriteDlpolyEnergy,  &
                                   WriteSmssEnsemble
  implicit none

  type(StopWatch)       :: timer
  type(GenericTextFile) :: file1, file2, file3, file4, file5, file6, file7, file8
  type(GenericTextFile) :: filed, fileh
  type(CoordData)       :: datac
  type(ConfigData)      :: datam
  type(ConfigHeader)    :: headm
  type(PoscarData)      :: datas
  type(PoscarHeader)    :: heads
  type(SmssGeometry)    :: sgmn, sgmo
  type(SmssSetup)       :: smss
  character(len=pl)     :: fbkp, fout
  real(dp)              :: tkn, tkr
  real(dp)              :: scfes, scfec, scfecw, scfem, fepe
  real(dp), allocatable :: scfemn(:), scfemo(:)

  procname = 'SmssEvaluateEnergy::SmssEvaluateEnergy'
  if (ltrace) call PrintTrace()

  call timer%Start()
  timer%name = 'SMSSE Timer'

  !Process input options.
  call file1%ReadFromDisc (file='SMSS_INPUT')
  call smss%ReadFromFile (file=file1)
  call file2%ReadFromDisc (file=trim(smss%idir_turbo)//'control')
  smss%rij = RiApproximationIsUsed (file=file2)

  !Read QM cluster data from source files.
  call file3%ReadFromDisc (file=trim(smss%idir_turbo)//'coord')
  call datac%ReadFromFile (file=file3)

  !Read QM surface data from source files.
  call file4%ReadFromDisc (file=trim(smss%idir_vasp)//'POSCAR')
  call file4%GetHead (head=fileh, numl=9)
  call heads%ReadFromFile (file=fileh)
  call file4%GetSection (esec=filed, numli=9+1, numlf=9+heads%numa)
  call datas%ReadFromFile (file=filed)

  !Read MM system data from source files.
  if (smss%mode=='EXPLICIT') then
    call file5%ReadFromDisc (file=trim(smss%idir_dlpoly)//'CONFIG')
    call file5%GetHead (head=fileh, numl=5)
    call headm%ReadFromFile (file=fileh)
    call file5%GetSection (esec=filed, numli=5+1, numlf=5+headm%numa*(headm%kfrm+2))
    call datam%ReadFromFile (file=filed, kfrm=headm%kfrm)
  end if

  !Import coordinates/indices and validate that the structures are correctly matched.
  sgmn%numa = datac%numa
  call sgmn%Initialize()
  call sgmn%ImportCoordinates (gmc=datac)
  call sgmn%ImportIndices (setup=smss)
  call ValidateSmssIndices (gmc=datac, gms=datas, gmi=sgmn)
  if (smss%mode=='EXPLICIT') call ValidateSmssIndices (gmc=datac, gmm=datam, gmi=sgmn)

  !Prepare DLPOLY, TURBOMOLE and VASP jobs.
  call PrepareProgramJob (sdir=smss%idir_turbo,  tdir=smss%wdir_turbo,  prog='turbomole')
  call PrepareProgramJob (sdir=smss%idir_vasp,   tdir=smss%wdir_vasp,   prog='vasp')
  if (smss%mode=='IMPLICIT') then
    call PrepareProgramJob (sdir=smss%idir_cosmo,  tdir=smss%wdir_cosmo,  prog='cosmo')
  else if (smss%mode=='EXPLICIT') then
    call PrepareProgramJob (sdir=smss%idir_peecm,  tdir=smss%wdir_peecm,  prog='peecm')
    call PrepareProgramJob (sdir=smss%idir_dlpoly, tdir=smss%wdir_dlpoly, prog='dlpoly')
    call PrepareProgramJob (sdir=smss%idir_refimg, tdir=smss%wdir_refimg, prog='dlpoly')
  end if

  !Re-read current coordinates from directory of optimization driver program.
  call sgmn%Duplicate (dupl=sgmo)
  call file6%ReadFromDisc (file=trim(smss%wdir_base)//'coord')
  call datac%ReadFromFile (file=file6)
  call sgmn%ImportCoordinates (gmc=datac)

  !Synchronize coordinate files for TURBOMOLE jobs.
  if (smss%mode=='IMPLICIT') then
    if (smss%driver=='COSMO') then
      call CopyFile (sdir=smss%wdir_cosmo, sfile='coord', tdir=smss%wdir_turbo, tfile='coord', lreplace=.true.)
    else if (smss%driver=='TURBOMOLE') then
      call CopyFile (sdir=smss%wdir_turbo, sfile='coord', tdir=smss%wdir_cosmo, tfile='coord', lreplace=.true.)
    end if
  else if (smss%mode=='EXPLICIT') then
    if (smss%driver=='PEECM') then
      call CopyFile (sdir=smss%wdir_peecm, sfile='coord', tdir=smss%wdir_turbo, tfile='coord', lreplace=.true.)
    else if (smss%driver=='TURBOMOLE') then
      call CopyFile (sdir=smss%wdir_turbo, sfile='coord', tdir=smss%wdir_peecm, tfile='coord', lreplace=.true.)
    end if
  end if

  !Synchronize coordinate files for VASP jobs.
  sgmn%cx = sgmn%cx - sgmo%cx
  sgmn%cy = sgmn%cy - sgmo%cy
  sgmn%cz = sgmn%cz - sgmo%cz
  call SynchronizeSmssCoordinates (gms=datas, gmi=sgmn)
  call heads%WriteToFile (file=fileh)
  call fileh%WriteToDisc (file=trim(smss%wdir_vasp)//'POSCAR', mode='replace')
  call datas%WriteToFile (file=filed)
  call filed%WriteToDisc (file=trim(smss%wdir_vasp)//'POSCAR', mode='append')

  !Synchronize coordinate files for DLPOLY jobs.
  if (smss%mode=='EXPLICIT') then
    call WriteSmssEnsemble (file=file7, gmi=sgmn)
    call file7%WriteToDisc (file=trim(smss%wdir_dlpoly)//'MMS_ENSEMBLE', mode='replace')
    call file7%WriteToDisc (file=trim(smss%wdir_refimg)//'MMS_ENSEMBLE', mode='replace')
  end if

  !Setup execution commands.
  if (smss%rij) then
    fbkp = 'control.ridft'
    fout = 'ridft.out'
  else
    fbkp = 'control.dscf'
    fout = 'dscf.out'
  end if

  !Refresh control files (if unchanged files available).
  if (smss%refresh_control) then
    call ChangeDirectory (dir=smss%wdir_turbo)
    if (FileExists(file=fbkp)) call CopyFile (sfile=fbkp, tfile='control', lreplace=.true.)
    if (smss%mode=='IMPLICIT') then
      call ChangeDirectory (dir=smss%wdir_cosmo)
      if (FileExists(file=fbkp)) call CopyFile (sfile=fbkp, tfile='control', lreplace=.true.)
    else if (smss%mode=='EXPLICIT') then
      call ChangeDirectory (dir=smss%wdir_peecm)
      if (FileExists(file=fbkp)) call CopyFile (sfile=fbkp, tfile='control', lreplace=.true.)
    end if
  end if

  !Refresh MOs.
  if (smss%mode=='IMPLICIT') then
    if (smss%refresh_mos=='IDIR_ALL') then
      call CopyFile (sdir=smss%idir_cosmo, sfile='alpha', tdir=smss%wdir_cosmo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_cosmo, sfile='beta',  tdir=smss%wdir_cosmo, tfile='beta',  lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='alpha', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='beta',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='IDIR_SWAP') then
      call CopyFile (sdir=smss%idir_cosmo, sfile='alpha', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_cosmo, sfile='beta',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='alpha', tdir=smss%wdir_cosmo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='beta',  tdir=smss%wdir_cosmo, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='IDIR_COSMO') then
      call CopyFile (sdir=smss%idir_cosmo, sfile='alpha', tdir=smss%wdir_cosmo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_cosmo, sfile='beta',  tdir=smss%wdir_cosmo, tfile='beta',  lreplace=.true.)
      call CopyFile (sdir=smss%idir_cosmo, sfile='alpha', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_cosmo, sfile='beta',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='IDIR_TURBO') then
      call CopyFile (sdir=smss%idir_turbo, sfile='alpha', tdir=smss%wdir_cosmo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='beta',  tdir=smss%wdir_cosmo, tfile='beta',  lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='alpha', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='beta',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='WDIR_SWAP') then
      call CopyFile (sdir=smss%wdir_cosmo, sfile='alpha', tdir=smss%wdir_cosmo, tfile='alpha.swp', lreplace=.true.)
      call CopyFile (sdir=smss%wdir_cosmo, sfile='beta',  tdir=smss%wdir_cosmo, tfile='beta.swp',  lreplace=.true.)
      call CopyFile (sdir=smss%wdir_turbo, sfile='alpha', tdir=smss%wdir_cosmo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%wdir_turbo, sfile='beta',  tdir=smss%wdir_cosmo, tfile='beta',  lreplace=.true.)
      call CopyFile (sdir=smss%wdir_cosmo, sfile='alpha.swp', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%wdir_cosmo, sfile='beta.swp',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='WDIR_COSMO') then
      call CopyFile (sdir=smss%wdir_cosmo, sfile='alpha', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%wdir_cosmo, sfile='beta',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='WDIR_TURBO') then
      call CopyFile (sdir=smss%wdir_turbo, sfile='alpha', tdir=smss%wdir_cosmo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%wdir_turbo, sfile='beta',  tdir=smss%wdir_cosmo, tfile='beta',  lreplace=.true.)
    end if
  else if (smss%mode=='EXPLICIT') then
    if (smss%refresh_mos=='IDIR_ALL') then
      call CopyFile (sdir=smss%idir_peecm, sfile='alpha', tdir=smss%wdir_peecm, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_peecm, sfile='beta',  tdir=smss%wdir_peecm, tfile='beta',  lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='alpha', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='beta',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='IDIR_SWAP') then
      call CopyFile (sdir=smss%idir_peecm, sfile='alpha', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_peecm, sfile='beta',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='alpha', tdir=smss%wdir_peecm, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='beta',  tdir=smss%wdir_peecm, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='IDIR_PEECM') then
      call CopyFile (sdir=smss%idir_peecm, sfile='alpha', tdir=smss%wdir_peecm, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_peecm, sfile='beta',  tdir=smss%wdir_peecm, tfile='beta',  lreplace=.true.)
      call CopyFile (sdir=smss%idir_peecm, sfile='alpha', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_peecm, sfile='beta',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='IDIR_TURBO') then
      call CopyFile (sdir=smss%idir_turbo, sfile='alpha', tdir=smss%wdir_peecm, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='beta',  tdir=smss%wdir_peecm, tfile='beta',  lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='alpha', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%idir_turbo, sfile='beta',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='WDIR_SWAP') then
      call CopyFile (sdir=smss%wdir_peecm, sfile='alpha', tdir=smss%wdir_peecm, tfile='alpha.swp', lreplace=.true.)
      call CopyFile (sdir=smss%wdir_peecm, sfile='beta',  tdir=smss%wdir_peecm, tfile='beta.swp',  lreplace=.true.)
      call CopyFile (sdir=smss%wdir_turbo, sfile='alpha', tdir=smss%wdir_peecm, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%wdir_turbo, sfile='beta',  tdir=smss%wdir_peecm, tfile='beta',  lreplace=.true.)
      call CopyFile (sdir=smss%wdir_peecm, sfile='alpha.swp', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%wdir_peecm, sfile='beta.swp',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='WDIR_PEECM') then
      call CopyFile (sdir=smss%wdir_peecm, sfile='alpha', tdir=smss%wdir_turbo, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%wdir_peecm, sfile='beta',  tdir=smss%wdir_turbo, tfile='beta',  lreplace=.true.)
    else if (smss%refresh_mos=='WDIR_TURBO') then
      call CopyFile (sdir=smss%wdir_turbo, sfile='alpha', tdir=smss%wdir_peecm, tfile='alpha', lreplace=.true.)
      call CopyFile (sdir=smss%wdir_turbo, sfile='beta',  tdir=smss%wdir_peecm, tfile='beta',  lreplace=.true.)
    end if
  end if

  !Extract free-energy change up to this point.
  call ChangeDirectory (dir=smss%wdir_base)
  if (smss%mode=='EXPLICIT') then
    if (FileExists(file='energy')) then
      call file8%ReadFromDisc (file='energy')
      call ExtractTurbomoleEnergy (file=file8, scfe=fepe, lsum=.true.)
    else
      fepe = zero
    end if
  end if

  !Execute shell script. (The one present in directory of optimization driver program takes precedence).
  call ChangeDirectory (dir=smss%wdir_base)
  if (FileExists(file='smss-energy.sh')) then
    call ExecuteSystemCommand (cmdl='${PWD}/smss-energy.sh')
  else
    call ExecuteSystemCommand (cmdl='smss-energy.sh')
  end if

  !Read TURBOMOLE and VASP energies from output files.
  call file1%ReadFromDisc (file=trim(smss%wdir_turbo)//trim(fout))
  call ExtractTurbomoleEnergy (file=file1, scfe=scfec, lsum=.false.)
  call file2%ReadFromDisc (file=trim(smss%wdir_vasp)//'OUTCAR')
  call ExtractVaspEnergy (file=file2, scfe=scfes)
  if (smss%mode=='IMPLICIT') then
    call file3%ReadFromDisc (file=trim(smss%wdir_cosmo)//trim(fout))
    call ExtractTurbomoleEnergy (file=file3, scfe=scfecw, lsum=.false.)
  else if (smss%mode=='EXPLICIT') then
    call file3%ReadFromDisc (file=trim(smss%wdir_peecm)//trim(fout))
    call ExtractTurbomoleEnergy (file=file3, scfe=scfecw, lsum=.false.)
  end if

  !Read DLPOLY energies from output files.
  if (smss%mode=='EXPLICIT') then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   MODIFICATION BY MEHDI ZARE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     Preciously we used the first conformation for refimg while 100
!     conformations were used for PEECM, hence we came up with this modification
!     that we need to use 100 conformation and use the mean field avergae.
!     Basically, this is just a fixed number that is {(1/100)* SUM (em(k)-emi(k), k=1-100}, where em(k) is the energy of one
!     conformation without QM cahrges and emi(k) is the energy of that conformation with QM charges that we got from PEECM       
!     Here I wrote my code and did some modifications in smss-energy.sh code in order to get this fixed number and I am 
!     going to store in in a file called "MMS_REPLAY_AVERAGE" in line 3 and use
!     Faheem code to read this value and store in in 'scfem' and pass it through
!     the code CalulateSmssEnegy as em and substitute (em-emi(1)) in
!     CalulateSmssEnegy subroutine with just em which is my average fixed number
!
!
! 
!     ! FAHEEM's Original line that was comented out by Mehdi Zare   
!    !call file4%ReadFromDisc (file=trim(smss%wdir_refimg)//'MMS_REPLAY')
    call file4%ReadFromDisc (file=trim(smss%wdir_refimg)//'MMS_REPLAY_AVERAGE')
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! END OF MODIFICATIONS BY MEHDI ZARE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call ExtractDlpolyEnergy (file=file4, tk=tkr, scfe=scfem)
  write(*,*) 'scfem is ' , scfem   ! FLAG
    call file5%ReadFromDisc (file=trim(smss%wdir_dlpoly)//'MMS_STEP')
    call ExtractDlpolyEnergy (file=file5, tk=tkn, scfei=scfemn, lnew=.true.)
      if (abs(tkn-tkr)>stol) call PrintError (ekey=3041, lstop=.true.)
  end if

  !Calculate QM/MM energies and save.
  if (smss%mode=='IMPLICIT') then
    call CalculateSmssEnergy (es=scfes, ec=scfec, ecw=scfecw)
  else if (smss%mode=='EXPLICIT') then
    call CalculateSmssEnergy (es=scfes, ec=scfec, ecw=scfecw, em=scfem, emi=scfemn)
    call WriteDlpolyEnergy (file=file6, tk=tkn, scfei=scfemn)
    call file6%WriteToDisc (file=trim(smss%wdir_dlpoly)//'MMS_ENERGY', mode='replace')
  end if

  !Read reference QM/MM energies and calculate free energy difference.
  if (smss%mode=='EXPLICIT') then
    call ChangeDirectory (dir=smss%wdir_dlpoly)
    if (FileExists(file='MMS_REF_ENERGY')) then
      call file7%ReadFromDisc (file=trim(smss%wdir_dlpoly)//'MMS_REF_ENERGY')
      call ExtractDlpolyEnergy (file=file7, tk=tkr, scfei=scfemo, lnew=.false.)
        if (abs(tkn-tkr)>stol) call PrintError (ekey=3041, lstop=.true.)
      call CalculateSmssEnergy (emin=scfemn, emio=scfemo, tk=tkn, ecw=scfecw)
    else
      call CalculateSmssEnergy (es=scfes, ec=scfec, ecw=scfecw)
    end if
    fepe = fepe + scfecw
  end if

  !Replace last SCF energy.
  if (smss%replace_energy) then
    if (smss%mode=='IMPLICIT') then
      if (smss%driver=='COSMO') then
        call file8%ReadFromDisc (file=trim(smss%wdir_cosmo)//'energy')
        call ReplaceTurbomoleEnergy (file=file8, scfe=scfecw)
        call file8%WriteToDisc (file=trim(smss%wdir_cosmo)//'energy', mode='replace')
      else if (smss%driver=='TURBOMOLE') then
        call file8%ReadFromDisc (file=trim(smss%wdir_turbo)//'energy')
        call ReplaceTurbomoleEnergy (file=file8, scfe=scfecw)
        call file8%WriteToDisc (file=trim(smss%wdir_turbo)//'energy', mode='replace')
      end if
    else if (smss%mode=='EXPLICIT') then
      if (smss%driver=='PEECM') then
        call file8%ReadFromDisc (file=trim(smss%wdir_peecm)//'energy')
        call ReplaceTurbomoleEnergy (file=file8, scfe=fepe)
        call file8%WriteToDisc (file=trim(smss%wdir_peecm)//'energy', mode='replace')
      else if (smss%driver=='TURBOMOLE') then
        call file8%ReadFromDisc (file=trim(smss%wdir_turbo)//'energy')
        call ReplaceTurbomoleEnergy (file=file8, scfe=fepe)
        call file8%WriteToDisc (file=trim(smss%wdir_turbo)//'energy', mode='replace')
      end if
    end if
  end if

  !Save log files.
  if (smss%save_logs) then
    call ChangeDirectory (dir=smss%wdir_vasp)
    call SaveLogFile (file='OUTCAR', mode='append')
    if (smss%mode=='EXPLICIT') then
      call ChangeDirectory (dir=smss%wdir_dlpoly)
      call SaveLogFile (file='MMS_ENERGY', mode='append')
    end if
  end if

  !Update reference QM/MM energies.
  if (smss%mode=='EXPLICIT') then
    call ChangeDirectory (dir=smss%wdir_dlpoly)
    call CopyFile (sfile='MMS_ENERGY', tfile='MMS_REF_ENERGY', lreplace=.true.)
  end if

  !Clean up memory.
  call file1%Erase()
  call file2%Erase()
  call file3%Erase()
  call file4%Erase()
  call file5%Erase()
  call file6%Erase()
  call file7%Erase()
  call file8%Erase()
  call filed%Erase()
  call fileh%Erase()
  call datac%Erase()
  call datam%Erase()
  call datas%Erase()
  call heads%Erase()
  call sgmn%Erase()
  call sgmo%Erase()
  call smss%Erase()
  call ResizeArray (nume=0, ida=scfemn)
  call ResizeArray (nume=0, ida=scfemo)

  call ChangeDirectory (dir=smss%wdir_base)
  call timer%Stop()
  call timer%Print()

end program SmssEvaluateEnergy
!**********************************************************************************************************************!
