!**********************************************************************************************************************!
!Main program file for calculating total QM/MM gradients using SMSS.                                                   !
!**********************************************************************************************************************!
program SmssEvaluateGradients

  use Parameters,            only: dp, pl, stol
  use Debugger,              only: PrintError, procname, ltrace, PrintTrace
  use Timers,                only: StopWatch
  use MemoryManagement,      only: ResizeArray
  use UtilityProcedures,     only: FileExists
  use CommandExecutors,      only: ChangeDirectory, CopyFile, ExecuteSystemCommand, SaveLogFile
  use GenericInputOutput,    only: GenericTextFile
  use CoordinateFileFormats, only: ConfigData, ConfigHeader, CoordData, PoscarData, PoscarHeader
  use SolvationScheme,       only: SmssGeometry, SmssSetup, CalculateSmssGradients, PrepareProgramJob,      &
                                   SynchronizeSmssCoordinates, ValidateSmssIndices
  use FileParsers,           only: ExtractDlpolyEnergy, ExtractDlpolyGradients, ExtractTurbomoleGradients,  &
                                   ExtractVaspGradients, ReplaceTurbomoleGradients, RiApproximationIsUsed,  &
                                   WriteDlpolyGradients, WriteSmssEnsemble
  implicit none

  type(StopWatch)       :: timer
  type(GenericTextFile) :: file1, file2, file3, file4, file5, file6, file7, file8
  type(GenericTextFile) :: filed, fileh
  type(CoordData)       :: datac, datacw
  type(CoordData)       :: datame, datami, datamr
  type(ConfigData)      :: datam
  type(ConfigHeader)    :: headm
  type(PoscarData)      :: datas
  type(PoscarHeader)    :: heads
  type(SmssGeometry)    :: sgmn, sgmo
  type(SmssSetup)       :: smss
  character(len=pl)     :: fbkp, fout
  real(dp)              :: tkn, tkr
  real(dp)              :: scfes, scfec, scfecw, scfem
  real(dp), allocatable :: scfemn(:), scfemo(:)

  procname = 'SmssEvaluateGradients::SmssEvaluateGradients'
  if (ltrace) call PrintTrace()

  call timer%Start()
  timer%name = 'SMSSG Timer'

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
    fout = 'rdgrad.out'
  else
    fbkp = 'control.dscf'
    fout = 'grad.out'
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

  !Execute shell script. (The one present in directory of optimization driver program takes precedence).
  call ChangeDirectory (dir=smss%wdir_base)
  if (FileExists(file='smss-gradients.sh')) then
    call ExecuteSystemCommand (cmdl='${PWD}/smss-gradients.sh')
  else
    call ExecuteSystemCommand (cmdl='smss-gradients.sh')
  end if

  !Read TURBOMOLE and VASP gradients from output files.
  call datac%Duplicate (dupl=datacw)
  call file1%ReadFromDisc (file=trim(smss%wdir_turbo)//'gradient')
  call ExtractTurbomoleGradients (file=file1, gmc=datac)
  call file2%ReadFromDisc (file=trim(smss%wdir_vasp)//'OUTCAR')
  call ExtractVaspGradients (file=file2, gms=datas)
  if (smss%mode=='IMPLICIT') then
    call file3%ReadFromDisc (file=trim(smss%wdir_cosmo)//'gradient')
    call ExtractTurbomoleGradients (file=file3, gmc=datacw)
  else if (smss%mode=='EXPLICIT') then
    call file3%ReadFromDisc (file=trim(smss%wdir_peecm)//'gradient')
    call ExtractTurbomoleGradients (file=file3, gmc=datacw)
  end if

  !Read current and reference QM/MM energies.
  if (smss%mode=='EXPLICIT') then
    call file4%ReadFromDisc (file=trim(smss%wdir_dlpoly)//'MMS_ENERGY')
    call ExtractDlpolyEnergy (file=file4, tk=tkn, scfei=scfemn, lnew=.false.)
    call file5%ReadFromDisc (file=trim(smss%wdir_dlpoly)//'MMS_REF_ENERGY')
    call ExtractDlpolyEnergy (file=file5, tk=tkr, scfei=scfemo, lnew=.false.)
      if (abs(tkn-tkr)>stol) call PrintError (ekey=3041, lstop=.true.)
  end if

  !Read DLPOLY gradients from output files.
  if (smss%mode=='EXPLICIT') then
    call datac%Duplicate (dupl=datame)
    call datac%Duplicate (dupl=datami)
    call datac%Duplicate (dupl=datamr)
    call file6%ReadFromDisc (file=trim(smss%wdir_refimg)//'MMS_REPLAY')
    call ExtractDlpolyGradients (file=file6, tk=tkn, gmm=datamr)
      if (abs(tkn-tkr)>stol) call PrintError (ekey=3041, lstop=.true.)
    call file7%ReadFromDisc (file=trim(smss%wdir_dlpoly)//'MMS_STEP')
    call ExtractDlpolyGradients (file=file7, tk=tkn, emin=scfemn, emio=scfemo, gmme=datame, gmmi=datami)
      if (abs(tkn-tkr)>stol) call PrintError (ekey=3041, lstop=.true.)
  end if

  !Calculate QM/MM energies and save DLPOLY ensemble-averaged gradients.
  if (smss%mode=='IMPLICIT') then
    call CalculateSmssGradients (gmc=datac, gms=datas, gmcw=datacw, gmi=sgmn)
  else if (smss%mode=='EXPLICIT') then
    call CalculateSmssGradients (gmc=datac, gms=datas, gmmr=datamr, gmme=datame, gmmi=datami, gmcw=datacw, gmi=sgmn)
    call WriteDlpolyGradients (file=file8, tk=tkn, gmm=datame)
    call file8%WriteToDisc (file=trim(smss%wdir_dlpoly)//'MMS_GRADIENTS', mode='replace')
  end if

  !Replace last cycle of gradients.
  if (smss%mode=='IMPLICIT') then
    if (smss%driver=='COSMO') then
      call ReplaceTurbomoleGradients (file=file3, gmc=datacw)
      call file3%WriteToDisc (file=trim(smss%wdir_cosmo)//'gradient', mode='replace')
    else if (smss%driver=='TURBOMOLE') then
      call ReplaceTurbomoleGradients (file=file1, gmc=datacw)
      call file1%WriteToDisc (file=trim(smss%wdir_turbo)//'gradient', mode='replace')
    end if
  else if (smss%mode=='EXPLICIT') then
    if (smss%driver=='PEECM') then
      call ReplaceTurbomoleGradients (file=file3, gmc=datacw)
      call file3%WriteToDisc (file=trim(smss%wdir_peecm)//'gradient', mode='replace')
    else if (smss%driver=='TURBOMOLE') then
      call ReplaceTurbomoleGradients (file=file1, gmc=datacw)
      call file1%WriteToDisc (file=trim(smss%wdir_turbo)//'gradient', mode='replace')
    end if
  end if

  !Save log files.
  if (smss%save_logs) then
    if (smss%mode=='EXPLICIT') then
      call ChangeDirectory (dir=smss%wdir_dlpoly)
      call SaveLogFile (file='MMS_GRADIENTS', mode='append')
    end if
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
  call datacw%Erase()
  call datam%Erase()
  call datame%Erase()
  call datami%Erase()
  call datamr%Erase()
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

end program SmssEvaluateGradients
!**********************************************************************************************************************!
