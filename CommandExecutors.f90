!**********************************************************************************************************************!
!This module provides procedures for direct execution of operating system commands.                                    !
!**********************************************************************************************************************!
module CommandExecutors

  implicit none

  public

  !******************************************************************************************************************!
  !                                               CONTAINED PROCEDURES                                               !
  !                                                                                                                  !
  !'CopyFile' copies a file to another one (overloaded).                                                             !
  !'ChangeDirectory' changes the present working directory.                                                          !
  !'CreateNewDirectory' creates a new directory at the specified path.                                               !
  !'ExecuteSystemCommand' executes an operating system command.                                                      !
  !'SaveLogFiles' saves a duplicate file.                                                                            !
  !******************************************************************************************************************!

  interface CopyFile
    module procedure CopyFile_sX2l
    module procedure CopyFile_sX4l
  end interface CopyFile

  interface ChangeDirectory
    module procedure ChangeDirectory_s
  end interface ChangeDirectory

  interface CreateNewDirectory
    module procedure CreateNewDirectory_s
  end interface CreateNewDirectory

  interface ExecuteSystemCommand
    module procedure ExecuteSystemCommand_s
  end interface ExecuteSystemCommand

  interface SaveLogFile
    module procedure SaveLogFile_sX3
  end interface SaveLogFile

  contains

  !******************************************************************************************************************!
  !                                                INTERFACE COPYFILE                                                !
  !******************************************************************************************************************!
  subroutine CopyFile_sX2l (sfile, tfile, lreplace)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: pl
    use UtilityProcedures, only: FileExists
    implicit none

    character(len=*),  intent(in) :: sfile, tfile     !Files (source/target).
    logical, optional, intent(in) :: lreplace         !Replace existing file?

    character(len=pl) :: cmdl

    procname = 'CommandExecutors::CopyFile_sX2l'
    if (ltrace) call PrintTrace()

    !Validate input parameters.
    if (len_trim(sfile)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: sfile')
    if (len_trim(tfile)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: tfile')

    !Ensure that the source and the target files are different. Also verify that the source file exists.
    if (sfile==tfile) call PrintError (ekey=2022, lstop=.true., msg1='File: '//sfile)
    if (.not. FileExists(file=sfile)) call PrintError (ekey=2002, lstop=.true., msg1='File: '//sfile)

    !Carry out the file copy operation only if the target file does not already exist or if it should be replaced.
    if (FileExists(file=tfile)) then
      if (present(lreplace)) then
        if (.not. lreplace) return
      end if
    end if
    if (len(cmdl)<len_trim(sfile)+len_trim(tfile)+7) call PrintError (ekey=1122, lstop=.true.)
    cmdl = 'cp -f ' // trim(adjustl(sfile)) // ' ' // trim(adjustl(tfile))
    call ExecuteSystemCommand (cmdl=cmdl)

  end subroutine CopyFile_sX2l

  subroutine CopyFile_sX4l (sdir, sfile, tdir, tfile, lreplace)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: pl
    use UtilityProcedures, only: AppendCharacter, DirectoryExists, FileExists
    implicit none

    character(len=*),  intent(in) :: sdir, tdir       !Directories (source/target).
    character(len=*),  intent(in) :: sfile, tfile     !Files (source/target).
    logical, optional, intent(in) :: lreplace         !Replace existing file?

    character(len=pl) :: cmdl, sdpath, tdpath, sfpath, tfpath

    procname = 'CommandExecutors::CopyFile_sX4l'
    if (ltrace) call PrintTrace()

    !Validate input parameters.
    if (len_trim(sdir)<1)  call PrintError (ekey=1121, lstop=.true., msg1='Parameter: sdir')
    if (len_trim(tdir)<1)  call PrintError (ekey=1121, lstop=.true., msg1='Parameter: tdir')
    if (len_trim(sfile)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: sfile')
    if (len_trim(tfile)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: tfile')

    !Ensure that the source directory and the source file exist.
    if (len(sdpath)<len(sdir)) call PrintError (ekey=1122, lstop=.true.)
    sdpath = adjustl(sdir)
    call AppendCharacter (line=sdpath, sym='/', lskip=.true.)
    if (.not. DirectoryExists(dir=sdpath)) call PrintError (ekey=2001, lstop=.true., msg1='Directory: '//sdpath)
    if (len(sfpath)<len_trim(sdpath)+len_trim(sfile)) call PrintError (ekey=1122, lstop=.true.)
    sfpath = trim(sdpath) // trim(adjustl(sfile))
    if (.not. FileExists(file=sfpath)) call PrintError (ekey=2002, lstop=.true., msg1='File: '//sfpath)

    !Ensure that the source and the target files are different. Create target directory if required.
    if (len(tdpath)<len(tdir)) call PrintError (ekey=1122, lstop=.true.)
    tdpath = adjustl(tdir)
    call AppendCharacter (line=tdpath, sym='/', lskip=.true.)
    if (len(tfpath)<len_trim(tdpath)+len_trim(tfile)) call PrintError (ekey=1122, lstop=.true.)
    tfpath = trim(tdpath) // trim(adjustl(tfile))
    if (sfpath==tfpath) call PrintError (ekey=2022, lstop=.true., msg1='File: '//sfpath)
    if (.not. DirectoryExists(dir=tdpath)) call CreateNewDirectory (dir=tdpath)

    !Carry out the file copy operation only if the target file does not already exist or if it should be replaced.
    if (FileExists(file=tfpath)) then
      if (present(lreplace)) then
        if (.not. lreplace) return
      end if
    end if
    if (len(cmdl)<len_trim(sfpath)+len_trim(tfpath)+7) call PrintError (ekey=1122, lstop=.true.)
    cmdl = 'cp -f ' // trim(sfpath) // ' ' // trim(tfpath)
    call ExecuteSystemCommand (cmdl=cmdl)

  end subroutine CopyFile_sX4l
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                            INTERFACE CHANGEDIRECTORY                                             !
  !******************************************************************************************************************!
  subroutine ChangeDirectory_s (dir)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use UtilityProcedures, only: DirectoryExists
    implicit none

    character(len=*), intent(in) :: dir

    procname = 'CommandExecutors::ChangeDirectory_s'
    if (ltrace) call PrintTrace()

    if (len_trim(dir)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: dir')
    if (.not. DirectoryExists(dir=dir)) call PrintError (ekey=2001, lstop=.true., msg1='Directory: '//dir)
    call chdir (trim(adjustl(dir)))

  end subroutine ChangeDirectory_s
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                           INTERFACE CREATENEWDIRECTORY                                           !
  !******************************************************************************************************************!
  subroutine CreateNewDirectory_s (dir)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: pl
    use UtilityProcedures, only: AppendCharacter, DirectoryExists
    implicit none

    character(len=*), intent(in) :: dir

    character(len=pl) :: cmdl

    procname = 'CommandExecutors::CreateNewDirectory_s'
    if (ltrace) call PrintTrace()

    !Create new directory (and all parent directories) as required.
    if (len_trim(dir)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: dir')
    if (DirectoryExists(dir=dir)) return
    if (len(cmdl)<len_trim(dir)+9) call PrintError (ekey=1122, lstop=.true.)
    cmdl = 'mkdir -p ' // trim(adjustl(dir))
    call ExecuteSystemCommand (cmdl=cmdl)
    if (.not. DirectoryExists(dir=dir)) call PrintError (ekey=2011, lstop=.true., msg1='Directory: '//dir)

  end subroutine CreateNewDirectory_s
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                          INTERFACE EXECUTESYSTEMCOMMAND                                          !
  !******************************************************************************************************************!
  subroutine ExecuteSystemCommand_s (cmdl, dir)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: pl
    implicit none

    character(len=*),           intent(in) :: cmdl     !Command to be executed.
    character(len=*), optional, intent(in) :: dir      !Different directory than current?

    character(len=pl) :: pwd

    procname = 'CommandExecutors::ExecuteSystemCommand_s'
    if (ltrace) call PrintTrace()

    if (len_trim(cmdl)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: cmdl')
    if (present(dir)) then
      if (len_trim(dir)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: dir')
      call get_environment_variable ('PWD', pwd)
      call ChangeDirectory (dir=dir)
      call system (trim(adjustl(cmdl)))
      call ChangeDirectory (dir=pwd)
    else
      call system (trim(adjustl(cmdl)))
    end if

  end subroutine ExecuteSystemCommand_s
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                              INTERFACE SAVELOGFILE                                               !
  !******************************************************************************************************************!
  subroutine SaveLogFile_sX3 (file, dir, mode)
    use Debugger,          only: PrintError, procname, ltrace, PrintTrace
    use Parameters,        only: pl, sl
    use UtilityProcedures, only: ConvertToUpperCase, FileExists
    implicit none

    character(len=*),           intent(in) :: file
    character(len=*), optional, intent(in) :: dir      !Different directory than current?
    character(len=*), optional, intent(in) :: mode     !Action for saving file (append/replace)?

    character(len=sl) :: action = 'APPEND'     !Default action is 'APPEND'.
    character(len=pl) :: cmdl

    procname = 'CommandExecutors::SaveLogFile_sX2'
    if (ltrace) call PrintTrace()

    !Validate input parameters.
    if (len_trim(file)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: file')
    if (present(dir)) then
      if (len_trim(dir)<1) call PrintError (ekey=1121, lstop=.true., msg1='Parameter: dir')
    end if

    !Override default action?
    if (present(mode)) then
      if (len(action)<len(mode)) call PrintError (ekey=1122, lstop=.true.)
      action = adjustl(mode)
      call ConvertToUpperCase (line=action)
      if ((action/='APPEND') .and. (action/='REPLACE'))  &
        call PrintError (ekey=1302, lstop=.true., msg1='Keyword: mode')
    end if

    !Only if the specified file exists, create and execute appropriate command.
    if (FileExists(file=file)) then
      if (len(cmdl)<2*len_trim(file)+12) call PrintError (ekey=1122, lstop=.true.)
      if (action=='APPEND') then
        cmdl = 'cat ' // trim(file) // ' >> log.' // trim(file)
      else if (action=='REPLACE') then
        cmdl = 'cat ' // trim(file) // ' >  log.' // trim(file)
      end if

      if (present(dir)) then
        call ExecuteSystemCommand (cmdl=cmdl, dir=dir)
      else
        call ExecuteSystemCommand (cmdl=cmdl)
      end if
    end if

  end subroutine SaveLogFile_sX3
  !******************************************************************************************************************!

end module CommandExecutors
!**********************************************************************************************************************!
