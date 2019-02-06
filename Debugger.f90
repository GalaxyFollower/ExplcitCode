!**********************************************************************************************************************!
!This module provides procedures for printing information/error messages to help debug the program.                    !
!**********************************************************************************************************************!
module Debugger

  use Parameters, only: sl
  implicit none

  public

  !******************************************************************************************************************!
  !                                                 GLOBAL VARIABLES                                                 !
  !                                                                                                                  !
  !'ltrace' determines whether all procedure calls are traced.                                                       !
  !'procname' is set by each procedure in the format <ModuleName>::<ProcedureName>                                   !
  !******************************************************************************************************************!
  logical           :: ltrace   = .false.
  character(len=sl) :: procname = ' '

  !******************************************************************************************************************!
  !                                               CONTAINED PROCEDURES                                               !
  !                                                                                                                  !
  !'PrintError' prints all available information about an error and (optionally) terminates the program.             !
  !'PrintTrace' tracks the order of procedure calls.                                                                 !
  !******************************************************************************************************************!

  interface PrintError
    module procedure PrintError_iX2sX2l
  end interface PrintError

  interface PrintTrace
    module procedure PrintTrace_void
  end interface PrintTrace

  contains

  !******************************************************************************************************************!
  !                                               INTERFACE PRINTERROR                                               !
  !******************************************************************************************************************!
  subroutine PrintError_iX2sX2l (ekey, ikey, msg1, msg2, lstop)
    use Parameters, only: sl, luerr, F001, F002, F011, F103
    implicit none

    integer,                    intent(in) :: ekey           !Error code.
    integer, optional,          intent(in) :: ikey           !Error information (e.g., location).
    character(len=*), optional, intent(in) :: msg1, msg2     !Up to 2 additional messages.
    logical, optional,          intent(in) :: lstop          !Terminate the program?

    logical           :: lkey  = .false.    !Error key recognized?
    logical           :: lkill = .true.     !Default is to terminate the program for all errors.
    character(len=sl) :: msg0               !Error message for recognized error codes.

    !Override default action?
    if (present(lstop)) lkill = lstop

    !Try to recognize the error code.
    if (ekey==1001) then
      msg0 = ': invalid calendar date.'
      lkey = .true.
    else if (ekey==1101) then
      msg0 = ': out-of-range array index.'
      lkey = .true.
    else if (ekey==1102) then
      msg0 = ': unexpected size of allocatable array.'
      lkey = .true.
    else if (ekey==1103) then
      msg0 = ': unexpected size of derived-type object.'
      lkey = .true.
    else if (ekey==1111) then
      msg0 = ': failed to allocate required memory.'
      lkey = .true.
    else if (ekey==1112) then
      msg0 = ': failed to deallocate memory.'
      lkey = .true.
    else if (ekey==1121) then
      msg0 = ': empty string is not permitted as parameter.'
      lkey = .true.
    else if (ekey==1122) then
      msg0 = ': buffer/string overrun by a parameter.'
      lkey = .true.
    else if (ekey==1123) then
      msg0 = ': out-of-range string index.'
      lkey = .true.
    else if (ekey==1124) then
      msg0 = ': unexpected character in string.'
      lkey = .true.
    else if (ekey==1125) then
      msg0 = ': specified character was not found in string.'
      lkey = .true.
    else if (ekey==1201) then
      msg0 = ': no logical unit available.'
      lkey = .true.
    else if (ekey==1202) then
      msg0 = ': logical unit is not connected.'
      lkey = .true.
    else if (ekey==1301) then
      msg0 = ': unrecognized key.'
      lkey = .true.
    else if (ekey==1302) then
      msg0 = ': unrecognized option.'
      lkey = .true.
    else if (ekey==1303) then
      msg0 = ': unexpected value of keyword.'
      lkey = .true.
    else if (ekey==1304) then
      msg0 = ': unexpected value of variable.'
      lkey = .true.
    else if (ekey==2001) then
      msg0 = ': failed to locate directory.'
      lkey = .true.
    else if (ekey==2002) then
      msg0 = ': failed to locate file.'
      lkey = .true.
    else if (ekey==2011) then
      msg0 = ': failed to create new directory.'
      lkey = .true.
    else if (ekey==2012) then
      msg0 = ': failed to create new file.'
      lkey = .true.
    else if (ekey==2021) then
      msg0 = ': failed to replace existing file.'
      lkey = .true.
    else if (ekey==2022) then
      msg0 = ': source and destination files are same.'
      lkey = .true.
    else if (ekey==2101) then
      msg0 = ': failed to open file.'
      lkey = .true.
    else if (ekey==2111) then
      msg0 = ': failed to read from file.'
      lkey = .true.
    else if (ekey==2112) then
      msg0 = ': failed to parse string.'
      lkey = .true.
    else if (ekey==2121) then
      msg0 = ': failed to write to file.'
      lkey = .true.
    else if (ekey==2131) then
      msg0 = ': file is empty.'
      lkey = .true.
    else if (ekey==2132) then
      msg0 = ': invalid specification of file section.'
      lkey = .true.
    else if (ekey==3001) then
      msg0 = ': too few SMSS atoms.'
      lkey = .true.
    else if (ekey==3011) then
      msg0 = ': out-of-range index for matching SMSS atoms.'
      lkey = .true.
    else if (ekey==3012) then
      msg0 = ': failed to match an atom of QM cluster in QM surface.'
      lkey = .true.
    else if (ekey==3013) then
      msg0 = ': failed to match an atom of QM cluster in MM system.'
      lkey = .true.
    else if (ekey==3014) then
      msg0 = ': index mismatch for SMSS atoms.'
      lkey = .true.
    else if (ekey==3021) then
      msg0 = ': QM cluster is not charge-neutral.'
      lkey = .true.
    else if (ekey==3031) then
      msg0 = ': periodic-box vectors are not aligned along Cartesian axes.'
      lkey = .true.
    else if (ekey==3041) then
      msg0 = ': temperature mismatch.'
      lkey = .true.
    else if (ekey==3042) then
      msg0 = ': invalid temperature.'
      lkey = .true.
    else if (ekey==3101) then
      msg0 = ': directories for input files and for running jobs are same.'
      lkey = .true.
    else if (ekey==3201) then
      msg0 = ': too few unique atom types in POSCAR.'
      lkey = .true.
    else if (ekey==3202) then
      msg0 = ': too many unique atom types in POSCAR.'
      lkey = .true.
    else if (ekey==3203) then
      msg0 = ': mismatch between unique atom types and counts in POSCAR.'
      lkey = .true.
    else if (ekey==3211) then
      msg0 = ': cannot determine whether an atom in POSCAR is fixed or free.'
      lkey = .true.
    else if (ekey==3301) then
      msg0 = ': unsupported file-format key in CONFIG.'
      lkey = .true.
    else if (ekey==3302) then
      msg0 = ': unsupported periodic-box key in CONFIG.'
      lkey = .true.
    else if (ekey==3311) then
      msg0 = ': specified CONFIG files cannot be merged.'
      lkey = .true.
    else if (ekey==3312) then
      msg0 = ': specified CONFIG files cannot be matched.'
      lkey = .true.
    else if (ekey==3401) then
      msg0 = ': cannot determine whether an atom in coord is fixed or free.'
      lkey = .true.
    else if (ekey==4001) then
      msg0 = ': failed to locate required information in file.'
      lkey = .true.
    else if (ekey==4002) then
      msg0 = ': failed to extract required information from file.'
      lkey = .true.
    else if (ekey==5001) then
      msg0 = ': negative numbers are not permitted.'
      lkey = .true.
    else if (ekey==5002) then
      msg0 = ': a larger number is expected in this context.'
      lkey = .true.
    else if (ekey==9001) then
      msg0 = ': a mandatory command-line argument is missing.'
      lkey = .true.
    else if (ekey==9002) then
      msg0 = ': missing value for a command-line argument.'
      lkey = .true.
    else if (ekey==9003) then
      msg0 = ': invalid value specified for a command-line argument.'
      lkey = .true.
    else if (ekey==9004) then
      msg0 = ': conflicting command-line arguments specified.'
      lkey = .true.
    else if (ekey==9011) then
      msg0 = ': failed to read a command-line argument.'
      lkey = .true.
    end if
    if (.not. lkey) msg0 = ': unrecognized error code.'

    !Print error information.
    write (luerr,F001) 'FATAL ERROR ENCOUNTERED!'
    write (luerr,F002) 'Procedure: ', trim(procname)
    write (luerr,F103) ekey, trim(msg0)
    if (lkey) then
      if (present(msg1)) then
        if (len_trim(msg1)>0) write (luerr,F001) trim(msg1)
      end if
      if (present(msg2)) then
        if (len_trim(msg2)>0) write (luerr,F001) trim(msg2)
      end if
      if (present(ikey)) write (luerr,F011) 'Info: ', ikey
    end if

    !Terminate the program? By default, the program should be terminated for all errors.
    if (lkill) then
      write (luerr,F001) 'Program will be terminated now!'
      stop
    else
      write (luerr,F001) 'WARNING! Program termination skipped upon user request.'
    end if

  end subroutine PrintError_iX2sX2l
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                               INTERFACE PRINTTRACE                                               !
  !******************************************************************************************************************!
  subroutine PrintTrace_void ()
    use Parameters, only: luerr, F001
    implicit none

    write (luerr,F001) trim(procname)

  end subroutine PrintTrace_void
  !******************************************************************************************************************!

end module Debugger
!**********************************************************************************************************************!
