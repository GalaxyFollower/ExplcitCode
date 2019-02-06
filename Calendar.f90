!**********************************************************************************************************************!
!This module provides the definition and supporting functionality for a stopwatch object for time-keeping purposes.    !
!**********************************************************************************************************************!
module Calendar

  implicit none

  !******************************************************************************************************************!
  !                                       CALENDARDATE (CD) OBJECT DEFINITION                                        !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'year', 'mon', and 'day' specify a calendar date.                                                                 !
  !'JDN' is the Julian Day Number (only integer part).                                                               !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'Read' reads a date from string.                                                                                  !
  !'IsValid' returns true for a valid calendar date.                                                                 !
  !'Print' returns a string representation of date in specified format.                                              !
  !******************************************************************************************************************!
  type CalendarDate
      integer, public :: day, mon, year
      integer, public :: JDN
    contains
      procedure, public :: Read    => Read_sCD
      procedure, public :: IsValid => IsValid_CD_l
!      procedure, public :: Print   => Print_sCD_s
  end type CalendarDate

  !******************************************************************************************************************!
  !                                               CONTAINED PROCEDURES                                               !
  !                                                                                                                  !
  !'DateIsValid' returns true for a valid calendar date.                                                             !
  !'JulianDayNumber' calculates Julian Day Number (overloaded).                                                      !
  !******************************************************************************************************************!

  interface DateIsValid
    module procedure DateIsValid_iX3_l
  end interface DateIsValid

  interface JulianDayNumber
    module procedure JulianDayNumber_void_i
    module procedure JulianDayNumber_iX3_i
  end interface JulianDayNumber

  contains

  !******************************************************************************************************************!
  !                                             CALENDARDATE PROCEDURES                                              !
  !******************************************************************************************************************!
  subroutine Read_sCD (self, line)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(CalendarDate), intent(inout) :: self
    character(len=*),    intent(in)    :: line

    integer :: code

    procname = 'Timers::Read_sCD'
    if (ltrace) call PrintTrace()

    !Read the date in YYYY-MM-DD format.
    read (line,*,iostat=code) self%year, self%mon, self%day
      if (code>0) call PrintError (ekey=2111, lstop=.true.)

    self%JDN = JulianDayNumber (year=self%year, mon=self%mon, day=self%day)

  end subroutine Read_sCD

  function IsValid_CD_l (self) result (ldate)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(CalendarDate), intent(in) :: self
    logical                         :: ldate

    procname = 'Timers::IsValid_CD_l'
    if (ltrace) call PrintTrace()

    ldate = DateIsValid (year=self%year, mon=self%mon, day=self%day)

  end function IsValid_CD_l
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                              INTERFACE DATEISVALID                                               !
  !******************************************************************************************************************!
  function DateIsValid_iX3_l (year, mon, day) result (ldate)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    integer, intent(in) :: day, mon, year
    logical             :: ldate

    integer :: dmon(12)     !Number of days in each month.

    procname = 'Timers::DateIsValid_iX3_l'
    if (ltrace) call PrintTrace()

    !Start by assuming that the specified date is indeed valid. Then check all components one-by-one. Any failed
    !check will cause immediate return.
    ldate = .true.

    !Limits for validation of year are arbitrarily set.
    if ((year<0) .or. (year>2100)) then
      ldate = .false.

    !Validate month only if year was OK.
    else
      if ((mon<0) .or. (mon>12)) then
        ldate = .false.

      !Validate day only if both year and month were OK. Adjust for leap year before validation.
      else
        dmon = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
        if (mod(year,4)==0)   dmon(2) = 29
        if (mod(year,100)==0) dmon(2) = 28
        if (mod(year,400)==0) dmon(2) = 29
        if ((day<0) .or. (day>dmon(mon))) ldate = .false.
      end if
    end if

  end function DateIsValid_iX3_l
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                            INTERFACE JULIANDAYNUMBER                                             !
  !******************************************************************************************************************!
  function JulianDayNumber_void_i () result (JDN)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    integer :: JDN

    integer :: date(8)

    procname = 'Timers::JulianDayNumber_void_i'
    if (ltrace) call PrintTrace()

    !Get current date from system timer. Then calculate Julian Day Number for this date.
    call date_and_time (values=date)
    JDN = JulianDayNumber (year=date(1), mon=date(2), day=date(3))

  end function JulianDayNumber_void_i

  function JulianDayNumber_iX3_i (year, mon, day) result (JDN)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    integer, intent(in) :: day, mon, year
    integer             :: JDN

    integer :: aa, mm, yy

    procname = 'Timers::JulianDayNumber_iX3_i'
    if (ltrace) call PrintTrace()

    !Calculate Julian Day Number (only integer part) for the specified date.
    if (.not. DateIsValid(year=year,mon=mon,day=day)) call PrintError (ekey=1001, lstop=.true.)
    aa  = (14-mon) / 12
    mm  = mon + 12*aa - 3
    yy  = year + 4800 - aa
    JDN = day + (153*mm+1)/5 + 365*yy + yy/4 - yy/100 + yy/400 -32045

  end function JulianDayNumber_iX3_i
  !******************************************************************************************************************!

end module Calendar
!**********************************************************************************************************************!
