!**********************************************************************************************************************!
!This module provides the definition and supporting functionality for a stopwatch object for time-keeping purposes.    !
!**********************************************************************************************************************!
module Timers

  use Parameters, only: bi, dp, sl
  implicit none

  !******************************************************************************************************************!
  !                                         STOPWATCH (SW) OBJECT DEFINITION                                         !
  !                                                                                                                  !
  !                                                 MEMBER ELEMENTS                                                  !
  !                                                                                                                  !
  !'name' is a descriptive name for the timer.                                                                       !
  !'active' returns true if the timer is currently running.                                                          !
  !'max' and 'rate' are system-dependent properties. 'rate' gives the resolution of the timer and 'max' is the       !
  !  number of ticks before the counter is reset.                                                                    !
  !'itck' and 'ftck' are tick-counts recorded when the timer is started/stopped.                                     !
  !                                                                                                                  !
  !                                                MEMBER PROCEDURES                                                 !
  !                                                                                                                  !
  !'Start' reads initial counters and starts the timer.                                                              !
  !'Stop' reads final counters and stops the timer.                                                                  !
  !'Reset' (re)reads initial counters without starting/stopping the timer.                                           !
  !'GetTime' returns the time measured by the timer in seconds without starting/stopping it.                         !
  !'Print' prints the name and measured time in seconds.                                                             !
  !******************************************************************************************************************!
  type StopWatch
      character(len=sl), public  :: name   = 'Global Timer'
      logical,           public  :: active = .false.
      integer(bi),       private :: max    = 0
      integer(bi),       private :: rate   = 0
      integer(bi),       private :: itck   = 0
      integer(bi),       private :: ftck   = 0
    contains
      procedure, public :: Start   => Start_sw
      procedure, public :: Stop    => Stop_sw
      procedure, public :: Reset   => Reset_sw
      procedure, public :: GetTime => GetTime_sw_dp
      procedure, public :: Print   => Print_sw
  end type StopWatch

  contains

  !******************************************************************************************************************!
  !                                               STOPWATCH PROCEDURES                                               !
  !******************************************************************************************************************!
  subroutine Start_sw (self)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(StopWatch), intent(inout) :: self

    procname = 'Timers::Start_sw'
    if (ltrace) call PrintTrace()

    !Start the timer. Do not reset.
    call system_clock (count_max=self%max, count_rate=self%rate)
    if (.not. self%active) then
      call system_clock (count=self%itck)
      self%ftck   = self%itck
      self%active = .true.
    end if

  end subroutine Start_sw

  subroutine Stop_sw (self)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(StopWatch), intent(inout) :: self

    procname = 'Timers::Stop_sw'
    if (ltrace) call PrintTrace()

    !Stop the timer. Do not reset.
    call system_clock (count_max=self%max, count_rate=self%rate)
    if (self%active) then
      call system_clock (count=self%ftck)
      self%active = .false.
    end if

  end subroutine Stop_sw

  subroutine Reset_sw (self)
    use Debugger, only: PrintError, procname, ltrace, PrintTrace
    implicit none

    class(StopWatch), intent(inout) :: self

    procname = 'Timers::Reset_sw'
    if (ltrace) call PrintTrace()

    !Reset all counters.
    call system_clock (count_max=self%max, count_rate=self%rate)
    self%itck = 0
    self%ftck = 0

    !Re-initialize all counters if the timer was previously running.
    if (self%active) then
      call system_clock (count=self%itck)
      self%ftck = self%itck
    end if

  end subroutine Reset_sw

  function GetTime_sw_dp (self) result (time)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: dp, zero
    implicit none

    class(StopWatch), intent(inout) :: self
    real(dp)                        :: time

    procname = 'Timers::GetTime_sw_dp'
    if (ltrace) call PrintTrace()

    if (self%max>0) then
      !Update counters if the timer is currently running. Do not reset/stop.
      if (self%active) call system_clock (count=self%ftck)

      !Calculate elapsed time in seconds since the timer was first started.
      time = (real(self%ftck,dp)-real(self%itck,dp)) / real(self%rate,dp)
      if (time<zero) time = time + real(self%max,dp)/real(self%rate,dp)

    !If the timer was never accessed before.
    else
      time = zero
    end if

  end function GetTime_sw_dp

  subroutine Print_sw (self)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: sl, luerr, F005, F201
    implicit none

    class(StopWatch), intent(inout) :: self

    character(len=sl) :: buff

    procname = 'Timers::Print_sw'
    if (ltrace) call PrintTrace()

    !Calculate elapsed time and print to output.
    write (buff,F201) self%GetTime()
    if (self%active) then
      write (luerr,F005) 'Elapsed time (', trim(self%name), ') = ', trim(adjustl(buff)),  &
        ' seconds (and counting)'
    else
      write (luerr,F005) 'Elapsed time (', trim(self%name), ') = ', trim(adjustl(buff)), ' seconds'
    end if

  end subroutine Print_sw
  !******************************************************************************************************************!

end module Timers
!**********************************************************************************************************************!
