!**********************************************************************************************************************!
!This module contains definitions/values of global constants. All members must be saved for the life-time of the       !
!program and protected from changes elsewhere in the code.                                                             !
!**********************************************************************************************************************!
module Parameters

  use iso_fortran_env, only: ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT
  implicit none

  public
  save

  !Intrinsic datatypes.
  integer, parameter :: bi = selected_int_kind (12)           !Datatype for big integers.
  integer, parameter :: dp = selected_real_kind (12, 200)     !Datatype for double-precision real numbers.
  integer, parameter :: sl = 256                              !Default length for character variables (files).
  integer, parameter :: pl = sl * 16                          !Default length for character variables (parameters).

  !Pre-connected logical file units.
  integer, parameter :: luerr = ERROR_UNIT
  integer, parameter :: luinp = INPUT_UNIT
  integer, parameter :: luout = OUTPUT_UNIT

  !Number constants.
  real(dp), parameter :: zero = 0.0_dp
  real(dp), parameter :: one  = 1.0_dp

  !Conversion factors and physical constants.
  real(dp), parameter :: PI       = 3.141592653589793_dp ! Pi
  real(dp), parameter :: B2A      = 0.52917721092_dp     ! 1 Angstrom = X Bohr.
  real(dp), parameter :: A2B      = one / B2A            ! 1 Bohr = X Angstrom.
  real(dp), parameter :: AmuKg    = 1.660538921e-27_dp   ! 1 amu = X kg
  real(dp), parameter :: H2eV     = 27.21138505_dp       ! 1 eV = X Hartree.
  real(dp), parameter :: eV2H     = one / H2eV           ! 1 Hartree = X eV.
  real(dp), parameter :: eVoA2HoB = B2A * eV2H           ! 1 Hartree/Bohr = X eV/Angstrom.
  real(dp), parameter :: HoB2eVoA = one / eVoA2HoB       ! 1 eV/Angstrom = X Hartree/Bohr.
  real(dp), parameter :: BZeVoK   = 8.6173324e-5_dp      ! Boltzmann constant (eV/Kelvin).
  real(dp), parameter :: BZHoK    = BZeVoK * eV2H        ! Boltzmann constant (Hartree/Kelvin).

  !Precisions and tolerances.
  real(dp), parameter :: dtol = 1.0e-12_dp     !Tolerance for matching real numbers (up to double precision).
  real(dp), parameter :: stol = 1.0e-8_dp      !Tolerance for matching real numbers (up to single precision).
  real(dp), parameter :: mtol = 1.0e-5_dp      !Tolerance for matching atomic coordinates (Bohr).

  !Format specifiers.
  character(len=*), parameter :: F001 = '(A)'
  character(len=*), parameter :: F002 = '(2A)'
  character(len=*), parameter :: F005 = '(5A)'
  character(len=*), parameter :: F011 = '(A,I6)'
  character(len=*), parameter :: F012 = '(A,I10)'
  character(len=*), parameter :: F021 = '(A,3F20.14)'
  character(len=*), parameter :: F101 = '(I6)'
  character(len=*), parameter :: F102 = '(I6.6)'
  character(len=*), parameter :: F103 = '(I6,A)'
  character(len=*), parameter :: F104 = '(I6,3F20.14)'
  character(len=*), parameter :: F111 = '(I10)'
  character(len=*), parameter :: F112 = '(I10,F20.14)'
  character(len=*), parameter :: F113 = '(2I10)'
  character(len=*), parameter :: F114 = '(2I10,3F20.14)'
  character(len=*), parameter :: F115 = '(3I10)'
  character(len=*), parameter :: F201 = '(F14.6)'
  character(len=*), parameter :: F211 = '(F14.10)'
  character(len=*), parameter :: F221 = '(F20.14)'
  character(len=*), parameter :: F222 = '(3F20.14)'
  character(len=*), parameter :: F223 = '(3F20.14,A)'
  character(len=*), parameter :: F231 = '(F23.14)'
  character(len=*), parameter :: F232 = '(3F23.14)'
  character(len=*), parameter :: F233 = '(3F23.14,A)'

end module Parameters
!**********************************************************************************************************************!
