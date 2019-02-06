!**********************************************************************************************************************!
!This module provides general statistical procedures.                                                                  !
!**********************************************************************************************************************!
module Statistics

  implicit none

  public

  !******************************************************************************************************************!
  !                                               CONTAINED PROCEDURES                                               !
  !                                                                                                                  !
  !'NumberOfCombinations' returns the total number of combinations.                                                  !
  !'NumberOfPermutations' returns the total number of permutations.                                                  !
  !******************************************************************************************************************!

  interface NumberOfCombinations
    module procedure NumberOfCombinations_iX2l_i
  end interface NumberOfCombinations

  interface NumberOfPermutations
    module procedure NumberOfPermutations_iX2l_i
  end interface NumberOfPermutations

  contains

  !******************************************************************************************************************!
  !                                          INTERFACE NUMBEROFCOMBINATIONS                                          !
  !******************************************************************************************************************!
  function NumberOfCombinations_iX2l_i (numk, numn, lrepeat) result (numc)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: bi
    implicit none

    integer,           intent(in) :: numk, numn     !Number of simultaneous picks, available choices.
    logical, optional, intent(in) :: lrepeat        !Repetition allowed?
    integer(bi)                   :: numc

    integer :: numb, ii

    procname = 'Statistics::NumberOfCombinations_iX2l_i'
    if (ltrace) call PrintTrace()

    if ((numk<0) .or. (numn<0)) call PrintError (ekey=5001, lstop=.true.)

    if (numk>numn) then
      numc = 0
    else if (numk==numn) then
      numc = 1
    else
      !Determine the binomial coefficient.
      numb = numn + 1
      if (present(lrepeat)) then
        if (lrepeat) numb = numn + numk
      end if

      !Calculate number of combinations.
      numc = 1
      do ii = 1, numk
        numc = numc * (numb-ii) / ii
      end do
    end if

  end function NumberOfCombinations_iX2l_i
  !******************************************************************************************************************!

  !******************************************************************************************************************!
  !                                          INTERFACE NUMBEROFPERMUTATIONS                                          !
  !******************************************************************************************************************!
  function NumberOfPermutations_iX2l_i (numk, numn, lrepeat) result (nump)
    use Debugger,   only: PrintError, procname, ltrace, PrintTrace
    use Parameters, only: bi
    implicit none

    integer,           intent(in) :: numk, numn     !Number of simultaneous picks, available choices.
    logical, optional, intent(in) :: lrepeat        !Repetition allowed?
    integer(bi)                   :: nump

    integer :: numb, ii

    procname = 'Statistics::NumberOfPermutations_iX2l_i'
    if (ltrace) call PrintTrace()

    if ((numk<0) .or. (numn<0)) call PrintError (ekey=5001, lstop=.true.)

    if (numk>numn) then
      nump = 0
    else
      !Calculate number of permutations with repetition.
      if (present(lrepeat)) then
        if (lrepeat) then
          nump = numn ** numk
          return
        end if
      end if

      !Calculate number of permutations without repetition.
      nump = 1
      numb = numn + 1
      do ii = 1, numk
        nump = nump * (numb-ii)
      end do
    end if

  end function NumberOfPermutations_iX2l_i
  !******************************************************************************************************************!

end module Statistics
!**********************************************************************************************************************!
