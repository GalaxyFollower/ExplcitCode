program fun
implicit none
!**************************************************************************************************!
!               Author          :       Mehdi Zare                                                 !
!               date            :       04/15/2019                                                 !
!               Purpose         :       Calculationg Mean Field average for                        !
!                                       reference HISTORY                                          !
!               Modification    :                                                                  !
!**************************************************************************************************!

integer  ,  parameter                   ::      dp = selected_real_kind(15, 307)
character(len=30)                       ::      file1 = 'MMS_REPLAY_Refimg_100'
character(len=30)                       ::      file2 = 'MMS_ENSEMBLE'
character(len=30)                       ::      file3 = 'MMS_REPLAY_Dlpoly_100'
integer                                 ::      error_flag,alloc_err                    
integer                                 ::      i, nline2, nline3, QMatoms             
real(dp), dimension (100)               ::      energy1, energy3
real(dp)                                ::      sumdiff,diff, Avediff
! count number of lines in files, MMS_ENSEMBLE, MMS_REPLAY_Refimg_100, and
call countline(file2,nline2)           ! nline2 is the number of lines in MMS_ENSEMPLE file
        if ( error_flag == 1  ) then
          call EXIT(0)           !FLAG
        end if
QMatoms=nline2-1   ! number of QM atoms is total lines in MMS_ENSEMBLE minus header line 
call energyValues(file1,energy1,QMatoms)
        if ( error_flag == 1  ) then
          call EXIT(0)           !FLAG
        end if
call energyValues(file3,energy3,QMatoms)
        if ( error_flag == 1  ) then
          call EXIT(0)           !FLAG
        end if
!write(*,*) energy1
!write(*,*) energy3
! CALCULATE THE AVERAGE OF DIFFERNECE BETWEEN ENERGIES
sumdiff=0
do i= 1,100
   diff=energy1(i)-energy3(i)
   sumdiff=sumdiff+diff
end do
Avediff=(0.01_dp)*(sumdiff)
!write(*,*) Avediff

!Wrtie the data in files named 'AVERAGE'
open    ( unit = 500, file = 'AVERAGE', status = 'new')
write   (500, *)  Avediff
close(500)



end program fun

!!!!!!!!!!!!!!!! SUBROUTINE FOR READING THE ENERGIES OF FILEs MMS_REPLAYE !!!!!!!!!!!!!!!!!!!!
subroutine energyValues(filename,energy,QMatoms)
implicit none

! Local parameters
integer  ,  parameter                   ::      dp = selected_real_kind(15, 307)
character(len=256)                      ::      skip                                            ! The variable for skipping lines
character(len=256)                      ::      str_1, str_2, str_3                             ! The variables for skipping strings in a line
real(dp)                                ::      num
integer                                 ::      ii, jj, kk, pp, ierror, alloc_err
integer                                 ::      error_flag                                      !for  subroutines checking
integer                                 ::      nline                                           ! The number of lines in a file! The number of lines in a file
integer                                 ::      enline                                          ! Energy line 

!parameter types and definition
character(len=30)         ,  intent(in)                   ::      filename
integer                   ,  intent(in)                   ::      QMatoms               
real(dp), dimension (100) ,  intent(out)                  ::      energy


! Get the number of lines in the files
call countline(filename,nline)
        if ( error_flag == 1  ) then
          call EXIT(0)           !FLAG
        end if


!Read the energies in the file
open(unit=77777, file=filename, status='old', action='read', iostat = ierror)

! energy line starts from line 3, skip the first two lines
do ii=1,2
   read(77777,'(A)', iostat = ierror) skip
        if (ierror /=0 ) then                  !!!!FALG
          write(*,*) " Problem with reading file: ", filename
          error_flag = 1
          call EXIT(0)
        end if
   !write(*,*) skip
end do

jj=3    ! the energy lines starts from line 3
kk=0    ! the index that controls which energy
pp=1    ! the enegy line in file energy
do ii=3,nline
   enline=jj+kk*(4+QMatoms)   ! 4 is the number of header lines     
   if (ii == enline) then     
        read(77777,*, iostat = ierror) str_1, str_2, str_3
                if (ierror /=0 ) then                  !!!!FALG
                write(*,*) " Problem with reading file: ", filename
                error_flag = 1
                call EXIT(0)
                end if
        read(str_3,*) num
        energy(pp)=num
       ! write(*,*) energy(pp)
        pp=pp+1
        kk=kk+1

   else 
        read(77777,'(A)', iostat = ierror) skip
                if (ierror /=0 ) then                  !!!!FALG
                 write(*,*) " Problem with reading file: ", filename
                 error_flag = 1
                call EXIT(0)
                 end if
   end if
end do

close(77777)


end subroutine energyValues
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! END OF SUBROUTINE energyValues !!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SUBROUTNE FOR COUNTING lines IN A FILE!!!!!!!!!!!!!
subroutine countline(filename, nline)
implicit none

integer  ,  parameter                   ::      dp = selected_real_kind(15, 307)

!local variable
integer                                 ::      ierror,  error_flag

!parameter types and definition
character(len=30)    ,  intent(in)                  ::      filename
integer              ,  intent(out)                 ::      nline


error_flag = 0

!read the # of lines in the file 
nline=0
open(1000, file=filename, status='old', action='read', iostat = ierror)
do
 read (1000, *, end=10)
        if (ierror /=0 ) then                  !!!!FALG
          write(*,*) " Problem with counting lines in file:  " , filename
           error_flag = 1
          call EXIT(0)
        end if
 nline=nline+1
end do
10 close(1000)

end subroutine countline
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! END OF SUBROUTINE COUNTLINE !!!!!!!!!!!!!!!!!!!!!

