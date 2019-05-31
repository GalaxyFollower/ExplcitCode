program fun
implicit none
!**************************************************************************************************!
!               Author          :       Mehdi Zare                                                 !
!               date            :       04/20/2019                                                 !
!               Purpose         :       Calculationg Mean Field average of                         !
!                                       gradients for reference HISTORY                            !
!               Modification    :                                                                  !
!**************************************************************************************************!


integer  ,  parameter                   ::      dp = selected_real_kind(15, 307)
character(len=50)                       ::      file1 = 'MMS_REPLAY_Refimg_100'
character(len=50)                       ::      file2 = 'MMS_ENSEMBLE'
character(len=50)                       ::      file3 = 'MMS_REPLAY_Dlpoly_100'
character(len=256)                      ::      cmdl, str_3, str_4, str_5
character(len=256)                      ::      header(4)
character(len=8), allocatable           ::      clusNum(:), confNum(:)
integer                                 ::      error_flag,alloc_err, ierror                    
integer                                 ::      i,jj,kk,nline1, nline2, nline3, QMatoms, frameRef, frameDL         
real(dp), allocatable, dimension (:)    ::      gradxRef,gradyRef,gradzRef, gradxDL,gradyDL,gradzDL, AveGradX, AveGradY, AveGradZ

! count number of lines in files, MMS_ENSEMBLE, MMS_REPLAY_Refimg_100, and
call countline(file2,nline2)           ! nline2 is the number of lines in MMS_ENSEMPLE file
        if ( error_flag == 1  ) then
          call EXIT(0)           !FLAG
        end if
call countline(file1,nline1)           
        if ( error_flag == 1  ) then
          call EXIT(0)           !FLAG
        end if
call countline(file3,nline3)           ! nline2 is the number of lines in MMS_ENSEMPLE file
        if ( error_flag == 1  ) then
          call EXIT(0)           !FLAG
        end if

QMatoms=nline2-1   ! number of QM atoms is total lines in MMS_ENSEMBLE minus header line 
frameRef=nline1/(4+QMatoms)
frameDL=nline3/(4+QMatoms)

if (frameRef /= frameDL) then
   write(*,*) ' The number of frames in file ', file1 ,' and in', file3, ' are different!!!'
   call EXIT(0)
end if

allocate(gradxRef(QMatoms))
allocate(gradyRef(QMatoms))
allocate(gradzRef(QMatoms))
allocate(gradxDL(QMatoms))
allocate(gradyDL(QMatoms))
allocate(gradzDL(QMatoms))
allocate(AveGradX(QMatoms))
allocate(AveGradY(QMatoms))
allocate(AveGradZ(QMatoms))
allocate(clusNum(QMatoms))
allocate(confNum(QMatoms))

call GradientValues(file1,gradxRef,gradyRef,gradzRef,QMatoms)
call GradientValues(file3,gradxDL,gradyDL,gradzDL,QMatoms)
! CALCULATE THE AVERAGE OF DIFFERNECE BETWEEN ENERGIES
AveGradX=0.0_dp
AveGradY=0.0_dp
AveGradZ=0.0_dp
do i= 1,QMatoms
   AveGradX(i)=gradxRef(i)-gradxDL(i)
   AveGradY(i)=gradyRef(i)-gradyDL(i)
   AveGradZ(i)=gradzRef(i)-gradzDL(i)
end do

! I am gonno write the AveGrad in format like MMS_REPLAY
! obtain the first 4 lines
open(unit=5555, file=file1, status='old', action='read', iostat = ierror)
   do kk=1,4
     read(5555,'(A)', iostat = ierror) header(kk)
        if (ierror /=0 ) then                  !!!!FALG
          write(*,*) " Problem with reading file: ", file1
          call EXIT(0)
        end if
 !     write(*,*) skip  ! FLAG
   end do

   ! Read QM cluster forces
   do jj=1,QMatoms
       read(5555,*, iostat = ierror) clusNum(jj), confNum(jj), str_3, str_4, str_5
        if (ierror /=0 ) then                  !!!!FALG
          write(*,*) " Problem with reading file: ", file1
          call EXIT(0)
        end if
   end do
close(5555)



!Wrtie the data in files named 'GradAVERAGE'
open    ( unit = 500, file = 'GradAVERAGE', status = 'new')
 !write header
 do kk=1,4
    write(500,*) trim(header(kk))
 end do
 
 !write Gradienets and atoms numbers
 do jj=1,QMatoms  
   write   (500, *) '       ', clusNum(jj), confNum(jj), AveGradX(jj), AveGradY(jj), AveGradZ(jj)
 end do
close(500)

!cmdl='head -4  ' // trim('MMS_REPLAY_Dlpoly_100') // ' >> copy'
!call system (trim(cmdl))


deallocate(gradxRef,gradyRef,gradzRef, gradxDL,gradyDL,gradzDL,AveGradX, AveGradY, AveGradZ, stat = alloc_err)
deallocate(clusNum,confNum, stat = alloc_err)
end program fun

!!!!!!!!!!!!!!!! SUBROUTINE FOR READING THE ENERGIES OF FILEs MMS_REPLAYE !!!!!!!!!!!!!!!!!!!!
subroutine GradientValues(filename,gradx,grady,gradz,QMatoms)
implicit none

! Local parameters
integer  ,  parameter                   ::      dp = selected_real_kind(15, 307)
character(len=256)                      ::      skip                                            ! The variable for skipping lines
character(len=256)                      ::      str_1, str_2, str_3, str_4, str_5               ! The variables for skipping strings in a line
integer                                 ::      ii, jj, kk, pp, ierror, alloc_err
integer                                 ::      error_flag                                      !for  subroutines checking
integer                                 ::      nline, frame                                    ! The number of lines in a file! The number of lines in a file
real(dp) ,dimension(QMatoms)            ::      fx,fy,fz

!parameter types and definition
character(len=50)            ,  intent(in)                   ::      filename
integer                      ,  intent(in)                   ::      QMatoms               
real(dp) ,dimension(QMatoms) ,  intent(out)                  ::      gradx,grady,gradz


! Get the number of lines in the files
call countline(filename,nline)
        if ( error_flag == 1  ) then
          call EXIT(0)           !FLAG
        end if
!write(*,*) nline


fx=0.0_dp
fy=0.0_dp
fz=0.0_dp
gradx=0.0_dp
grady=0.0_dp
gradz=0.0_dp

open(unit=77777, file=filename, status='old', action='read', iostat = ierror)
frame=nline/(4+QMatoms) 

!write(*,*) ' The total Number of Frames is: ', frame  !Flag

do ii=1,frame
   !skip the first 4 lines
   do kk=1,4
     read(77777,'(A)', iostat = ierror) skip
        if (ierror /=0 ) then                  !!!!FALG
          write(*,*) " Problem with reading file: ", filename
          error_flag = 1
          call EXIT(0)
        end if
 !     write(*,*) skip  ! FLAG
   end do
   
   ! Read QM cluster forces
   do jj=1,QMatoms
       read(77777,*, iostat = ierror) str_1, str_2, str_3, str_4, str_5
        if (ierror /=0 ) then                  !!!!FALG
          write(*,*) " Problem with reading file: ", filename
          error_flag = 1
          call EXIT(0)
        end if
       read(str_3,*) fx(jj)
       read(str_4,*) fy(jj)
       read(str_5,*) fz(jj)
      !Calculate Gradient
       gradx(jj)=gradx(jj)+(1.0_dp/frame)*(-fx(jj))
       grady(jj)=grady(jj)+(1.0_dp/frame)*(-fy(jj))
       gradz(jj)=gradz(jj)+(1.0_dp/frame)*(-fz(jj))
   end do
 ! write(*,*) fx(37)   !FLAG
end do

!write(*,*) 'Gradx(1) is:', gradx(1)
!write(*,*) 'Shape Grad):' , size(gradx), size(grady), size(gradz)   

close(77777)


end subroutine GradientValues
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

