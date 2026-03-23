! #############################################################################
!    Name: Module_GeoCount
!    Purpose: Convert Geostationary Satellite Data Value (Albedo or BT) into
!               Count(0~255).
!
!
! Modify by                                    Akey Chen in CWB  2017/09/11
! #############################################################################
MODULE Module_GeoCount

 private

 character(3) :: GC_Channel
  data GC_Channel /''/ 


 character(200) :: GC_TableDir
  data GC_TableDir  /'./'/

 character(1)   :: GC_TableState
 character(250) :: GC_TableFile
 real           :: GC_TableValue(0:255)


 private :: GC_TableLoad
 public :: GC_Value2Count
 public :: GC_Set_TableDir

CONTAINS

 integer Function GC_Set_TableDir(Dir_Str)
  implicit none
  character(*) :: Dir_Str
  GC_TableDir = Dir_Str
  GC_Set_TableDir = 0
 Return
 End Function GC_Set_TableDir  
 


 Subroutine GC_Value2Count(Values,Counts,Channel_str,error)
  implicit none
  real         :: Values(:,:)
  byte         :: Counts(:,:)
  character(*) :: Channel_str
  integer      :: x, y, dx, dy, c, C0, C1, CC
  integer      :: error, err

  if(GC_Channel.ne.Channel_str)call GC_TableLoad(Channel_str,err)
  if(err.ne.0)then
   write(*,*)'Errir in GC_TableLoad.'
   error = 1
   return
  endif

  
  if(GC_TableState.eq.'R')then
    C0 = 1
    C1 = 255
    CC = 1
  else
    C0 = 254
    C1 = 0
    CC = -1
  endif


!  print*,'akey test'
  dx = size(Values,1)
  dy = size(Values,2)
!  x = 100
!  y = 100
 do x = 1 , dx
 do y = 1 , dy
    Counts(x,y) = 0 
!   print*,C0, C1, CC

    do c = C0, C1, CC
      if(Values(x,y).gt.GC_TableValue(c))Counts(x,y) = c
!      print*,Values(x,y),GC_TableValue(c),Counts(x,y),c
    enddo !c

!    print*,Values(x,y),c

 enddo !x
 enddo !y


!print*,Counts(x,y)

 
 error = 0
 Return
 End Subroutine GC_Value2Count



 Subroutine GC_TableLoad(Channel_str,error)
  implicit none
  character(*)   :: Channel_str
  character(250) :: TableFile
  logical        :: ex
  real           :: v
  integer        :: i, c
  integer        :: error,err
  !error = 1 : Not Exist File.
  !error = 2 : Loading Error.

  TableFile = trim(GC_TableDir)//'Channel_'//trim(Channel_str)//'_Standary.LUT'

  inquire(file=TableFile,exist=ex)
  if(.not.ex)then
    write(*,*)'Not Exist File: ',trim(TableFile)
    error = -1
    return
  endif


  open(21,file=TableFile,status='old')
  do i = 0 , 255
    read(21,*,iostat=err)c,v
    if(err.ne.0)then
      write(*,*)'Loading Error in ',trim(TableFile)
      close(21)
      GC_TableValue = -999.
      error = 2
      return
    endif 
     
    GC_TableValue(c) = v

  enddo
  close(21)


 
  if(GC_TableValue(0).lt.GC_TableValue(255))GC_TableState = 'R' !rise
  if(GC_TableValue(0).gt.GC_TableValue(255))GC_TableState = 'D' !drop
  if(GC_TableValue(0).eq.GC_TableValue(255))then
    write(*,*)'First and Last Value of Table are the same. '
    do i = 0 , 255
      write(*,*)i,GC_TableValue
    enddo
    error = 3
    return
  endif



 GC_Channel = Channel_str
 error = 0
 Return
 End Subroutine GC_TableLoad


End MODULE Module_GeoCount
