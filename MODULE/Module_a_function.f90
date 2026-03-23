!###########################################################################
!  Name: Module_a_function
!  Purpose: Some Function to use
!
!  Add A_Dismantle                        Akey Chen in CWB  2016/08/08
!  Add A_DirName                          Akey Chen in CWB  2016/11/11
!  Add A_JDay, A_TimeBetween              Akey Chen in CWB  2016/12/16
!###########################################################################


      MODULE Module_a_function
      IMPLICIT NONE

      character(250) :: CMD_CONVERT
        data CMD_CONVERT /'convert'/

       Interface A_File_Write
         module procedure A_File_Write_Char
         module procedure A_File_Write_Char2
         module procedure A_File_Write_Char3
         module procedure A_File_Write_Byte
         module procedure A_File_Write_Byte2
         module procedure A_File_Write_Byte3
         module procedure A_File_Write_Real2
         module procedure A_File_Write_2INT2
       End Interface

       Interface A_File_Read
         module procedure A_File_Read_Char
         module procedure A_File_Read_Char2
         module procedure A_File_Read_Char3
         module procedure A_File_Read_Byte
         module procedure A_File_Read_Byte2
         module procedure A_File_Read_Byte3
       End Interface


      public :: Version_Module_a_function
      public :: A_File_Write
      public :: A_File_Read
      public :: A_Set_Convert
      public :: A_BaseName
      public :: A_DirName
!      public :: A_Congrid   !pgf90 unknow error
      public :: A_Timecal
      public :: A_JDay
      public :: A_Fed_day
      public :: A_Dismantle
      public :: A_Dismantle2
      public :: A_Reverse3
      public :: A_STR_2_Integer
      public :: A_TimeBetween
      public :: A_FileExist



      CONTAINS
       Function Version_Module_a_function()
       implicit none
       character(50) :: Version_Module_a_function
       Version_Module_a_function = 'Module_a_function = V2.3.2'
                      ! 2.3.1: Add A_JDay
                      ! 2.3.2: Add A_TimeBetween
                      ! 2.3.3: Fit ulittle Bug in A_BaseName A_DirName
       return
       End Function


       Function A_Set_Convert(INPUT)
       implicit none
       integer      :: A_Set_Convert
       character(*) :: INPUT

       A_Set_Convert = 0
       if(INPUT.eq.'')then
         A_Set_Convert = 1
         write(*,*)'Error Set Convert Commond: ',trim(INPUT)
         return
       endif

       CMD_CONVERT = trim(INPUT)
       End Function A_Set_Convert


       Function A_BaseName(INPUT)
       Implicit none
       Integer :: LEN0
       Character(250) :: A_BaseName
       Character(*)   :: INPUT
       LEN0 = 1
       A_BaseName = INPUT
       if(A_BaseName(250:250).ne.' ')then
         write(*,*)'Too long string.....',trim(INPUT)
         stop
       endif
       do while (LEN0.ne.0)
        LEN0 = INDEX(A_BaseName,'/')
        if(LEN0 .ne. 0)then
         A_BaseName = A_BaseName(LEN0+1:)
        endif
       enddo
       return
       End Function

       Function A_DirName(INPUT)
       Implicit none
       Integer :: LEN0
       Character(250) :: A_DirName
       Character(*)   :: INPUT
       LEN0 = 1
       A_DirName = ''
       LEN0 = index(INPUT,'/',.true.)
       if(LEN0.gt.0)then
         A_DirName = INPUT(1:LEN0-1)
       endif
       if(A_DirName(250:250).ne.' ')then
         write(*,*)'Too long string.....',trim(INPUT)
         stop
       endif

       return
       End Function A_DirName




       Integer Function A_File_Write_Char(Write_FileName,Array)
        implicit none
!        integer :: num_bytes
        character(1),dimension(:) :: Array
        character(*) :: Write_FileName
        open(31,file=Write_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        write(31,rec=1,iostat=A_File_Write_Char)Array
        close(31)
       Return
       End Function A_File_Write_Char

       Integer Function A_File_Write_Byte(Write_FileName,Array)
        implicit none
!        integer :: num_bytes
        Byte,dimension(:) :: Array
        character(*) :: Write_FileName
        open(31,file=Write_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        write(31,rec=1,iostat=A_File_Write_Byte)Array
        close(31)
       Return
       End Function A_File_Write_Byte

       Integer Function A_File_Write_Char2(Write_FileName,Array)
        implicit none
        character(*)        :: Write_FileName
        character(1),dimension(:,:) :: Array
        open(31,file=Write_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        write(31,rec=1,iostat=A_File_Write_Char2)Array
        close(31)
       Return
       End Function A_File_Write_Char2

       Integer Function A_File_Write_Byte2(Write_FileName,Array)
        implicit none
        character(*)        :: Write_FileName
        Byte,dimension(:,:) :: Array
        open(31,file=Write_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        write(31,rec=1,iostat=A_File_Write_Byte2)Array
        close(31)
       Return
       End Function A_File_Write_Byte2

       Integer Function A_File_Write_Char3(Write_FileName,Array)
        implicit none
        character(*)        :: Write_FileName
        character(1),dimension(:,:,:) :: Array
        open(31,file=Write_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        write(31,rec=1,iostat=A_File_Write_Char3)Array
        close(31)
       Return
       End Function A_File_Write_Char3

       Integer Function A_File_Write_Byte3(Write_FileName,Array)
        implicit none
        character(*)        :: Write_FileName
        Byte,dimension(:,:,:) :: Array
        open(31,file=Write_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        write(31,rec=1,iostat=A_File_Write_Byte3)Array
        close(31)
       Return
       End Function A_File_Write_Byte3

       Integer Function A_File_Write_Real2(Write_FileName,Array)
        implicit none
        character(*)        :: Write_FileName
        Real,dimension(:,:) :: Array
        open(31,file=Write_FileName,form='unformatted',access='direct', &
             recl=4*size(Array))
        write(31,rec=1,iostat=A_File_Write_Real2)Array
        close(31)
       Return
       End Function A_File_Write_Real2

       Integer Function A_File_Write_2INT2(Write_FileName,Array)
        implicit none
        character(*)              :: Write_FileName
        integer(2),dimension(:,:) :: Array
        open(31,file=Write_FileName,form='unformatted',access='direct', &
             recl=2*size(Array))
        write(31,rec=1,iostat=A_File_Write_2INT2)Array
        close(31)
       Return
       End Function A_File_Write_2INT2









       Integer Function A_File_Read_Char(Read_FileName,Array)
        implicit none
        character(1),dimension(:) :: Array
        character(*) :: Read_FileName
        open(31,file=Read_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        read(31,rec=1,iostat=A_File_Read_Char)Array
        close(31)
       Return
       End Function A_File_Read_Char

       Integer Function A_File_Read_Char2(Read_FileName,Array)
        implicit none
        character(1),dimension(:,:) :: Array
        character(*) :: Read_FileName
        open(31,file=Read_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        read(31,rec=1,iostat=A_File_Read_Char2)Array
        close(31)
       Return
       End Function A_File_Read_Char2

       Integer Function A_File_Read_Char3(Read_FileName,Array)
        implicit none
        character(1),dimension(:,:,:) :: Array
        character(*) :: Read_FileName
        open(31,file=Read_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        read(31,rec=1,iostat=A_File_Read_Char3)Array
        close(31)
       Return
       End Function A_File_Read_Char3


       Integer Function A_File_Read_Byte(Read_FileName,Array)
        implicit none
        character(*)      :: Read_FileName
        byte,dimension(:) :: Array
        open(31,file=Read_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        read(31,rec=1,iostat=A_File_Read_Byte)Array
        close(31)
       Return
       End Function A_File_Read_Byte

       Integer Function A_File_Read_Byte2(Read_FileName,Array)
        implicit none
        character(*)        :: Read_FileName
        byte,dimension(:,:) :: Array
        open(31,file=Read_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        read(31,rec=1,iostat=A_File_Read_Byte2)Array
        close(31)
       Return
       End Function A_File_Read_Byte2

       Integer Function A_File_Read_Byte3(Read_FileName,Array)
        implicit none
        character(*)          :: Read_FileName
        byte,dimension(:,:,:) :: Array
        open(31,file=Read_FileName,form='unformatted',access='direct', &
             recl=size(Array))
        read(31,rec=1,iostat=A_File_Read_Byte3)Array
        close(31)
       Return
       End Function A_File_Read_Byte3



!       Real Function A_Congrid(Array,xs,ys)
!        implicit none
!        integer :: x,y,x0,y0,xs,ys
!        real,dimension(xs,ys) :: A_Congrid
!        real,dimension(:,:)   :: Array
!        integer :: delta_X0, delta_Y0
!        integer :: delta_X,  delta_Y
!
!        delta_X0 = size(Array,1) - 1
!        delta_Y0 = size(Array,2) - 1
!        delta_X  = xs - 1
!        delta_Y  = ys - 1
!        do x = 1 , xs
!        do y = 1 , ys
!           x0 = (x-1) * delta_X0 / delta_X + 1
!           y0 = (y-1) * delta_Y0 / delta_Y + 1
!           A_Congrid(x,y) = Array(x0,y0)
!        enddo
!        enddo
!       Return
!       End Function A_Congrid


!########################### Time_Cal.f ###############################
!     Program for Compute day and time.
!                                         akey 2012/11/20
!                               Modify by akey 2013/06/26
! Modify for TimeFormat = 3               akey Chen in CWB  2016/04/11
!######################################################################
      Character(15) Function A_Timecal(time1,timec)
      integer :: y1,m1,d1,h1,mi1,se1 !input
      integer :: yc,mc,dc,hc,mic,sec !change value
      integer :: md(0:12),len0
      integer :: upvalue
      integer :: TimeFormat
      integer :: len_time
!      character*15 time1,timec
      character(*) :: time1,timec
!      character :: way,c*13,w
      character :: c*13,w
      logical   :: CVALUE
!=====================================================
      data md /31,31,28,31,30,31,30,31,31,30,31,30,31/

      A_Timecal = '-1'
!========= input time information ====================
      len0 = len_trim(time1)
      if(len0.eq.0)then
       print*,'Input time: YYYYMMDDHHmmss (ex: 20090101)'
       print*,'Input time: YYYY-MM-DD_HHmm (ex: 2015-10-28_0200)'
       print*,'Input time: YYYY-MM-DD_HH   (ex: 2015-10-28_02)'
       print*,'Input time: YYYY-MM-DD      (ex: 2015-10-28)'
       read(*,*)time1
      endif
!========== Check Input Time Format =================
      A_Timecal = ''
      y1 = 0
      m1 = 1
      d1 = 1
      h1 = 0
      mi1 = 0
      se1 = 0

!   Check ERROR Time Fornat:
      len_time = len_trim(time1)

      if(len_time.eq.15 .and. &
         time1(5:5)  .eq.'-' .and. &
         time1(8:8)  .eq.'-' .and. &
         time1(11:11).eq.'_') then
       TimeFormat = 1
       read(time1,'(i4,1x,i2,1x,i2,1x,2i2)')y1,m1,d1,h1,mi1

      elseif(len_time.eq.13 .and. &
         time1(5:5)  .eq.'-' .and. &
         time1(8:8)  .eq.'-' .and. &
         time1(11:11).eq.'_') then
       TimeFormat = 3
       read(time1,'(i4,1x,i2,1x,i2,1x,2i2)')y1,m1,d1,h1

      elseif(len_time.eq.10 .and. &
         time1(5:5)  .eq.'-' .and. &
         time1(8:8)  .eq.'-')then
       TimeFormat = 4
       read(time1,'(i4,1x,i2,1x,i2,1x,2i2)')y1,m1,d1


      else

       TimeFormat = 2
       len0 = len_trim(time1)
       if(len0.eq.4)then
         read(time1,'(i4,5i2)')y1
       elseif(len0.eq.6)then
         read(time1,'(i4,5i2)')y1,m1
       elseif(len0.eq.8)then
         read(time1,'(i4,5i2)')y1,m1,d1
       elseif(len0.eq.10)then
         read(time1,'(i4,5i2)')y1,m1,d1,h1
       elseif(len0.eq.12)then
         read(time1,'(i4,5i2)')y1,m1,d1,h1,mi1
       elseif(len0.eq.14)then
         read(time1,'(i4,5i2)')y1,m1,d1,h1,mi1,se1
       else
        print*,"TIME_CAL: You have to input with right format."
        stop
       endif

      endif !Time Format

!      write(*,'(i4,5(a,i2.2))')y1,"/",m1,"/",d1," ",h1,":",mi1,":",se1

!   Check ERROR Time Value:
      CVALUE = .false.
      call A_Fed_day(y1,md(2))

      if(y1.lt.1)then
        print*,"TIME_CAL: Error Year:",y1

      elseif(m1.gt.12 .or. m1.lt.1)then
        print*,"TIME_CAL: Error Month:",m1

      elseif(d1.gt.md(m1) .or. d1.lt.1)then
        print*,"TIME_CAL: Error Day:",d1,"Month:(",m1,")"

      elseif(h1 .gt.24 .or. h1 .lt.0)then
        print*,"TIME_CAL: Error Hour:",h1

      elseif(mi1.gt.59 .or. mi1.lt.0)then
        print*,"TIME_CAL: Error Minute:",mi1

      elseif(se1.gt.59 .or. se1.lt.0)then
        print*,"TIME_CAL: Error Second:",se1

      else
        CVALUE = .true.

      endif

      if(.not.CVALUE)stop
!====================================================
       len0 = len_trim(timec)
       if(len0.lt.2)then
        print*,'change time:  ( ex:  +1Y <year> )'
        print*,'change time:  ( ex:  -3D <day> )'
        print*,'change time:  ( ex:  +5h <hour> )'
        print*,'change time:  ( ex: -10m <minute> )'
        read(*,*)timec
       endif
!====================================================
      yc=0
      mc=0
      dc=0
      hc=0
      mic=0
      sec=0

      len0=len_trim(timec)
!      way=timec(1:1)
      c=timec(1:len0-1)
      w=timec(len0:len0)

      if(w.eq.'Y')read(c,'(i13)')yc
      if(w.eq.'M')read(c,'(i13)')mc
      if(w.eq.'D')read(c,'(i13)')dc
      if(w.eq.'h')read(c,'(i13)')hc
      if(w.eq.'m')read(c,'(i13)')mic
      if(w.eq.'s')read(c,'(i13)')sec

      select case(w)
       case('Y') ; read(c,'(i13)')yc
       case('M') ; read(c,'(i13)')mc
       case('D') ; read(c,'(i13)')dc
       case('h') ; read(c,'(i13)')hc
       case('m') ; read(c,'(i13)')mic
       case('s') ; read(c,'(i13)')sec
       case default
         write(*,*)'Unknow unit of Time:',w
         return
      end select



!      print*,mc,c,sec

!===== Compute interval time before time loop ==============

!  Second:
      upvalue = int(sec / 60)
      mic = mic + upvalue
      sec = mod(sec,60)
!      print*,'H:',hc,' Minute:',mic,' Sec:',Sec,' V:',upvalue

!  Minute:
      upvalue = int(mic / 60)
      mic = mod(mic,60)
      hc = hc + upvalue
!      print*,'H:',hc,' Minute:',mic,' Sec:',Sec,' V:',upvalue

!==========================================================
      m1=m1+mc

      if(m1.gt.12)then
       yc=yc+m1/12
       m1=mod(m1,12)
      elseif(m1.le.0)then
       yc=yc+m1/12-1
       m1=12+mod(m1,12)
      endif

       y1  =  y1 +  yc
       d1  =  d1 +  dc
       h1  =  h1 +  hc
       mi1 = mi1 + mic
       se1 = se1 + sec
!====================================================+
       do while(  h1.lt.0 .or.  h1.gt.23 .or. &
                 mi1.lt.0 .or. mi1.gt.59 .or. &
                 se1.lt.0 .or. se1.gt.59  )

      ! SECONDS calculate
       if(se1.lt.0)then
        se1=se1+60
        mi1=mi1-1
       elseif(se1.gt.23)then
        se1=se1-60
        mi1=mi1+1
       endif

      ! MINUTE calculate
       if(mi1.lt.0)then
        mi1=mi1+60
        h1=h1-1
       elseif(mi1.gt.23)then
        mi1=mi1-60
        h1=h1+1
       endif


      ! HOUR calculate
       if(h1.lt.0)then
        h1=h1+24
        d1=d1-1
       elseif(h1.gt.23)then
        h1=h1-24
        d1=d1+1
       endif

      ! DAY calculate
       call A_Fed_day(y1,md(2))

       if(d1.gt.md(m1))then
        d1=d1-md(m1)
        m1=m1+1
       elseif(d1.le.0)then
        d1=d1+md(m1-1)
        m1=m1-1
       endif

      ! MONTH calculate
       if(m1.gt.12)then
        m1=m1-12
        y1=y1+1
       elseif(m1.le.0)then
        m1=m1+12
        y1=y1-1
       endif

      enddo

!      print*,'Output time: '
!      write(*,'(i4,5(a,i2.2))')y1,"/",m1,"/",d1," ",h1,":",mi1,":",se1

      if(TimeFormat.eq.1)then
         write(A_Timecal,'(i4,a,i2.2,a,i2.2,a,2i2.2)') &
                              y1,'-',m1,'-',d1,'_',h1,mi1

      elseif(TimeFormat.eq.3)then
         write(A_Timecal,'(i4,a,i2.2,a,i2.2,a,2i2.2)') &
                              y1,'-',m1,'-',d1,'_',h1

      elseif(TimeFormat.eq.4)then
         write(A_Timecal,'(i4,a,i2.2,a,i2.2,a,2i2.2)') &
                              y1,'-',m1,'-',d1

      elseif(TimeFormat.eq.2)then

       len0 = len_trim(time1)
       if(len0.eq.4)then
         write(A_Timecal,'(i4,5i2.2)') y1
       elseif(len0.eq.6)then
         write(A_Timecal,'(i4,5i2.2)') y1,m1
       elseif(len0.eq.8)then
         write(A_Timecal,'(i4,5i2.2)') y1,m1,d1
       elseif(len0.eq.10)then
         write(A_Timecal,'(i4,5i2.2)') y1,m1,d1,h1
       elseif(len0.eq.12)then
         write(A_Timecal,'(i4,5i2.2)') y1,m1,d1,h1,mi1
       elseif(len0.eq.14)then
         write(A_Timecal,'(i4,5i2.2)') y1,m1,d1,h1,mi1,se1
       endif

      endif !TimeFormat

      Return
      End Function A_Timecal


!############################################################
      Function A_Jday(Year,MM,DD)
      integer :: A_Jday
      integer :: Year,JDay,MM,DD
      integer :: i,md(12),sumday
      data md /31,28,31,30,31,30,31,31,30,31,30,31/

      A_Jday = 0
      call A_Fed_day(Year,md(2))

!  Compute Month and day:
      Jday = DD 
      do i = 1 , MM-1
        Jday = Jday + md(i)
      enddo
      A_Jday = Jday
      End Function A_Jday


!============================================================
      subroutine A_Fed_day(year,days)
      integer year,days

        days=28   !normal
        if(mod(year,4).eq.0)days=29
        if(mod(year,100).eq.0)days=28
        if(mod(year,400).eq.0)days=29
        if(mod(year,4000).eq.0)days=28

        return
        end subroutine
!============================================================


      Subroutine A_Dismantle(Input_String,Out_Elements,n_elements,Error)
       implicit none
       integer :: ele, idx1, Error,n_elements
       character(80)  :: Out_Elements(n_elements)
       character(*)   :: Input_String
       character(500) :: String_Line
       character(80)  :: String,Elements(50)

       Error = 0
       Elements = ''
       String_Line = trim(Input_String)//' End~'
       idx1 = index(trim(String_Line),' ')
       ele = 0 

       do while(idx1.gt.0)
         ele = ele + 1
         read(String_Line(1:idx1),'(a)')String
         Elements(ele) = String
         String_Line = adjustl(String_Line(idx1:))
         idx1 = index(trim(String_Line),' ')
       enddo

       if(ele.eq.0)Error = 1
       if(ele.eq.0)Return

       Out_Elements = Elements

      Return
      End Subroutine A_Dismantle


      Subroutine A_Dismantle2(Input_String,Out_Element,idx,Error)
       implicit none
       integer :: i, idx1, Error, idx
       character(*)   :: Out_Element
       character(*)   :: Input_String
       character(500) :: String_Line
!       character(80)  :: String,Elements(50)
       Error = 0
       String_Line = adjustl(trim(Input_String))//' End~'
       do i = 1 , idx
         idx1 = index(String_Line,' ')
         Out_Element = String_Line(1:idx1-1)
         String_Line = adjustl(String_Line(idx1:))
       enddo
      Return
      End Subroutine A_Dismantle2



      Subroutine A_Reverse3(Array,demo)
        implicit none
        integer            :: d1, d0, demo, dsize
        integer            :: xs, ys, zs
        real               :: Array(:,:,:)
        real,allocatable   :: Array0(:,:,:)
        dsize = size(Array,demo)
        xs = size(Array,1)
        ys = size(Array,2)
        zs = size(Array,3)
        allocate(Array0(xs,ys,zs))
        do d1 = 1 , dsize
          d0 = dsize - d1 + 1
          Array0(:,:,d0) = Array(:,:,d1)
        enddo
        Array = Array0
      Return
      End Subroutine A_Reverse3




        Integer Function A_STR_2_Integer(in_Str,Array)
        implicit none
        integer,parameter    :: Max_Length = 200
        integer, allocatable :: Array(:)
        character(*)         :: in_Str
        character(Max_Length):: Str
        integer              :: idx, len_str
        integer              :: num, in_num
        character(200)       :: format_Str
        character(10)        :: Str_num

        A_STR_2_Integer = 0
        if(len_trim(in_Str).gt.Max_Length)then
          write(*,*)'Too long input string....Max length: Max_Length'
          A_STR_2_Integer = -1
          return
        endif

        Str        = in_Str//' '
        len_str    = len_trim(Str)
        if(len_str.eq.0)then
          A_STR_2_Integer = 10
          return
        endif
        num        = 0
        in_num     = 0
        format_Str = '('
        do idx = 2 , len_str+1
          num = num + 1
          if(Str(idx-1:idx-1).eq.' ' .and. Str(idx:idx).ne.' ')then
            if(format_Str.ne.'(')format_Str = trim(format_Str)//','
            write(Str_num,'(i10)')num
            format_Str = trim(format_Str)//trim(adjustl(Str_num))//'x'
            num    = 0
          elseif(Str(idx-1:idx-1).ne.' ' .and. Str(idx:idx).eq.' ')then
            if(format_Str.ne.'(')format_Str = trim(format_Str)//','
            write(Str_num,'(i10)')num
            format_Str = trim(format_Str)//'i'//trim(adjustl(Str_num))
            num    = 0
            in_num = in_num + 1
          endif
        enddo
        format_Str = trim(format_Str)//')'

        allocate(Array(in_num))
        read(Str,format_Str,iostat=A_STR_2_Integer)Array

       Return
       End Function A_STR_2_Integer


       integer Function A_TimeBetween(T,T0,T1)
        implicit none
        character(14)  :: T,T0,T1     !YYYY-MM-DD_hhZ
        character(50)  :: formatT
        character(10)  :: T_date,T_date0,T_date1
        character(2)   :: T_hour,T_hour0,T_hour1
        character(50)  :: Temp_Time_File
        character(500) :: cmdstr
        integer        :: s,s0,s1


        A_TimeBetween = 0
        T_date  =  T(1:10)  ; T_hour  =  T(12:13)
        T_date0 = T0(1:10)  ; T_hour0 = T0(12:13)
        T_date1 = T1(1:10)  ; T_hour1 = T1(12:13)


        Temp_Time_File = 'Temp_Time_File'
        cmdstr = '>'//trim(Temp_Time_File)
        call system(cmdstr)
        cmdstr = 'date -d "'//T_date//' '//T_hour//'" +%s >> ' &
                //trim(Temp_Time_File)
        call system(cmdstr)
        cmdstr = 'date -d "'//T_date0//' '//T_hour0//'" +%s >> ' &
               //trim(Temp_Time_File)
        call system(cmdstr)
        cmdstr = 'date -d "'//T_date1//' '//T_hour1//'" +%s >> ' &
               //trim(Temp_Time_File)
        call system(cmdstr)

        open(21,file=Temp_Time_File,status='old')
        read(21,*)s,s0,s1

        if(s.ge.s0 .and. s.le.s1) A_TimeBetween = 1
        if(s.ge.s1 .and. s.le.s0) A_TimeBetween = 1

        cmdstr = 'rm '//trim(Temp_Time_File)
        call system(cmdstr)

       End Function A_TimeBetween

       logical Function A_FileExist(CheckFile)
       character(*) :: CheckFile
       inquire(file=CheckFile,exist=A_FileExist)
       if(.not.A_FileExist)write(*,*)'Not Exist File: ',CheckFile
       End Function A_FileExist


      END MODULE


