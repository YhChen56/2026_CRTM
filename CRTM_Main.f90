!#######################################################################
!
! Name: CRTM_Main
! Propose: CRTM Main Program.
!
!                                        Akey Chen in CWB  2017/09/15
!#######################################################################
Program CRTM_Main

 USE Module_a_function 
 USE Module_CRTM_ModelDataLoad
 USE Module_CRTM_Parameters
 USE Module_CRTM_ValueTransport
 USE Module_CRTM_Check
 USE Module_CRTM_DataArray
 USE Module_CRTM_Compute
 USE Module_GeoCount

 implicit none
 integer        :: error

! ## Filename Section:
 character(100) :: CRTM_PGname 
 character(100) :: CRTM_InputDir 
 character(100) :: CRTM_InputSub 
 character(100) :: CRTM_InputName 
 character(250) :: CRTM_InputFile 
 character(250) :: CRTM_OutputName 
 character(250) :: CRTM_OutputFile 
 character(250) :: CRTM_OutputDir 
 character(250) :: CRTM_SettingFile

! ## Output Array:
 real,allocatable :: CRTM_OutArray(:,:,:,:)  ! (n_Output,n_Channels,MODEL_xs,MODEL_ys)
! real,allocatable :: CRTM_Out(:,:)  ! (n_Output, n_Channels)
! real,allocatable :: BT(:,:,:)      ! (MODEL_xs, MODEL_ys, n_Channels)
! real,allocatable :: Rad(:,:,:)     ! (MODEL_xs, MODEL_ys, n_Channels)
! real,allocatable :: Up_R(:,:,:)    ! (MODEL_xs, MODEL_ys, n_Channels)
! real,allocatable :: SFC_R(:,:,:)   ! (MODEL_xs, MODEL_ys, n_Channels)
! byte,allocatable :: Counts(:,:,:)  ! (MODEL_xs, MODEL_ys, n_Channels)

! ## Channel Infomation: 
! integer,allocatable :: Channel_idx(:)
! character(3)        :: Channel_Name
! real,allocatable :: V(:,:) ! Values
! byte,allocatable :: C(:,:) ! Counts
! real :: WaveLength, AlbedoCeoeff 
! real :: r


 !## Common Parameters:
 integer :: x,y,l,chi
 logical :: Debug_Main, Quiet
 character(250) :: Str_Info
 character(1)   :: quiet_str
 character(500) :: cmdstr

 Debug_Main = .false.
! Debug_Main = .true.


! ##### Model Source Data:
! CRTM_InputDir    = '/data4/crtm/NCEP_GFS025/CRTM_input_Data/'
! CRTM_InputSub    = '2017082800/'
! CRTM_InputName   = 'CRTMinput.ncepp25.i2017082800.f2017082900'
! CRTM_InputFile   = trim(CRTM_InputDir)//trim(CRTM_InputSub)//trim(CRTM_InputName)
! CRTM_OutputDir   = './'
! CRTM_OutputName  = 'CRTMout.ncep.dat'
! CRTM_OutputFile  = trim(CRTM_OutputDir)//trim(CRTM_OutputName)
! CRTM_SettingFile = './CRTM_SETTING_NCEP025_IR'


! ##### Model Source Data:
! CRTM_InputDir    = '/data4/crtm/WRF/CRTM_input_Data/'
! CRTM_InputSub    = '2017090400/'
! CRTM_InputName   = 'CRTMinput.wrfd01.i2017090400.f2017090401'
! CRTM_InputFile   = trim(CRTM_InputDir)//trim(CRTM_InputSub)//trim(CRTM_InputName)
! CRTM_OutputDir   = './'
! CRTM_OutputName  = 'CRTMout.wrf.dat'
! CRTM_OutputFile  = trim(CRTM_OutputDir)//trim(CRTM_OutputName)
! CRTM_SettingFile = './CRTM_SETTING_NCEP025_IR'

 
 call getarg(0,CRTM_PGname) 
 call getarg(1,CRTM_SettingFile) 
 call getarg(2,CRTM_InputFile) 
 call getarg(3,CRTM_OutputFile)
 call getarg(4,quiet_str)





! ##### Input Information Ckecking:
 if(CRTM_SettingFile.eq.'')then
   Str_Info = trim(CRTM_PGname)//' CRTM_SettingFile CRTM_InputFile CRTM_OutputFile'
   write(*,*)('#',x=1,len_trim(Str_Info))
   write(*,*)'Using Top:'
   write(*,*)trim(Str_Info)
   write(*,*)('#',x=1,len_trim(Str_Info))
   stop
 endif



! ##### Set NWP Data Information:
 call MC_Parameter_Setting(CRTM_SettingFile,error)
 if(CRTMP_Debug_Main.eq.1)     Debug_Main      = .true.
 if(CRTMP_Debug_CRTMP.eq.1)    Debug_CRTMP     = .true.
 if(CRTMP_Debug_ModelLoad.eq.1)Debug_ModelLoad = .true.
 if(CRTMP_Debug_CRTM.eq.1)     Debug_CRTM      = .true.
 if(quiet_str.ne.'')Quiet = .true.
 if(quiet_str.eq.'')Quiet = .false.
 if(Quiet)Debug_Main = .false.
 if(Debug_Main)write(*,*)'NWP Data Information Already:'




 if(error.ne.0)stop
 if(CRTM_OutputFile.eq.'')then
   Str_Info = trim(CRTM_PGname)//' CRTM_SettingFile CRTM_InputFile CRTM_OutputFile'
   write(*,*)('#',x=1,len_trim(Str_Info))
   write(*,*)'CRTM_SettingFile: ',trim(CRTM_SettingFile)
   write(*,*)'CRTM_Coeff_Dir  : ',trim(CRTMP_Coeff_Dir)
   write(*,*)''
   write(*,*)'Using Top:'
   write(*,*)trim(Str_Info)
   write(*,*)('#',x=1,len_trim(Str_Info))
   stop
 endif


! ##### Find CRTM Output Path:
  CRTM_OutputDir = A_DirName(CRTM_OutputFile)


! ##### Model Data Load Section:
 if(Debug_Main)write(*,*)'Model Data Load Section:'
 call MC_MODEL_LoadFile(CRTM_InputFile,error)
 if(error.ne.0)stop
 call MC_MODEL_SolarAngle()



! ##### Model Data Adjection Section:
 if(Debug_Main)write(*,*)'Model Data Adjection Section:'
 do x = 1 , MODEL_X_Size
 do y = 1 , MODEL_Y_Size
   call MR2LWP_by_LayerP(MODEL_ATM_Pressure(x,y,:),MODEL_ATM_CloudWater(x,y,:))
   call MR2LWP_by_LayerP(MODEL_ATM_Pressure(x,y,:),MODEL_ATM_CloudIce(x,y,:))
   call MR2LWP_by_LayerP(MODEL_ATM_Pressure(x,y,:),MODEL_ATM_RainWater(x,y,:))
   call MR2LWP_by_LayerP(MODEL_ATM_Pressure(x,y,:),MODEL_ATM_Snow(x,y,:))
   call CRTM_Check_Cloud(MODEL_ATM_CloudWater(x,y,:),2)
   call CRTM_Check_Cloud(MODEL_ATM_CloudIce(x,y,:),2)
   call CRTM_Check_Cloud(MODEL_ATM_RainWater(x,y,:),2)
   call CRTM_Check_Cloud(MODEL_ATM_Snow(x,y,:),2)
 enddo
 enddo
 UNIT_ATM_CloudWater = 'LIQUID_WATER_PATH'  !kg/m^2
 UNIT_ATM_CloudIce   = 'LIQUID_WATER_PATH'
 UNIT_ATM_RainWater  = 'LIQUID_WATER_PATH'
 UNIT_ATM_Snow       = 'LIQUID_WATER_PATH'



! ##### Put Data Array in CRTM:
 MODEL_ATM_Humidity = MODEL_ATM_Humidity * Ratio_Vapor

 if(Debug_Main)write(*,*)'Put Data Array in CRTM:'
 if(UNIT_ATM_Humidity.eq.'RELATIVE_HUMIDITY')then
   MODEL_ATM_Humidity = MODEL_ATM_Humidity / 100.
 endif
 

 Error = CRTM_DataArray_Dimension(MODEL_X_Size,MODEL_Y_Size,MODEL_Z_Size)
 Error = CRTM_DataArray('ATM_Pressure',    MODEL_ATM_Pressure)
 Error = CRTM_DataArray('ATM_Temperature', MODEL_ATM_Temperature)
 Error = CRTM_DataArray('ATM_Vapor',       MODEL_ATM_Humidity)
 Error = CRTM_DataArray('ATM_O3',          MODEL_ATM_O3)
 Error = CRTM_DataArray('Cloud_W',         MODEL_ATM_CloudWater)
 Error = CRTM_DataArray('Cloud_I',         MODEL_ATM_CloudIce)
 Error = CRTM_DataArray('Cloud_R',         MODEL_ATM_RainWater)
 Error = CRTM_DataArray('Cloud_S',         MODEL_ATM_Snow)
 Error = CRTM_DataArray('SFC_Temperature', MODEL_SFC_Temperature)
 Error = CRTM_DataArray('SFC_Mask',        MODEL_SFC_Type)
 Error = CRTM_DataArray('Sensor_Angle_S',  CRTMP_Sensor_Angle_S)
 Error = CRTM_DataArray('Sensor_Angle_Z',  CRTMP_Sensor_Angle_Z)
 if(CRTMP_AssignAngle.eq.1)then
   Error = CRTM_DataArray('Source_Angle_Z',  CRTMP_Source_Angle_Z)
   Error = CRTM_DataArray('Source_Angle_A',  CRTMP_Source_Angle_A)
 else
   Error = CRTM_DataArray('Source_Angle_Z',  MODEL_Source_Angle_Z)
   Error = CRTM_DataArray('Source_Angle_A',  MODEL_Source_Angle_A)
 endif


 Error = CRTM_DataUnit('ATM_Vapor',UNIT_ATM_Humidity) 
 Error = CRTM_DataUnit('ATM_O3',   UNIT_ATM_O3) 

 

! ##### Get CRTM output Value for echo pixel:
 if(Debug_Main)write(*,*)'CRTM Initial:'
 call CRTM_initial(1,MODEL_Z_Size)
! allocate(CRTM_Out(CRTM_n_Output,CRTM_n_Channels))
! allocate(CRTM_OutArray(CRTM_n_Output,CRTM_n_Channels,MODEL_X_Size,MODEL_Y_Size))
 allocate(CRTM_OutArray(MODEL_X_Size,MODEL_Y_Size,CRTM_n_Output,CRTM_n_Channels))
! allocate(   BT(MODEL_X_Size,MODEL_Y_Size,CRTM_n_Channels))
! allocate(  Rad(MODEL_X_Size,MODEL_Y_Size,CRTM_n_Channels))
! allocate( Up_R(MODEL_X_Size,MODEL_Y_Size,CRTM_n_Channels))
! allocate(SFC_R(MODEL_X_Size,MODEL_Y_Size,CRTM_n_Channels))




! ##### CRTM Calculate Section:
 if(Debug_Main)write(*,*)'CRTM Calculate Section:'
! x = 111
! y = 64



 do x = 1 , MODEL_X_Size
 if(Debug_Main)write(*,*)x,'/',MODEL_X_Size
 do y = 1 , MODEL_Y_Size

   
  CRTM_OutArray(x,y,:,:) = CRTM_Result(x,y)



!   CRTM_Out     = CRTM_Result(x,y)
!   BT(x,y,:)    = CRTM_Out(1,:)
!   Rad(x,y,:)   = CRTM_Out(2,:)
!   Up_R(x,y,:)  = CRTM_Out(3,:)
!   SFC_R(x,y,:) = CRTM_Out(4,:)
 enddo
 enddo


! ##### Albedo Section:  Convert Radiance into Albedo.
! if(CRTMP_Sensor_ID(1:2).eq.'v.')then
!   allocate(Channel_idx(CRTM_n_Channels))
!   Channel_idx = CRTM_Get_Channels()
!   do chi = 1 , CRTM_n_Channels
!     WaveLength   = MC_Parameter_WaveLength(Channel_idx(chi))
!     AlbedoCeoeff = MC_Parameter_AlbedoCeoeff(Channel_idx(chi))
!     do x = 1 , MODEL_X_Size
!     do y = 1 , MODEL_Y_Size
!       r = Rad(x,y,chi)*10*WaveLength**(-2)
!       BT(x,y,chi) = r*AlbedoCeoeff*100
!     enddo
!     enddo
!   enddo
!   deallocate(Channel_idx)
! endif

 
! ##### Convert Value: into Counts:
! Error = GC_Set_TableDir('/home/akey/CRTM_Process/CRTM_V1/MODULE/GeoCount_Table/')  ! fit
! allocate(V(MODEL_X_Size,MODEL_Y_Size))
! allocate(C(MODEL_X_Size,MODEL_Y_Size))
! allocate(Channel_idx(CRTM_n_Channels))
! allocate(Counts(MODEL_X_Size,MODEL_Y_Size,CRTM_n_Channels))
! Channel_idx = CRTM_Get_Channels()
! do chi = 1 , CRTM_n_Channels
!   V = BT(:,:,chi)
!   Channel_Name = MC_Parameter_ChName(Channel_idx(chi))
!   call GC_Value2Count(V,C,Channel_Name,Error)
!   if(Debug_Main)write(*,*)Channel_Name,Error
!   Counts(:,:,chi) = C
! enddo


! ##### CRTM Output File:
 if(Debug_Main)write(*,*)'CRTM Output File: ',trim(CRTM_OutputFile)
 if(CRTM_OutputDir.ne.'') then
   cmdstr = 'test -d '//trim(CRTM_OutputDir)//' || mkdir -p '//trim(CRTM_OutputDir)
   call system(cmdstr)
 endif
 open(31,file=CRTM_OutputFile,form='unformatted',access='stream')

  do l = 1 , CRTMF_n_outputs 
    select case(CRTMF_outputs(l))
     case('MODEL_SFC_Temperature'); write(31)MODEL_SFC_Temperature
     case('CRTM_OutArray')        ; write(31)CRTM_OutArray
     case('')                     ; continue
     case default                 ; write(*,*)'Unknow Output Item:',trim(CRTMF_outputs(l))
    end select
  enddo


! write(31)Counts
! write(31)BT
 close(31)


stop
end
