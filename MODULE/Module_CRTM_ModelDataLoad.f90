!#################################################################
!
! Name : Module_CRTM_ModelDataLoad
! Purpose : Loading NWP Data in Array for Reading.
!
! First Version                     Akey Chen in UMD  ????/??/??
! Modify by                         Akey Chen in CWB  2017/09/04
!#################################################################
MODULE Module_CRTM_ModelDataLoad

 public
 character(90) :: MODEL_Title
 character(10) :: MODEL_ini_Time 
 character(10) :: MODEL_fct_Time
 character(3)  :: MODEL_fct_Hour
 logical       :: Debug_ModelLoad

 integer, parameter :: num_ATM_DataKinds = 9
 real,allocatable :: MODEL_ATM_Pressure(:,:,:)       ! 1 Atmosphere Pressure 
 real,allocatable :: MODEL_ATM_Temperature(:,:,:)    ! 2 Atmosphere Temperature
 real,allocatable :: MODEL_ATM_Humidity(:,:,:)       ! 3 Atmosphere Water Vaper
 real,allocatable :: MODEL_ATM_O3(:,:,:)             ! 4 Atmosphere Water Vaper
 real,allocatable :: MODEL_ATM_CloudWater(:,:,:)     ! 5 Atmosphere Cloud Water
 real,allocatable :: MODEL_ATM_CloudIce(:,:,:)       ! 6 Atmosphere Cloud Ice
 real,allocatable :: MODEL_ATM_RainWater(:,:,:)      ! 7 Atmosphere Cloud Rain
 real,allocatable :: MODEL_ATM_Snow(:,:,:)           ! 8 Atmosphere Cloud Snow
 real,allocatable :: MODEL_ATM_Graupel(:,:,:)        ! 9 Atmosphere Cloud Graupel

 integer, parameter :: num_SFC_DataKinds = 7
 real,allocatable :: MODEL_SFC_Temperature(:,:)      ! 1 Surface Temperature
 real,allocatable :: MODEL_SFC_Type(:,:)             ! 2 Surface Type (land or water)
 real,allocatable :: MODEL_SFC_LandUse(:,:)          ! 3 Surface Land Use
 real,allocatable :: MODEL_SFC_WSpeed(:,:)           ! 4 Surface Wind Speed
 real,allocatable :: MODEL_SFC_WDirection(:,:)       ! 5 Surface Wind Direction
 real,allocatable :: MODEL_SFC_Lon(:,:)              ! 6 Surface Lon
 real,allocatable :: MODEL_SFC_Lat(:,:)              ! 7 Surface Lat

 integer, parameter :: num_Angle_DataKinds = 4 
 real,allocatable :: MODEL_Sensor_Angle_S(:,:)       ! 1 Scan Angle of Sensor
 real,allocatable :: MODEL_Sensor_Angle_Z(:,:)       ! 2 Zenith Angle of Sensor
 real,allocatable :: MODEL_Source_Angle_Z(:,:)       ! 3 Zenith Angle of Source
 real,allocatable :: MODEL_Source_Angle_A(:,:)       ! 4 Azimuth Angle of Source

 
 character(25) :: UNIT_ATM_Pressure
 character(25) :: UNIT_ATM_Temperature
 character(25) :: UNIT_ATM_Humidity
 character(25) :: UNIT_ATM_O3
 character(25) :: UNIT_ATM_CloudWater
 character(25) :: UNIT_ATM_CloudIce
 character(25) :: UNIT_ATM_RainWater
 character(25) :: UNIT_ATM_Snow
 character(25) :: UNIT_ATM_Graupel
 character(25) :: UNIT_SFC_Temperature
 character(25) :: UNIT_SFC_Type
 character(25) :: UNIT_SFC_LandUse
 character(25) :: UNIT_SFC_WSpeed
 character(25) :: UNIT_SFC_WDirection
 character(25) :: UNIT_SFC_Lon
 character(25) :: UNIT_SFC_Lat

 integer :: MODEL_X_Size
 integer :: MODEL_Y_Size
 integer :: MODEL_Z_Size
  data MODEL_X_Size / 0 /
  data MODEL_Y_Size / 0 /
  data MODEL_Z_Size / 0 /


 public :: MC_MODEL_AllocateArray
 public :: MC_MODEL_LoadFile
 public :: MC_MODEL_SolarAngle


CONTAINS

 Subroutine MC_MODEL_LoadFile(inFilename,error)
  implicit none
  integer,parameter :: strlen=250
  character(*)     :: inFilename
  integer          :: error
  logical          :: ex1, ex2
  character(strlen):: Filename
  character(strlen):: Listname
  character(100)   :: strinfo
  real,allocatable :: DataValue(:,:)
  character(25)    :: DataSymls,DataLevelInfo
  integer          :: ir
  integer          :: Z_index,DataExist,TotalField
  integer          :: x,y 


  if(len_trim(inFilename).gt.250)then
    write(*,*)'Too long input Filename....'//inFilename
    write(*,*)'Max len str is ',strlen
    error = 2
    return
  endif

  Filename = trim(inFilename)
  Listname = trim(Filename)//'.list'
  inquire(file=Filename,exist=ex1)
  inquire(file=Listname,exist=ex2)
  if(.not.ex1)write(*,*)'Not Exist File: ',trim(Filename)
  if(.not.ex2)write(*,*)'Not Exist File: ',trim(Listname)
  if(.not.ex1 .or. .not.ex2)then
    error = 1
    return
  endif


  open(21,file=Listname,status='old')
  read(21,'(a)')MODEL_Title
  read(21,'(14x,a10)')MODEL_ini_Time 
  read(21,'(15x,a10)')MODEL_fct_Time 
  read(21,'(14x,a3)')MODEL_fct_Hour 
  read(21,'(6x,i5)')MODEL_X_Size 
  read(21,'(6x,i5)')MODEL_Y_Size 
  read(21,'(6x,i5)')MODEL_Z_Size
  read(21,'(12x,i5)')TotalField


  if(Debug_ModelLoad)then
    write(*,*)'Filename: ',trim(Filename)
    write(*,*)'Listname: ',trim(Listname)
  endif

  call MC_MODEL_AllocateArray()
  allocate(DataValue(MODEL_X_Size,MODEL_Y_Size))
  open(22,file=Filename,form='unformatted',access='direct',recl=4*MODEL_X_Size*MODEL_Y_Size)
  do ir = 1 , TotalField
    read(21,*)DataSymls,DataLevelInfo,Z_index,DataExist
    if(Debug_ModelLoad)write(*,*)DataSymls,DataLevelInfo,Z_index,DataExist
    read(22,rec=ir)DataValue

    select case(DataSymls)
      case('Pressure_mb')
        MODEL_ATM_Pressure(:,:,Z_index+1) = DataValue
        UNIT_ATM_Pressure = 'mb'
      case('Temperature_K')
        MODEL_ATM_Temperature(:,:,Z_index+1) = DataValue
        UNIT_ATM_Temperature = 'K'
      case('O3_MR_kg_kg-1')
!        MODEL_ATM_O3(:,:,Z_index+1) = DataValue*1000
        MODEL_ATM_O3(:,:,Z_index+1) = DataValue
        UNIT_ATM_O3 = 'MASS_MIXING_RATIO'    ! g/kg
      case('Vapor_SA')
        MODEL_ATM_Humidity(:,:,Z_index+1) = DataValue
        UNIT_ATM_Humidity = 'SPECIFIC_AMOUNT'
      case('Vapor_RH')
        MODEL_ATM_Humidity(:,:,Z_index+1) = DataValue
        UNIT_ATM_Humidity = 'RELATIVE_HUMIDITY'
      case('Vapor_MR_kg_kg-1')
        MODEL_ATM_Humidity(:,:,Z_index+1) = DataValue*1000
        UNIT_ATM_Humidity = 'MASS_MIXING_RATIO'    ! g/kg
      case('CloudWater_MR_kg_kg-1')
        MODEL_ATM_CloudWater(:,:,Z_index+1) = DataValue*1000
        UNIT_ATM_CloudWater = 'MASS_MIXING_RATIO'    ! g/kg
      case('CloudIce_MR_kg_kg-1')
        MODEL_ATM_CloudIce(:,:,Z_index+1) = DataValue*1000
        UNIT_ATM_CloudIce = 'MASS_MIXING_RATIO'    ! g/kg
      case('CloudRain_MR_kg_kg-1')
        MODEL_ATM_RainWater(:,:,Z_index+1) = DataValue*1000
        UNIT_ATM_RainWater = 'MASS_MIXING_RATIO'    ! g/kg
      case('CloudSnow_MR_kg_kg-1')
        MODEL_ATM_Snow(:,:,Z_index+1) = DataValue*1000
        UNIT_ATM_Snow = 'MASS_MIXING_RATIO'    ! g/kg
      case('CloudGroupe_MR_kg_kg-1')
        MODEL_ATM_Graupel(:,:,Z_index+1) = DataValue*1000
        UNIT_ATM_Graupel = 'MASS_MIXING_RATIO'      ! g/kg
      case('SFCTemperature_K')
        MODEL_SFC_Temperature(:,:) = DataValue
        UNIT_SFC_Temperature = 'K'
      case('LandMask_NAN')
        MODEL_SFC_Type(:,:) = DataValue
        UNIT_SFC_Type = 'LandMask_NAN'
      case('WindSpeed_MS')
        MODEL_SFC_WSpeed(:,:) = DataValue
        UNIT_SFC_WSpeed = 'm/s'
      case('WindDirection_degree')
        MODEL_SFC_WDirection(:,:) = DataValue
        UNIT_SFC_WDirection = 'Degree'
      case('Latitude_degree')
        MODEL_SFC_Lat(:,:) = DataValue
        UNIT_SFC_Lat = 'Degree'
      case('Longtitude_degree')
        MODEL_SFC_Lon(:,:) = DataValue
        UNIT_SFC_Lon = 'Degree'
    end select

  enddo !ir

 
!  do x = 44, 1, -1
!    write(*,'(3f10.5)')MODEL_ATM_Pressure(111,64,x), & 
!                       MODEL_ATM_RainWater(111,64,x),MODEL_ATM_Snow(111,64,x)
!  enddo
!  print*,'akey test'
!  stop



  if(UNIT_SFC_Type.eq.'LandMask_NAN')then
   do x = 1 , MODEL_X_Size
   do y = 1 , MODEL_Y_Size
    if(MODEL_SFC_Type(x,y).eq.1)then 
      MODEL_SFC_Type(x,y) = 20   !Land
    elseif(MODEL_SFC_Type(x,y).eq.0)then
      MODEL_SFC_Type(x,y) = 10   !Sea
    endif
   enddo
   enddo
   UNIT_SFC_Type = 'LandMask_CRTM'
  endif



 Return
 End Subroutine MC_MODEL_LoadFile


 Subroutine MC_MODEL_AllocateArray()
  implicit none

  ! Release the Data Array:
  if(allocated(MODEL_ATM_Pressure))   deallocate(MODEL_ATM_Pressure)
  if(allocated(MODEL_ATM_Temperature))deallocate(MODEL_ATM_Temperature)
  if(allocated(MODEL_ATM_Humidity))   deallocate(MODEL_ATM_Humidity)
  if(allocated(MODEL_ATM_O3))         deallocate(MODEL_ATM_O3)
  if(allocated(MODEL_ATM_CloudWater)) deallocate(MODEL_ATM_CloudWater)
  if(allocated(MODEL_ATM_CloudIce))   deallocate(MODEL_ATM_CloudIce)
  if(allocated(MODEL_ATM_RainWater))  deallocate(MODEL_ATM_RainWater)
  if(allocated(MODEL_ATM_Snow))       deallocate(MODEL_ATM_Snow)
  if(allocated(MODEL_ATM_Graupel))    deallocate(MODEL_ATM_Graupel)

  if(allocated(MODEL_SFC_Type))       deallocate(MODEL_SFC_Type)
  if(allocated(MODEL_SFC_Temperature))deallocate(MODEL_SFC_Temperature)
  if(allocated(MODEL_SFC_WSpeed))     deallocate(MODEL_SFC_WSpeed)
  if(allocated(MODEL_SFC_WDirection)) deallocate(MODEL_SFC_WDirection)
  if(allocated(MODEL_SFC_Lon))        deallocate(MODEL_SFC_Lon)
  if(allocated(MODEL_SFC_Lat))        deallocate(MODEL_SFC_Lat)

  ! Set the Data Array:
  allocate(MODEL_ATM_Pressure(MODEL_X_Size,MODEL_Y_Size,MODEL_Z_Size))
  allocate(MODEL_ATM_Temperature(MODEL_X_Size,MODEL_Y_Size,MODEL_Z_Size))
  allocate(MODEL_ATM_Humidity(MODEL_X_Size,MODEL_Y_Size,MODEL_Z_Size))
  allocate(MODEL_ATM_O3(MODEL_X_Size,MODEL_Y_Size,MODEL_Z_Size))
  allocate(MODEL_ATM_CloudWater(MODEL_X_Size,MODEL_Y_Size,MODEL_Z_Size))
  allocate(MODEL_ATM_CloudIce(MODEL_X_Size,MODEL_Y_Size,MODEL_Z_Size))
  allocate(MODEL_ATM_RainWater(MODEL_X_Size,MODEL_Y_Size,MODEL_Z_Size))
  allocate(MODEL_ATM_Snow(MODEL_X_Size,MODEL_Y_Size,MODEL_Z_Size))
  allocate(MODEL_ATM_Graupel(MODEL_X_Size,MODEL_Y_Size,MODEL_Z_Size))
  allocate(MODEL_SFC_Type(MODEL_X_Size,MODEL_Y_Size))
  allocate(MODEL_SFC_Temperature(MODEL_X_Size,MODEL_Y_Size))
  allocate(MODEL_SFC_WSpeed(MODEL_X_Size,MODEL_Y_Size))
  allocate(MODEL_SFC_WDirection(MODEL_X_Size,MODEL_Y_Size))
  allocate(MODEL_SFC_Lon(MODEL_X_Size,MODEL_Y_Size))
  allocate(MODEL_SFC_Lat(MODEL_X_Size,MODEL_Y_Size))

  ! Set Angle Array:
  allocate(MODEL_Sensor_Angle_S(MODEL_X_Size,MODEL_Y_Size))
  allocate(MODEL_Sensor_Angle_Z(MODEL_X_Size,MODEL_Y_Size))
  allocate(MODEL_Source_Angle_Z(MODEL_X_Size,MODEL_Y_Size))
  allocate(MODEL_Source_Angle_A(MODEL_X_Size,MODEL_Y_Size))


  MODEL_ATM_Pressure    = -1.
  MODEL_ATM_Temperature = -1.
  MODEL_ATM_Humidity    = -1.
  MODEL_ATM_O3          = -1.
  MODEL_ATM_CloudWater  = -1.
  MODEL_ATM_CloudIce    = -1.
  MODEL_ATM_RainWater   = -1.
  MODEL_ATM_Snow        = -1.
  MODEL_ATM_Graupel     = -1.
  MODEL_SFC_Type        = -1.
  MODEL_SFC_Temperature = -1.
  MODEL_SFC_WSpeed      = -1.
  MODEL_SFC_WDirection  = -1.
  MODEL_SFC_Lon         = -1.
  MODEL_SFC_Lat         = -1.

  UNIT_ATM_Humidity     = 'MASS_MIXING_RATIO'
  UNIT_ATM_O3           = 'MASS_MIXING_RATIO'

  ! ### Default Angle Value:
  MODEL_Sensor_Angle_S = 0
  MODEL_Sensor_Angle_Z = 0
  MODEL_Source_Angle_Z = 0
  MODEL_Source_Angle_A = 0



 Return
 End Subroutine MC_MODEL_AllocateArray


 Subroutine MC_MODEL_SolarAngle()
 USE Module_AngleCalculate, only: SolarAngle
  implicit none
  integer :: x, y
  real    :: lon, lat
  real    :: year,month,day,hour
  real    :: SA(5)


  ! ### Model Time Information:
  read(MODEL_fct_Time(1:4), *)year
  read(MODEL_fct_Time(5:6), *)month
  read(MODEL_fct_Time(7:8), *)day
  read(MODEL_fct_Time(9:10),*)hour

  ! ### Allocate Angle Array:
  if(allocated(MODEL_Source_Angle_A))deallocate(MODEL_Source_Angle_A)
  if(allocated(MODEL_Source_Angle_Z))deallocate(MODEL_Source_Angle_Z)
  allocate(MODEL_Source_Angle_A(MODEL_X_Size,MODEL_Y_Size))
  allocate(MODEL_Source_Angle_Z(MODEL_X_Size,MODEL_Y_Size))

  ! ### Solar Source Angle Calculate:
  do x = 1 , MODEL_X_Size
  do y = 1 , MODEL_Y_Size
    lon = MODEL_SFC_Lon(x,y)
    lat = MODEL_SFC_Lat(x,y)
    SA = SolarAngle(year, month, day, hour, lat, lon)
    MODEL_Source_Angle_A(x,y) = SA(1)
    MODEL_Source_Angle_Z(x,y) = 90 -  SA(2)
    if(MODEL_Source_Angle_Z(x,y).gt.90)MODEL_Source_Angle_Z(x,y) = 90.
  enddo
  enddo


 Return
 End SubRoutine MC_MODEL_SolarAngle


 Subroutine MC_MODEL_ShowValue(x,y)
  implicit none
  integer       :: x, y, l
  character(20) :: PrintFormat1, PrintFormat2

  PrintFormat1 = '(10(f10.5,1x))'
  PrintFormat2 = '(10(a10,1x))'
  write(*,*)'Locate x and y: ',x,y
  write(*,PrintFormat2)'ATM_P','ATM_T','ATM_Vapor','ATM_O3',&
                       'ATM_CW','ATM_CI','ATM_CR','ATM_CS','ATM_CG'
  do l = 1 , MODEL_Z_Size
    write(*,PrintFormat1)MODEL_ATM_Pressure(x,y,l), &
                         MODEL_ATM_Temperature(x,y,l), &
                         MODEL_ATM_Humidity(x,y,l), &
                         MODEL_ATM_O3(x,y,l), &
                         MODEL_ATM_CloudWater(x,y,l), &
                         MODEL_ATM_CloudIce(x,y,l),   &
                         MODEL_ATM_RainWater(x,y,l), &
                         MODEL_ATM_Snow(x,y,l), &
                         MODEL_ATM_Graupel(x,y,l)
  enddo

  write(*,*)'MODEL_SFC_Type: ',MODEL_SFC_Type(x,y)
  write(*,*)'MODEL_SFC_Temperature: ',MODEL_SFC_Temperature(x,y)
            
 Return
 End Subroutine MC_MODEL_ShowValue


END MODULE Module_CRTM_ModelDataLoad

