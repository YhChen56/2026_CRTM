!############################################################################
!
! Name : Module_CRTM_Compute
! Purpose : Module For CRTM Main Precess.
!
! First Version                             Akey Chen in CWB  2013/09/14
! Modify by                                 Akey Chen in UMD  2014/08/28
! Modify by                                 Akey Chen in CWB  2015/05/02
! Modify by                                 Akey Chen in CWB  2017/09/04
!############################################################################




      MODULE Module_CRTM_Compute
       USE CRTM_module
       USE CRTM_Planck_Functions, only:CRTM_Planck_Radiance
       implicit none
       private
       logical, public :: Debug_CRTM


      ! ### Input Data Array:
       real, allocatable, public :: inCRTM_ATM_PressL(:,:) !hPa   (n_Profile,n_Layer)
       real, allocatable, public :: inCRTM_ATM_Press(:,:)  !hPa   (n_Profile,n_Layer)
       real, allocatable, public :: inCRTM_ATM_Temp(:,:)   !K     (n_Profile,n_Layer)
       real, allocatable, public :: inCRTM_ATM_Vapor(:,:)  !g/kg  (n_Profile,n_Layer)
       real, allocatable, public :: inCRTM_ATM_O3(:,:)     !g/kg  (n_Profile,n_Layer)
       real, allocatable, public :: inCRTM_Cloud_W(:,:)    !kg/m2 (n_Profile,n_Layer)
       real, allocatable, public :: inCRTM_Cloud_I(:,:)    !kg/m2 (n_Profile,n_Layer)
       real, allocatable, public :: inCRTM_Cloud_R(:,:)    !kg/m2 (n_Profile,n_Layer)
       real, allocatable, public :: inCRTM_Cloud_S(:,:)    !kg/m2 (n_Profile,n_Layer)
       real, allocatable, public :: inCRTM_Cloud_G(:,:)    !kg/m2 (n_Profile,n_Layer)
       real, allocatable, public :: inCRTM_Aerosol_D(:,:)  !kg/m2 (n_Profile,n_Layer)

       real, allocatable, public :: inCRTM_SFC_Temp(:)     !K       (n_Profile)
       real, allocatable, public :: inCRTM_SFC_Mask(:)     !NA      (n_Profile)
       real, allocatable, public :: inCRTM_Wind_S(:)       !K       (n_Profile)
       real, allocatable, public :: inCRTM_Wind_D(:)       !K       (n_Profile)
       real(8), allocatable, public :: inCRTM_Sensor_Angle_S(:)   ! degree (n_Profile)
       real(8), allocatable, public :: inCRTM_Sensor_Angle_Z(:)   ! degree (n_Profile)
       real(8), allocatable, public :: inCRTM_Source_Angle_Z(:)   ! degree (n_Profile)
       real(8), allocatable, public :: inCRTM_Source_Angle_A(:)   ! degree (n_Profile)

       logical, allocatable, public :: CRTM_Layer_Use(:,:) !NA   (n_Profile,n_Layer)

       character(25), public :: inCRTM_UNIT_ATM_Vapor
       character(25), public :: inCRTM_UNIT_ATM_O3


      ! ### Logical Parameters:
       logical, public :: inCRTM_Cloud_W_set
       logical, public :: inCRTM_Cloud_I_set
       logical, public :: inCRTM_Cloud_R_set
       logical, public :: inCRTM_Cloud_S_set
       logical, public :: inCRTM_Cloud_G_set
        data inCRTM_Cloud_W_set /.false./
        data inCRTM_Cloud_I_set /.false./
        data inCRTM_Cloud_R_set /.false./
        data inCRTM_Cloud_S_set /.false./
        data inCRTM_Cloud_G_set /.false./


      ! ### CRTM Public Parameters:
       integer,public :: CRTM_n_Output         ! Number of Output Parameters
       integer,public :: CRTM_n_Channels       ! Number of Channels

      ! ### CRTM Calculate Parameters:
       integer,parameter :: CRTM_n_Sensors  = 1
       character(120) :: CRTM_Sensor_ID ! Sensor ID
       character(120) :: CRTM_Coeff_Dir ! Coefficient file Directory
       integer :: CRTM_n_Profiles       ! Number of Profile in CRTM at one time/
       integer :: CRTM_n_Layers         ! Number of Vertical Layers in CRTM
       integer :: CRTM_n_Absorbers      ! Number of Absorbers
       integer :: CRTM_n_Aerosols       ! Number of Aerosols
       integer :: CRTM_n_Clouds         ! Number of Clouds
       integer :: CRTM_Max_Layers       ! The Max number of Profile Layer
       real(8) :: CRTM_Angle_S          ! Geometry Scan Angles
       real(8) :: CRTM_Angle_Z          ! Geometry Zemith Angles
       real(8) :: CRTM_EffecR_Water     ! Effective Radius of Cloud Water
       real(8) :: CRTM_EffecR_Ice       ! Effective Radius of Cloud Ice
       real(8) :: CRTM_EffecR_Rain      ! Effective Radius of Cloud Rain
       real(8) :: CRTM_EffecR_Snow      ! Effective Radius of Cloud Snow
       real(8) :: CRTM_EffecR_Graupel   ! Effective Radius of Cloud Graupel
       real(8) :: CRTM_EffecR_DUST      ! Effective Radius of Aerosol


      ! ### CRTM Setting Coeff Parameter:
       character(30) :: IRlandCoeff_File
       character(30) :: IRwaterCoeff_File
       character(30) :: MWwaterCoeff_File
       character(30) :: VISlandCoeff_File


      ! ### CRTM State Parameter:
       character(50) :: STATE_CRTM_Sensor_ID
       character(30) :: STATE_IRlandCoeff_File
       character(30) :: STATE_IRwaterCoeff_File
       character(30) :: STATE_MWwaterCoeff_File
       character(30) :: STATE_VISlandCoeff_File

      ! ### CRTM other Optional Setting:
       integer, allocatable, public :: CRTM_Force_ch(:)

      ! ### MODULE Inside Parameters:
       character(10) :: CRTM_Absorber_Kind(10)
!       character(30) :: CRTM_Absorber_Unit(10)
       character(10) :: CRTM_Aerosol_Kind(10)
       character(10) :: CRTM_Cloud_Kind(10)
       logical :: log_CRTM_initical

      ! ### CRTM Data Array for Input and Output:
       type(CRTM_ChannelInfo_type),allocatable :: ChannelInfo(:)
       type(CRTM_Atmosphere_type) ,allocatable :: Atm(:)
       type(CRTM_Surface_type)    ,allocatable :: Sfc(:)
       type(CRTM_Geometry_type)   ,allocatable :: Geometry(:)
       type(CRTM_Options_type)    ,allocatable :: Options(:)
       type(CRTM_RTSolution_type) ,allocatable :: RTSolution(:,:)

      !!!! K_Matrix:
       type(CRTM_Atmosphere_type) ,allocatable :: Atm_K(:,:)
       type(CRTM_Surface_type)    ,allocatable :: Sfc_K(:,:)
       type(CRTM_RTSolution_type) ,allocatable :: RTSolution_K(:,:)

      ! ### CRTM Output type:
       character(15) :: CRTMOut_type(5)


      ! ### CRTM Default:
       data CRTMOut_type  /'Brightness_Temp','','','',''/

       data CRTM_EffecR_Water    / 10./
       data CRTM_EffecR_Ice      / 30./
       data CRTM_EffecR_Rain     / 300./
       data CRTM_EffecR_Snow     / 600./
       data CRTM_EffecR_Graupel  / 600./
       data CRTM_EffecR_DUST     / 5./
       data CRTM_Angle_Z         / 0./
       data CRTM_Angle_S         / 0./

       data IRlandCoeff_File  / 'NPOESS.IRland.EmisCoeff.bin'/
       data IRwaterCoeff_File / 'Nalli.IRwater.EmisCoeff.bin'/
       data MWwaterCoeff_File / 'FASTEM5.MWwater.EmisCoeff.bin'/
       data VISlandCoeff_File / 'NPOESS.VISland.EmisCoeff.bin'/



       Interface CRTM_Set_Variable
         module procedure CRTM_Set_Parameter_char
         module procedure CRTM_Set_Parameter_int
         module procedure CRTM_Set_Parameter_int1D
       End Interface



       public :: CRTM_initial
       public :: CRTM_Set_Variable
       public :: CRTM_set_Angle
       public :: CRTM_set_EffecR
       public :: CRTM_set_Kind
       public :: CRTM_set_Coeff
       public :: CRTM_unset_initial
       public :: CRTM_ComputeOut
       public :: CRTM_Setting_Check
       public :: CRTM_Planck
       public :: CRTM_Get_Channels

       private :: Set_SensorData
       private :: Set_Profile
       private :: Set_UnitsIdx

      CONTAINS

       Subroutine CRTM_Set_Parameter_char(Categary,Value,Error)
        implicit none
        integer      :: Error
        character(*) :: Categary
        character(*) :: Value

        select case(Categary)
         case('CRTM_Sensor_ID'); CRTM_Sensor_ID = Value
         case('CRTM_Coeff_Dir'); CRTM_Coeff_Dir = Value
         case default
           write(*,*)'Unknow input Categary:',Categary
           Error = 1
        end select
       Return
       End Subroutine CRTM_Set_Parameter_char

       Subroutine CRTM_Set_Parameter_int(Categary,Value,Error)
        implicit none
        integer      :: Error
        character(*) :: Categary
        integer      :: Value

        select case(Categary)
!         case('CRTM_n_Layers');    CRTM_n_Layers    = Value
         case('CRTM_n_Absorbers'); CRTM_n_Absorbers = Value
         case('CRTM_n_Aerosols');  CRTM_n_Aerosols  = Value
         case('CRTM_n_Clouds');    CRTM_n_Clouds    = Value
         case('CRTM_n_Output');    CRTM_n_Output    = Value
         case default
           write(*,*)'Unknow input Categary:',Categary
           Error = 1
        end select
       Return
       End Subroutine CRTM_Set_Parameter_int


       Subroutine CRTM_Set_Parameter_int1D(Categary,Value,Error)
        implicit none
        integer      :: Error, i, Size_input
        character(*) :: Categary
        integer      :: Value(:), n_value

        n_value = size(Value)
        select case(Categary)
         case('CRTM_Force_ch')
            if(Value(1).ne.0)then
              allocate(CRTM_Force_ch(n_value))
              CRTM_Force_ch = Value
            endif
         case default
           write(*,*)'Unknow input Categary:',Categary
           Error = 1
        end select

       Return
       End Subroutine CRTM_Set_Parameter_int1D


       Function CRTM_Get_Channels()
        implicit none
        integer :: CRTM_Get_Channels(CRTM_n_Channels)
        integer :: c, ln

        ln = 0
        do c = 1 , ChannelInfo(1)%n_Channels
          if(.not.ChannelInfo(1)%Process_Channel(c))cycle
          ln = ln + 1
          CRTM_Get_Channels(ln) = ChannelInfo(1)%Sensor_Channel(c)
        enddo

       Return
       End Function CRTM_Get_Channels



       Subroutine CRTM_initial(n_Profiles,n_Layers)
        implicit none
        integer :: n_Profiles
        integer :: n_Layers
        integer :: istatus
        integer :: Error

        CRTM_n_Layers = n_Layers
        call CRTM_Set_CRTMParameters()
        call CRTM_DeallocateAll()
        allocate(inCRTM_ATM_PressL(n_Profiles,0:n_Layers))
        allocate(inCRTM_ATM_Press(n_Profiles,n_Layers))
        allocate(inCRTM_ATM_Temp(n_Profiles,n_Layers))
        allocate(inCRTM_ATM_Vapor(n_Profiles,n_Layers))
        allocate(inCRTM_ATM_O3(n_Profiles,n_Layers))
        allocate(inCRTM_Aerosol_D(n_Profiles,n_Layers))
        allocate(inCRTM_SFC_Temp(n_Profiles))
        allocate(inCRTM_SFC_Mask(n_Profiles)) 
        allocate(inCRTM_Wind_S(n_Profiles))     
        allocate(inCRTM_Wind_D(n_Profiles))       
        allocate(inCRTM_Sensor_Angle_S(n_Profiles))
        allocate(inCRTM_Sensor_Angle_Z(n_Profiles))
        allocate(inCRTM_Source_Angle_Z(n_Profiles))
        allocate(inCRTM_Source_Angle_A(n_Profiles))
        allocate(CRTM_Layer_Use(n_Profiles,0:n_Layers))

        if(inCRTM_Cloud_W_set)allocate(inCRTM_Cloud_W(n_Profiles,n_Layers))!LWP
        if(inCRTM_Cloud_I_set)allocate(inCRTM_Cloud_I(n_Profiles,n_Layers))!LWP
        if(inCRTM_Cloud_R_set)allocate(inCRTM_Cloud_R(n_Profiles,n_Layers))!LWP
        if(inCRTM_Cloud_S_set)allocate(inCRTM_Cloud_S(n_Profiles,n_Layers))!LWP
        if(inCRTM_Cloud_G_set)allocate(inCRTM_Cloud_G(n_Profiles,n_Layers))!LWP

       ! ## Defaule Value:
        inCRTM_SFC_Mask = -99.
        inCRTM_SFC_Temp = -99.
        inCRTM_Wind_D   = 0.
        inCRTM_Wind_S   = 0.
        CRTM_Layer_Use  = .true.

      !#### CRTM Initial setting ####
        if(STATE_CRTM_Sensor_ID   .ne.CRTM_Sensor_ID    .or. &
           STATE_IRlandCoeff_File .ne.IRlandCoeff_File  .or. &
           STATE_IRwaterCoeff_File.ne.IRwaterCoeff_File .or. &
           STATE_MWwaterCoeff_File.ne.MWwaterCoeff_File .or. &
           STATE_VISlandCoeff_File.ne.VISlandCoeff_File      )then

          allocate(ChannelInfo(CRTM_n_Sensors))
          istatus = CRTM_Init((/CRTM_Sensor_ID/), &
                           ChannelInfo, &
                           File_Path=CRTM_Coeff_Dir, &
                           IRlandCoeff_File=IRlandCoeff_File , &
                           IRwaterCoeff_File=IRwaterCoeff_File , &
                           MWwaterCoeff_File=MWwaterCoeff_File , &
                           VISlandCoeff_File=VISlandCoeff_File , &
                           Quiet=.true.)
          if(Debug_CRTM) write(*,*)'CRTM_Init: ',istatus
          if(istatus.ne.0)then
            write(*,*)'Something wrong with CRTM_Init!'
            if(istatus.eq.3)write(*,*)' Reading Coeff_File error! '
            stop
          endif

          STATE_CRTM_Sensor_ID    = CRTM_Sensor_ID
          STATE_IRlandCoeff_File  = IRlandCoeff_File
          STATE_IRwaterCoeff_File = IRwaterCoeff_File
          STATE_MWwaterCoeff_File = MWwaterCoeff_File
          STATE_VISlandCoeff_File = VISlandCoeff_File
        endif


      !#### CRTM Channel Choose ####
        if(Debug_CRTM)write(*,*)'CRTM Channel Choose:'
        if(allocated(CRTM_Force_ch))then
         if(Debug_CRTM)write(*,*)'CRTM Force Channel: ',CRTM_Force_ch
         istatus = CRTM_ChannelInfo_Subset(ChannelInfo(1), &
                                       channel_Subset=(/CRTM_Force_ch/))
          CRTM_n_Channels = size(CRTM_Force_ch)
        else
          istatus = CRTM_ChannelInfo_Subset(ChannelInfo(1))
          CRTM_n_Channels = ChannelInfo(1)%n_Channels
        endif
        if(Debug_CRTM)write(*,*)'CRTM_ChannelInfo_Subset: ',istatus


        if(Debug_CRTM)then
          write(*,*)'Sensor_ID:       ',ChannelInfo%Sensor_ID
          write(*,*)'Sensor_Type:     ',ChannelInfo%Sensor_Type
          write(*,*)'WMO_Sensor_ID:   ',ChannelInfo%WMO_Sensor_ID
          write(*,*)'WMO_Satellite_ID:',ChannelInfo%WMO_Satellite_ID
          write(*,*)'n_Channels:      ',ChannelInfo%n_Channels
          write(*,*)'Channel Index:   ',ChannelInfo(1)%Channel_Index(:)
          write(*,*)'Sensor Channel:  ',ChannelInfo(1)%Sensor_Channel(:)
        endif





        log_CRTM_initical = .true.
       Return
       End Subroutine CRTM_initial



       Subroutine CRTM_set_Angle(Angle_Z,Angle_S)
        implicit none
        real(8) :: Angle_Z,Angle_S
        CRTM_Angle_Z = Angle_Z
        CRTM_Angle_S = Angle_S
       Return
       End Subroutine CRTM_set_Angle

       Subroutine CRTM_set_EffecR(Categary,R,Error)
        implicit none
        integer      :: Error
        character(*) :: Categary
        real(8)      :: R

        Error = 0
        if(R.le.0)return
        select case (Categary)
          case('ER_Water');   CRTM_EffecR_Water   = R
          case('ER_Ice');     CRTM_EffecR_Ice     = R
          case('ER_Rain');    CRTM_EffecR_Rain    = R
          case('ER_Snow');    CRTM_EffecR_Snow    = R
          case('ER_Graupel'); CRTM_EffecR_Graupel = R
          case('ER_DUST');    CRTM_EffecR_DUST    = R
          case default
            write(*,*)'Unknow Effect R Categary.....',Categary
            Error = 1
        end select
       Return
       End Subroutine CRTM_set_EffecR


       Subroutine CRTM_set_Coeff(Categary,filenum,Error)
        implicit none
        character(*) :: Categary
        integer      :: filenum
        integer      :: Error

        Error = 0
        select case (Categary)
          case('IRland')
            if(filenum.eq.0)then
              IRlandCoeff_File  = 'NPOESS.IRland.EmisCoeff.bin'
            elseif(filenum.eq.1)then
              IRlandCoeff_File  = 'USGS.IRland.EmisCoeff.bin'
            elseif(filenum.eq.2)then
              IRlandCoeff_File  = 'IGBP.IRland.EmisCoeff.bin'
            else
              Error = 2
            endif

          case('IRwater')
            if(filenum.eq.0)then
              IRwaterCoeff_File = 'Nalli.IRwater.EmisCoeff.bin'
            elseif(filenum.eq.1)then
              IRwaterCoeff_File = 'WuSmith.IRwater.EmisCoeff.bin'
            else
              Error = 2
            endif

          case('MWwater')
            if(filenum.eq.0)then
              MWwaterCoeff_File = 'FASTEM5.MWwater.EmisCoeff.bin'
            elseif(filenum.eq.1)then
              MWwaterCoeff_File = 'FASTEM4.MWwater.EmisCoeff.bin'
            else
              Error = 2
            endif

          case('VISland')
            if(filenum.eq.0)then
              VISlandCoeff_File = 'NPOESS.VISland.EmisCoeff.bin'
            elseif(filenum.eq.1)then
              VISlandCoeff_File = 'USGS.VISland.EmisCoeff.bin'
            elseif(filenum.eq.2)then
              VISlandCoeff_File = 'IGBP.VISland.EmisCoeff.bin'
            else
              Error = 2
            endif
          case default
             write(*,*)'Unknow Kind of Coeff Categary.....',Categary
            return
        end select

        if(Error.eq.2)then
           write(*,*)'Undefind number ',filenum,'for ',trim(Categary)
          return
        endif

       Return
       End Subroutine CRTM_set_Coeff

       Subroutine CRTM_unset_initial
        implicit none
        CRTMOut_type = ''
        CRTM_Absorber_Kind = ''
!        CRTM_Absorber_Unit = ''
        CRTM_Aerosol_Kind  = ''
        CRTM_Cloud_Kind    = ''
        log_CRTM_initical = .false.
       Return
       End Subroutine CRTM_unset_initial


       Subroutine CRTM_set_Kind(Categary,numb,dataKind,Error)
        implicit none
        integer :: numb, Error
        character(*) :: Categary
        character(*) :: dataKind

        Error = 0
        select case (Categary)
          case('Cloud')
            select case (dataKind)
              case('CloudWater'); inCRTM_Cloud_W_set = .true.
              case('CloudIce');   inCRTM_Cloud_I_set = .true.
              case('Rain');       inCRTM_Cloud_R_set = .true.
              case('Snow');       inCRTM_Cloud_S_set = .true.
              case('Graupel');    inCRTM_Cloud_G_set = .true.
            end select !dataKind
            CRTM_Cloud_Kind(numb) = dataKind

          case('Aerosol');      CRTM_Aerosol_Kind(numb)  = dataKind
          case('Absorber');     CRTM_Absorber_Kind(numb) = dataKind
          case('CRTM_output');  CRTMOut_type(numb)       = dataKind

          case default
            write(*,*)'Unknow Categary....',Categary
            Error = 1
        end select !Categary


       Return
       End Subroutine CRTM_set_Kind



       Function CRTM_ComputeOut(n_Profiles,Error)
       Use CRTM_Model_Profiles
       Use Module_CRTM_ValueTransport, only: Level_Pressure_get_8
       implicit none

       real    :: CRTM_ComputeOut(CRTM_n_Output, &
                                  CRTM_n_Channels, &
                                  n_Profiles)
       integer  :: i
       integer  :: istatus
       integer  :: Error
       integer  :: n_Profiles
       integer  :: pf, layer, iab, icl, ias, ich, in_pf
       integer  :: Allocate_Status
       integer  :: Using_Layers
       real(fp) :: ZENITH_ANGLE
       real(fp) :: SCAN_ANGLE
       character(256) :: cmdstr, PGname
       logical :: Parameter_Correct
       logical :: USE_CRTM_Standard_Profile


      !##############################
       Error    = 0
       PGname   = 'CRTM_Compute'
       CRTM_ComputeOut = -999.
      !##############################
       if(Debug_CRTM)write(*,*)trim(PGname),' Start:'


      !### CRTM Parameters Ckeck:
       Parameter_Correct = .true.
       if(CRTM_n_Layers.le.0)  Parameter_Correct = .false.
       if(CRTM_n_Output.le.0)  Parameter_Correct = .false.
       if(CRTM_n_Channels.le.0)Parameter_Correct = .false.
       if(n_Profiles.le.0)     Parameter_Correct = .false.
       if(.not.Parameter_Correct)then
         write(*,*)'Error in CRTM Parameter:'
         write(*,*)'CRTM_n_Layers:   ',CRTM_n_Layers
         write(*,*)'CRTM_n_Output:   ',CRTM_n_Output
         write(*,*)'CRTM_n_Channels: ',CRTM_n_Channels
         write(*,*)'n_Profiles:      ',n_Profiles
         Error = 1
         return
       endif


      !#### CRTM Data Array Allocate: ####
       if(Debug_CRTM)print*,'CRTM Data Array Allocate:'
       CRTM_n_Profiles = 0
       do in_pf = 1 , n_Profiles
         if(CRTM_Layer_Use(in_pf,0))CRTM_n_Profiles = CRTM_n_Profiles+1
       enddo

       if(CRTM_n_Profiles.eq.0)then
         write(*,*)trim(PGname),': No profile need to be compute....'
         Error = -1
         return
       endif


      !~~~ K_Matrix: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! allocate(Atm_K(CRTM_n_Channels,CRTM_n_Profiles))
      ! allocate(Sfc_K(CRTM_n_Channels,CRTM_n_Profiles))
      ! allocate(RTSolution_K(CRTM_n_Channels,CRTM_n_Profiles))

      ! do ich = 1, CRTM_n_Channels
      !   call CRTM_Atmosphere_Create(Atm_K(ich,1),CRTM_n_Layers, CRTM_n_Absorbers, &
      !                                            CRTM_n_Clouds, CRTM_n_Aerosols)
      !   call CRTM_Surface_Create(Sfc_K(ich,1),CRTM_n_Channels)
      ! enddo
      ! call CRTM_RTSolution_Create(RTSolution_K,CRTM_n_Layers)
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



      !#### Profile Processing in CRTM: ####
       if(Debug_CRTM)write(*,*)'Atmosphere Infomation Setting start: '
       pf = 1

       if(n_Profiles.ne.1)then
         print*,'********* CRTM Compute Mode Error ********************'
         print*,'  Changeable Layer Mode of CRTM Compute Module    '
         print*,'******************************************************'
         print*,'This version of CRTM_Compute not support multiple data'
         print*,' profiles. Please swith CRTM_Compute.f90 file to other'
         print*,' version and re-build the library again. Or never set '
         print*,' n_Profiles more then 1. '
         stop
       endif


       do in_pf = 1 , n_Profiles

         ! The profile that don not calculate:
         if(.not.CRTM_Layer_Use(in_pf,0))cycle

         ! Find the Number of Useful Layers:
         Using_Layers = 0
         do layer = 1 , CRTM_n_Layers
          if(CRTM_Layer_Use(in_pf,layer))Using_Layers = Using_Layers + 1
         enddo
         if(Using_Layers.eq.0)cycle


         ! Creating the CRTM ATMS Information and Output Array:
         if(Debug_CRTM)print*,'CRTM Data Array Allocate:'
         allocate(Atm(1))
         call CRTM_Atmosphere_Create(Atm, Using_Layers, &
                                          CRTM_n_Absorbers, &
                                          CRTM_n_Clouds, &
                                          CRTM_n_Aerosols)
         allocate(Sfc(1))
         call CRTM_Surface_Create(Sfc,CRTM_n_Channels)

         allocate(Geometry(1))
         call CRTM_Geometry_Create(Geometry)    ! just Is_Allocated

         allocate(Options(1))
         call CRTM_Options_Create(Options,CRTM_n_Channels)

         allocate(RTSolution(CRTM_n_Channels,1))
         call CRTM_RTSolution_Create(RTSolution,Using_Layers)
         RTSolution%Scattering_Flag = .false.


         ! Atmosphere Information Setting:
         if(Debug_CRTM)print*,'Atmosphere Information Setting start: '
         Atm(pf)%Climatology         = TROPICAL
         call Set_Profile(inCRTM_ATM_Press(in_pf,:), &
                          Atm(pf)%Pressure, &
                          CRTM_Layer_Use(in_pf,1:),istatus)
         call Set_Profile(inCRTM_ATM_Temp(in_pf,:), &
                          Atm(pf)%Temperature, &
                          CRTM_Layer_Use(in_pf,1:),istatus)

         Atm(pf)%Level_Pressure(0) = Atm(pf)%Pressure(1) / 2
         do layer = 1 , Using_Layers - 1
           Atm(pf)%Level_Pressure(layer) = (Atm(pf)%Pressure(layer) + &
                                            Atm(pf)%Pressure(layer+1))/2
         enddo
         Atm(pf)%Level_Pressure(Using_Layers) = &
                        Atm(pf)%Level_Pressure(Using_Layers-1) + &
                        Atm(pf)%Pressure(Using_Layers) - &
                        Atm(pf)%Pressure(Using_Layers-1)



         !~ Absorbers:
         do iab = 1 , CRTM_n_Absorbers
           select case (CRTM_Absorber_Kind(iab))
            case('H2O')
              Atm(pf)%Absorber_ID(iab) = H2O_ID
              call Set_Profile(inCRTM_ATM_Vapor(in_pf,:), &
                               Atm(pf)%Absorber(:,iab), &
                               CRTM_Layer_Use(in_pf,1:),istatus)
              Atm(pf)%Absorber_Units(iab) = &
                                     Set_UnitsIdx(inCRTM_UNIT_ATM_Vapor)
            case('O3')
              Atm(pf)%Absorber_ID(iab) = O3_ID
              call Set_Profile(inCRTM_ATM_O3(in_pf,:), &
                               Atm(pf)%Absorber(:,iab), &
                               CRTM_Layer_Use(in_pf,1:),istatus)
              Atm(pf)%Absorber_Units(iab) = &
                                        Set_UnitsIdx(inCRTM_UNIT_ATM_O3)
            case default
              write(*,*)'Error in CRTM Absorber Setting:'
              write(*,*)'Unknow Absorber: ',trim(CRTM_Absorber_Kind(iab))
           end select
         enddo


         !~ Aerosol:
         do ias = 1 , CRTM_n_Aerosols
            select case (CRTM_Aerosol_Kind(iab))
             case('DUST')
               Atm(pf)%Aerosol(ias)%Type = DUST_AEROSOL
               call Set_Profile(inCRTM_Aerosol_D(in_pf,:), &
                                Atm(pf)%Aerosol(ias)%Concentration, &
                                 CRTM_Layer_Use(in_pf,1:),istatus)
               Atm(pf)%Aerosol(ias)%Effective_Radius = CRTM_EffecR_DUST
           end select
         enddo


         !~ Clouds:
         do icl = 1 , CRTM_n_Clouds
             select case (CRTM_Cloud_Kind(icl))
              case('CloudWater')
               Atm(pf)%Cloud(icl)%Type = WATER_CLOUD
               call Set_Profile(inCRTM_Cloud_W(in_pf,:), &
                                Atm(pf)%Cloud(icl)%Water_Content, &
                                CRTM_Layer_Use(in_pf,1:),istatus)
               Atm(pf)%Cloud(icl)%Effective_Radius = CRTM_EffecR_Water
               Atm(pf)%Cloud(icl)%Effective_Variance = 0.05
              case('CloudIce')
               Atm(pf)%Cloud(icl)%Type = ICE_CLOUD
               call Set_Profile(inCRTM_Cloud_I(in_pf,:), &
                                Atm(pf)%Cloud(icl)%Water_Content, &
                                CRTM_Layer_Use(in_pf,1:),istatus)
               Atm(pf)%Cloud(icl)%Effective_Radius = CRTM_EffecR_Ice
               Atm(pf)%Cloud(icl)%Effective_Variance = 0.05
              case('Rain')
               Atm(pf)%Cloud(icl)%Type = RAIN_CLOUD
               call Set_Profile(inCRTM_Cloud_R(in_pf,:), &
                                Atm(pf)%Cloud(icl)%Water_Content, &
                                CRTM_Layer_Use(in_pf,1:),istatus)
               Atm(pf)%Cloud(icl)%Effective_Radius = CRTM_EffecR_Rain
               Atm(pf)%Cloud(icl)%Effective_Variance = 0.5
              case('Snow')
               Atm(pf)%Cloud(icl)%Type = SNOW_CLOUD
               call Set_Profile(inCRTM_Cloud_S(in_pf,:), &
                                Atm(pf)%Cloud(icl)%Water_Content, &
                                CRTM_Layer_Use(in_pf,1:),istatus)
               Atm(pf)%Cloud(icl)%Effective_Radius = CRTM_EffecR_Snow
               Atm(pf)%Cloud(icl)%Effective_Variance = 0.5



              case('Graupel')
               Atm(pf)%Cloud(icl)%Type = GRAUPEL_CLOUD
               call Set_Profile(inCRTM_Cloud_G(in_pf,:), &
                                Atm(pf)%Cloud(icl)%Water_Content, &
                                CRTM_Layer_Use(in_pf,1:),istatus)
               Atm(pf)%Cloud(icl)%Effective_Radius = CRTM_EffecR_Graupel
               Atm(pf)%Cloud(icl)%Effective_Variance = 0.1
              case default
               write(*,*)'CRTM_n_clouds: ',CRTM_n_Clouds
               write(*,*)'Cloud Idx    : ',icl
               write(*,*)'Unknow Cloud : ',CRTM_Cloud_Kind(icl)
               stop
             end select


         enddo !icl = 1 , CRTM_n_Clouds


        ! Surface Infomation Setting:
        if(Debug_CRTM)print*,'Surface Infomation Setting start: '

        ! Reset Coverage Value
        Sfc(pf)%Land_Coverage   = 0.0_fp
        Sfc(pf)%Water_Coverage  = 0.0_fp
        Sfc(pf)%Snow_Coverage   = 0.0_fp
        Sfc(pf)%Ice_Coverage    = 0.0_fp

        ! Surface: Land
        if(inCRTM_SFC_Mask(in_pf).eq.20)then
         Sfc(pf)%Land_Coverage   = 1.0_fp
          Sfc(pf)%Land_Type        = 7
          Sfc(pf)%Land_Temperature = inCRTM_SFC_Temp(in_pf)
          Sfc(pf)%Lai              = 0.65_fp                          !MicroWave
          Sfc(pf)%Soil_Type        = 1  !COARSE_SOIL_TYPE             !MicroWave
          Sfc(pf)%Vegetation_Type  = 11 !BARE_SOIL_VEGETATION_TYPE    !MicroWave

        ! Surface: Sea Water
        elseif(inCRTM_SFC_Mask(in_pf).eq.10)then
         Sfc(pf)%Water_Coverage  = 1.0_fp
          Sfc(pf)%Water_Type        = 1 !SEA WATER
          Sfc(pf)%Water_Temperature = inCRTM_SFC_Temp(in_pf)
          Sfc(pf)%Wind_Speed        = inCRTM_Wind_S(in_pf)
          Sfc(pf)%Wind_Direction    = inCRTM_Wind_D(in_pf)
          Sfc(pf)%Salinity          = 33  ! /1000

        ! Surface: Ice
        elseif(inCRTM_SFC_Mask(in_pf).eq.11)then
         Sfc(pf)%Ice_Coverage    = 1.0_fp
          Sfc(pf)%Ice_Type          = 1 !New Ice
          Sfc(pf)%Ice_Temperature   = inCRTM_SFC_Temp(in_pf)

        ! Surface: a part od Sea Ice
        elseif(inCRTM_SFC_Mask(in_pf).eq.12)then
         Sfc(pf)%Water_Coverage  = 0.5_fp
         Sfc(pf)%Ice_Coverage    = 0.5_fp
          Sfc(pf)%Water_Type        = 1 !SEA WATER
          Sfc(pf)%Water_Temperature = inCRTM_SFC_Temp(in_pf)
          Sfc(pf)%Wind_Speed        = inCRTM_Wind_S(in_pf)
          Sfc(pf)%Wind_Direction    = inCRTM_Wind_D(in_pf)
          Sfc(pf)%Salinity          = 33  ! /1000
          Sfc(pf)%Ice_Type          = 1 !New Ice
          Sfc(pf)%Ice_Temperature   = inCRTM_SFC_Temp(in_pf)

        ! Surface: Coast
        elseif(inCRTM_SFC_Mask(in_pf).eq.30)then
         Sfc(pf)%Water_Coverage  = 0.5_fp
         Sfc(pf)%Land_Coverage   = 0.5_fp
          Sfc(pf)%Water_Type        = 1 !SEA WATER
          Sfc(pf)%Water_Temperature = inCRTM_SFC_Temp(in_pf)
          Sfc(pf)%Wind_Speed        = inCRTM_Wind_S(in_pf)
          Sfc(pf)%Wind_Direction    = inCRTM_Wind_D(in_pf)
          Sfc(pf)%Salinity          = 33  ! /1000
          Sfc(pf)%Land_Type         = 7
          Sfc(pf)%Land_Temperature  = inCRTM_SFC_Temp(in_pf)
          Sfc(pf)%Lai               = 0.65_fp                        !MicroWave
          Sfc(pf)%Soil_Type         = 1  !COARSE_SOIL_TYPE           !MicroWave
          Sfc(pf)%Vegetation_Type   = 11 !BARE_SOIL_VEGETATION_TYPE  !MicroWave
        else
         print*,'###################################'
         print*,' New Surface_Mask appear! '
         print*,'  Input Profile: ',in_pf
         print*,'  Surface_Mask: ',inCRTM_SFC_Mask(in_pf)
         print*,'###################################'
         stop
        endif


       ! Geometry Information Setting:

        call CRTM_Geometry_SetValue(Geometry(pf), &
                 Sensor_Zenith_Angle  = inCRTM_Sensor_Angle_Z(in_pf), &
                 Sensor_Scan_Angle    = inCRTM_Sensor_Angle_S(in_pf), &
                 Source_Zenith_Angle  = inCRTM_Source_Angle_Z(in_pf), &
                 Source_Azimuth_Angle = inCRTM_Source_Angle_A(in_pf))

!        call CRTM_Geometry_SetValue(Geometry(pf),
!     &           Sensor_Zenith_Angle  = inCRTM_Sensor_Angle_Z(in_pf),
!     &           Sensor_Scan_Angle    = inCRTM_Sensor_Angle_S(in_pf))


       enddo !in_pf


      !#### Options Set Value:
       Options(:)%Use_Emissivity          = .FALSE.
       Options(:)%Use_Direct_Reflectivity = .FALSE.


      !#### Initial CRTM Output Array:
       RTSolution(:,:)%Radiance = 0
       RTSolution(:,:)%Brightness_Temperature = 1

      !#### Setting Sensor Data:
       if(Debug_CRTM)write(*,*)'Setting Sensor Data:'
       call Set_SensorData(istatus)


      ! ##### CRTM Module Standard Profile:
       USE_CRTM_Standard_Profile = .false.
      ! USE_CRTM_Standard_Profile = .true.
       if(USE_CRTM_Standard_Profile)then
         Using_Layers = 100 !reset n_Layers for CRTM Standard Profile
         deallocate(Atm)
         deallocate(RTSolution)
         allocate(Atm(CRTM_n_Profiles))
         allocate(RTSolution(CRTM_n_Channels, CRTM_n_Profiles), &
                   STAT = Allocate_Status)
         call CRTM_Atmosphere_Create(Atm, Using_Layers, &
                                          CRTM_n_Absorbers, &
                                          CRTM_n_Clouds, &
                                          CRTM_n_Aerosols)
         call CRTM_RTSolution_Create(RTSolution,Using_Layers)

         Atm(1)%Absorber_ID(1) = H2O_ID
         Atm(1)%Absorber_ID(2) = O3_ID
         Atm(1)%Absorber_Units(1) = MASS_MIXING_RATIO_UNITS
         Atm(1)%Absorber_Units(2) = VOLUME_MIXING_RATIO_UNITS
         Atm(1)%Climatology    = TROPICAL



         write(*,*)'CRTM_Get_Model_Profile:'
         CALL CRTM_Get_Model_Profile( Atm(1)%Absorber_ID(:)  , &
                                      Atm(1)%Pressure(:)     , &
                                      Atm(1)%Temperature(:)  , &
                                      Atm(1)%Absorber(:,:)   , &
                                      Model=TROPICAL  )
         write(*,*)'CRTM_Get_Model_Profile end'
         call Level_Pressure_get_8(Using_Layers,Atm(1)%Pressure(:), &
                                   Atm(1)%Level_Pressure(:))
       endif !USE_CRTM_Standard_Profile


      !#### Start CRTM_Forward:
         if(Debug_CRTM)write(*,*)'****** Start CRTM_Forward: ********'

!           print*,RTSolution(1,1)%Sensor_Channel
           istatus = CRTM_Forward( Atm, Sfc, Geometry, ChannelInfo, &
                                   RTSolution ,Options = Options)

!           print*,ChannelInfo(1)%Sensor_Channel(:)
!           print*,ChannelInfo(1)%Process_Channel(:)
!           print*,RTSolution(1,1)%Sensor_Channel
!           print*,'akey test'
!           stop


           if(istatus.eq.0)then
             Error = 0
           elseif(istatus.eq.1)then
             Error = -1
             write(*,*)trim(PGname),' : CRTM_Forward is fail.....'
           else
             Error = -2
             write(*,*)trim(PGname), &
                      ' : Unknow Error in CRTM_Forward....',istatus
           endif

         if(Debug_CRTM)write(*,*)'********  End  CRTM_Forward: ********'
         if(Debug_CRTM)call CRTM_Setting_Check(1,Using_Layers)
         if(Debug_CRTM)read(*,*)



      !!!! K_Matrix:
      !   print*,'Start CRTM_K_Matrix:'
      !   Atm_K(:,1) = Atm(1)
      !   istatus =   CRTM_K_Matrix( Atm                 , &
      !                              Sfc                 , &
      !                              RTSolution_K        , &
      !                              Geometry            , &
      !                              ChannelInfo         , &
      !                              Atm_K               , &
      !                              Sfc_K               , &
      !                              RTSolution          , &
      !                              Options = Options    )
      !   print*,'End CRTM_K_Matrix!'
      !
      !   if(Debug_CRTM_out) call CRTM_Setting_Check(CRTM_n_Layers)



      !####  Out put Array:
       if(Debug_CRTM)write(*,*)'Out put Array:'
       pf = 0
       do in_pf = 1 , n_Profiles

        if(CRTM_Layer_Use(in_pf,0))then
          pf = pf + 1

         do i = 1 , CRTM_n_Output
          select case (CRTMOut_type(i))
           case('Brightness_Temp')
             CRTM_ComputeOut(i,:,in_pf) = &
                                RTSolution(:,pf)%Brightness_Temperature
           case('Radiance')
             CRTM_ComputeOut(i,:,in_pf) = &
                                RTSolution(:,pf)%Radiance
           case('Up_R')
             CRTM_ComputeOut(i,:,in_pf) = &
                                RTSolution(:,pf)%Up_Radiance
           case('Down_R')
             CRTM_ComputeOut(i,:,in_pf) = &
                                RTSolution(:,pf)%Down_Radiance
           case('Surf_Plank_R')
             CRTM_ComputeOut(i,:,in_pf) = &
                                RTSolution(:,pf)%Surface_Planck_Radiance
           case('SurfEmissivity')
             CRTM_ComputeOut(i,:,in_pf) = &
                                RTSolution(:,pf)%Surface_Emissivity
          end select

         enddo
        endif !CRTM_Layer_Use(in_pf,0)

       enddo !in_pf


      !#### destroy CRTM:
       if(Debug_CRTM)write(*,*)'Destroy CRTM:'
       call CRTM_Atmosphere_Destroy(Atm)
       call CRTM_Surface_Destroy(Sfc)
       call CRTM_Geometry_Destroy(Geometry)
       call CRTM_Options_Destroy(Options)
       deallocate(Atm)
       deallocate(Sfc)
       deallocate(Geometry)
       deallocate(Options)
       deallocate(RTSolution)


       Return
       End Function CRTM_ComputeOut

  
!       Function CRTM_Get_Channels()
!        implicit none
!        integer :: CRTM_Get_Channels(CRTM_n_Channels)
        

!       Return
!       End Function CRTM_Get_Channels 

!        RTSolution
!        RTSolution(ich,pf)%Sensor_Channel






 
       Subroutine CRTM_Set_CRTMParameters()
        USE Module_CRTM_Parameters
        USE Module_a_function, only:A_STR_2_Integer
        implicit none
        integer :: Error

       ! ### CRTM Parameters Setting:
        call CRTM_Set_Variable('CRTM_Sensor_ID'  ,CRTMP_Sensor_ID,0)
        call CRTM_Set_Variable('CRTM_Coeff_Dir'  ,CRTMP_Coeff_Dir,0)
        call CRTM_Set_Variable('CRTM_n_Absorbers',CRTMP_n_Absorbers,0)
        call CRTM_Set_Variable('CRTM_n_Aerosols' ,CRTMP_n_Aerosols,0)
        call CRTM_Set_Variable('CRTM_n_Clouds'   ,CRTMP_n_Clouds,0)
        call CRTM_Set_Variable('CRTM_n_Output'   ,CRTMP_n_Output,0)

       ! ### CRTM Effect Radian Setting:
        call CRTM_set_EffecR('ER_Water'  ,CRTMP_EffecR_Water  ,Error)
        call CRTM_set_EffecR('ER_Ice'    ,CRTMP_EffecR_Ice    ,Error)
        call CRTM_set_EffecR('ER_Rain'   ,CRTMP_EffecR_Rain   ,Error)
        call CRTM_set_EffecR('ER_Snow'   ,CRTMP_EffecR_Snow   ,Error)
        call CRTM_set_EffecR('ER_Graupel',CRTMP_EffecR_Graupel,Error)

       ! ### CRTM Coefficient File Setting:
        call CRTM_set_Coeff('IRland' ,CRTMP_Coeff_IRland ,Error)
        call CRTM_set_Coeff('IRwater',CRTMP_Coeff_IRwater,Error)
        call CRTM_set_Coeff('MWwater',CRTMP_Coeff_MWwater,Error)
        call CRTM_set_Coeff('MWwater',CRTMP_Coeff_VISland,Error)
        call CRTM_set_Kind('Absorber',1,CRTMP_Absorber1,Error)
        call CRTM_set_Kind('Absorber',2,CRTMP_Absorber2,Error)
        call CRTM_set_Kind('Absorber',3,CRTMP_Absorber3,Error)
        call CRTM_set_Kind('Cloud',1,CRTMP_Cloud1,Error)
        call CRTM_set_Kind('Cloud',2,CRTMP_Cloud2,Error)
        call CRTM_set_Kind('Cloud',3,CRTMP_Cloud3,Error)
        call CRTM_set_Kind('Cloud',4,CRTMP_Cloud4,Error)
        call CRTM_set_Kind('Cloud',5,CRTMP_Cloud5,Error)
        call CRTM_set_Kind('Cloud',6,CRTMP_Cloud6,Error)
        call CRTM_set_Kind('CRTM_output',1,CRTMP_output1,Error)
        call CRTM_set_Kind('CRTM_output',2,CRTMP_output2,Error)
        call CRTM_set_Kind('CRTM_output',3,CRTMP_output3,Error)
        call CRTM_set_Kind('CRTM_output',4,CRTMP_output4,Error)
        call CRTM_set_Kind('CRTM_output',5,CRTMP_output5,Error)

        Error = A_STR_2_Integer(CRTMP_Force_ch,Force_ch_array)
        if(Error.eq.0) &
         call CRTM_Set_Variable('CRTM_Force_ch',Force_ch_array,0)

       Return
       End Subroutine CRTM_Set_CRTMParameters



       Subroutine CRTM_DeallocateAll()
        if(allocated(ChannelInfo))      deallocate(ChannelInfo)
        if(allocated(inCRTM_ATM_PressL))deallocate(inCRTM_ATM_PressL)
        if(allocated(inCRTM_ATM_Press)) deallocate(inCRTM_ATM_Press)
        if(allocated(inCRTM_ATM_Temp))  deallocate(inCRTM_ATM_Temp)
        if(allocated(inCRTM_ATM_Vapor)) deallocate(inCRTM_ATM_Vapor)
        if(allocated(inCRTM_ATM_O3))    deallocate(inCRTM_ATM_O3)
        if(allocated(inCRTM_Cloud_W))   deallocate(inCRTM_Cloud_W)
        if(allocated(inCRTM_Cloud_I))   deallocate(inCRTM_Cloud_I)
        if(allocated(inCRTM_Cloud_R))   deallocate(inCRTM_Cloud_R)
        if(allocated(inCRTM_Cloud_S))   deallocate(inCRTM_Cloud_S)
        if(allocated(inCRTM_Cloud_G))   deallocate(inCRTM_Cloud_G)
        if(allocated(inCRTM_Aerosol_D)) deallocate(inCRTM_Aerosol_D)
        if(allocated(inCRTM_SFC_Temp))  deallocate(inCRTM_SFC_Temp)
        if(allocated(inCRTM_SFC_Mask))  deallocate(inCRTM_SFC_Mask)
        if(allocated(inCRTM_Wind_S))    deallocate(inCRTM_Wind_S)
        if(allocated(inCRTM_Wind_D))    deallocate(inCRTM_Wind_D)
        if(allocated(inCRTM_Sensor_Angle_S))deallocate(inCRTM_Sensor_Angle_S)
        if(allocated(inCRTM_Sensor_Angle_Z))deallocate(inCRTM_Sensor_Angle_Z)
        if(allocated(inCRTM_Source_Angle_Z))deallocate(inCRTM_Source_Angle_Z)
        if(allocated(inCRTM_Source_Angle_A))deallocate(inCRTM_Source_Angle_A)
        if(allocated(CRTM_Layer_Use)) deallocate(CRTM_Layer_Use)
       Return
       End Subroutine CRTM_DeallocateAll
  

       subroutine CRTM_Setting_Check(pf,Layers)
        implicit none
        integer :: Layers
        integer :: pf, l
        integer :: icl, iab, ich

        character(50) :: Climatology(6)
        character(10) :: Cloud_Type(6)
        character(5)  :: Absorber_Type(6)
        character(50) :: Water_Unit(10)


        Climatology(TROPICAL) = 'Tropical'
        Climatology(MIDLATITUDE_SUMMER) = 'Midlatitude summer'
        Climatology(MIDLATITUDE_WINTER) = 'Midlatitude winter'
        Climatology(SUBARCTIC_SUMMER) = 'Subarctic summer'
        Climatology(SUBARCTIC_WINTER) = 'Subarctic winter'
        Climatology(US_STANDARD_ATMOSPHERE) = 'U.S. Standard Atmosphere'

        Cloud_Type(WATER_CLOUD) = 'Water'
        Cloud_Type(ICE_CLOUD) = 'Ice'
        Cloud_Type(RAIN_CLOUD) = 'Rain'
        Cloud_Type(SNOW_CLOUD) = 'Snow'
        Cloud_Type(GRAUPEL_CLOUD) = 'Graupel'
        Cloud_Type(HAIL_CLOUD) = 'Hail'

        Absorber_Type(H2O_ID) = 'H2O'
        Absorber_Type(CO2_ID) = 'CO2'
        Absorber_Type(O3_ID) = 'O3'
        Absorber_Type(N2O_ID) = 'N2O'
        Absorber_Type(CO_ID) = 'CO'
        Absorber_Type(CH4_ID) = 'CH4'

        Water_Unit(VOLUME_MIXING_RATIO_UNITS) = &
                         'Volume mixing ratio, ppmv'
        Water_Unit(NUMBER_DENSITY_UNITS) = 'Number density, cm−3'
        Water_Unit(MASS_MIXING_RATIO_UNITS) = 'Mass mixing ratio, g/kg'
        Water_Unit(MASS_DENSITY_UNITS) = 'Mass density, g/m3'
        Water_Unit(PARTIAL_PRESSURE_UNITS) = 'Partial pressure, hPa'
        Water_Unit(DEWPOINT_TEMPERATURE_K_UNITS) = &
                          'Dewpoint temperature, K (H2O ONLY)'
        Water_Unit(DEWPOINT_TEMPERATURE_C_UNITS) = &
                          'Dewpoint temperature, C (H2O ONLY)'
        Water_Unit(RELATIVE_HUMIDITY_UNITS) = &
                          'Relative humidity, % (H2O ONLY)'
        Water_Unit(SPECIFIC_AMOUNT_UNITS) = 'Specific amount, g/g'
        Water_Unit(INTEGRATED_PATH_UNITS) = 'Integrated path, mm'



        print*,'#######################################################'
        print*,'Show Information:'
        print*,' profile: ',pf
        print*,' num Layers:',Layers
        print*,' ATM Climatology: ',Atm(pf)%Climatology, &
                                  trim(Climatology(Atm(pf)%Climatology))
        print*,''
        write(*,221)'## Profile Units ##'
        do iab = 1 , CRTM_n_Absorbers
         write(*,'(a20,2x,a35,f5.1)') &
                trim(Absorber_Type(Atm(pf)%Absorber_ID(iab))), &
                trim(Water_Unit(Atm(pf)%Absorber_Units(iab)))
        enddo
        do icl = 1 , CRTM_n_Clouds
         write(*,'(a20,2x,a30,f5.1)') &
         trim(Cloud_Type(Atm(pf)%Cloud(icl)%type))//'(Effect_R)', &
         'LWP kg/m2 ',Atm(pf)%Cloud(icl)%Effective_Radius(1)
        enddo



  201   format(10a10)   ! Show Title
  202   format(10f10.4) ! Show Data Value
        write(*,201) 'Pressure','Temp', &
          (trim(Absorber_Type(Atm(pf)%Absorber_ID(iab))), &
            iab=1,CRTM_n_Absorbers), &
          (trim(Cloud_Type(Atm(pf)%Cloud(icl)%type)), &
            icl=1,CRTM_n_Clouds)
        do l = 1 , Layers
         write(*,202) Atm(pf)%Pressure(l),Atm(pf)%Temperature(l), &
                     (Atm(pf)%Absorber(l,iab),iab=1,CRTM_n_Absorbers), &
                     (Atm(pf)%Cloud(icl)%Water_Content(l), &
                         icl=1,CRTM_n_Clouds)
        enddo


  203   format(a20,22f9.3)
  204   format(a20,22i9)
  205   format(a20,22a9)
  213   format(3(a20,f9.4))
  214   format(3(a20,i9))
  221   format(a20,a30)
        print*,''
        write(*,221)'## Surface Type ##'
        write(*,213)'Water_Coverage: ',Sfc(pf)%Water_Coverage, &
                    'Land_Coverage: ' ,Sfc(pf)%Land_Coverage, &
                    'Ice_Coverage: '  ,Sfc(pf)%Ice_Coverage
        write(*,214)'Water_Type: ',Sfc(pf)%Water_Type, &
                    'Land_Type: ' ,Sfc(pf)%Land_Type, &
                    'Ice_Type: '  ,Sfc(pf)%Ice_Type
        write(*,213)'Water_Temperature: ',Sfc(pf)%Water_Temperature, &
                    'Land_Temperature: ' ,Sfc(pf)%Land_Temperature, &
                    'Ice_Temperature: '  ,Sfc(pf)%Ice_Temperature
        write(*,213)'Wind Speed: ',Sfc(pf)%Wind_Speed, &
                    'Wind Direction: ',Sfc(pf)%Wind_Direction

        write(*,203)''
        write(*,221)'## Geometry Angle ##'
        write(*,213)'Sensor_Zenith: ', &
                              Geometry(pf)%Sensor_Zenith_Angle, &
                    'Sensor_Scan: ',Geometry(pf)%Sensor_Scan_Angle
        write(*,213)'Source_Zenith: ', &
                              Geometry(pf)%Source_Zenith_Angle, &
              'Source_Azimuth: ',Geometry(pf)%Source_Azimuth_Angle


        write(*,*)''
        write(*,221)'## CRTM Result ##'
        write(*,221)'Sensor_ID',RTSolution(1,pf)%Sensor_ID
        write(*,204)'Channel',(RTSolution(ich,pf)%Sensor_Channel, &
                               ich=1,CRTM_n_Channels)
        write(*,203)'SurfEmissivity', &
           (RTSolution(ich,pf)%Surface_Emissivity,ich=1,CRTM_n_Channels)
        write(*,203)'SOD',(RTSolution(ich,pf)%SOD,ich=1,CRTM_n_Channels)
        write(*,203)'Up_R',(RTSolution(ich,pf)%Up_Radiance, &
                              ich=1,CRTM_n_Channels)
        write(*,203)'Down_R',(RTSolution(ich,pf)%Down_Radiance, &
                                ich=1,CRTM_n_Channels)
        write(*,203)'Down_Solar_R', &
                         (RTSolution(ich,pf)%Down_Solar_Radiance, &
                          ich=1,CRTM_n_Channels)
        write(*,203)'Surf_Plank_R', &
                          (RTSolution(ich,pf)%Surface_Planck_Radiance, &
                                                ich=1,CRTM_n_Channels)
        write(*,203)'Radiance',(RTSolution(ich,pf)%Radiance, &
                                    ich=1,CRTM_n_Channels)
        write(*,203)'Brightness_Temp', &
                           (RTSolution(ich,pf)%Brightness_Temperature, &
                            ich=1,CRTM_n_Channels)

!        read(*,*)
!        return


!        write(*,203)'Sensor%Tb',(Sfc(pf)%SensorData%Tb(ich),
!     &                             ich=1,CRTM_n_Channels)

      !  print*,'Up_R(CH1)',RTSolution(1,pf)%Down_Solar_Radiance
      !  print*,'Up_R(CH5)',RTSolution(5,pf)%Down_Solar_Radiance
      !  print*,'Up_R(CH9)',RTSolution(9,pf)%Down_Solar_Radiance

      !  print*,'Down_Solar_R(CH1)',RTSolution(1,pf)%Down_Solar_Radiance
      !  print*,'Down_Solar_R(CH5)',RTSolution(5,pf)%Down_Solar_Radiance
      !  print*,'Down_Solar_R(CH9)',RTSolution(9,pf)%Down_Solar_Radiance

!        print*,'Surf_Plank_R(CH1)',
!     &             RTSolution(1,pf)%Surface_Planck_Radiance
      !  print*,'Surf_Plank_R(CH5)',RTSolution(5,pf)%Surface_Planck_Radiance
      !  print*,'Surf_Plank_R(CH9)',RTSolution(9,pf)%Surface_Planck_Radiance

      !  print*,'RTSolution Layer_Optical_Depth:'
      !  do l = 1 , Layers
      !    print*,RTSolution(9,1)%Layer_Optical_Depth(l)
      !  enddo

        write(*,*)'Options:'
        write(*,*)'Check_Input: ',Options%Check_Input
        write(*,*)'Use_Old_MWSSEM: ',Options%Use_Old_MWSSEM
        write(*,*)'Use_Antenna_Correction: ', &
                                       Options%Use_Antenna_Correction
        write(*,*)'Apply_NLTE_Correction: ', &
                                        Options%Apply_NLTE_Correction
        write(*,*)'RT_Algorithm_Id: ',Options%RT_Algorithm_Id
        write(*,*)'Aircraft_Pressure: ',Options%Aircraft_Pressure
        write(*,*)'Use_n_Streams: ',Options%Use_n_Streams
        write(*,*)'n_Streams: ',Options%n_Streams
        write(*,*)'Option Include_Scattering: ', &
                                           Options%Include_Scattering
        write(*,*)'n_Channels: ',Options%n_Channels
        write(*,*)'Channel: ',Options%Channel
        write(*,*)'Use_Emissivity: ',Options(pf)%Use_Emissivity
      !  write(*,*)'Emissivity: ',Options(1)%Emissivity
      !  write(*,*)'Direct_Reflectivity: ',Options(1)%Direct_Reflectivity
      !  write(*,*)'SSU: ',Options(1)%SSU
      !  write(*,*)'Zeeman: ',Options(1)%Zeeman


       return
       end subroutine CRTM_Setting_Check

       subroutine Set_SensorData(istatus)
        implicit none
        integer :: istatus, ch, idx, pf
        integer :: num_Channel
        TYPE(CRTM_SensorData_type) :: SensorData


        do pf = 1 , CRTM_n_Profiles
          Sfc(pf)%SensorData%Sensor_Id        = ChannelInfo(1)%Sensor_ID
          Sfc(pf)%SensorData%WMO_Satellite_Id = &
                                         ChannelInfo(1)%WMO_Satellite_ID
          Sfc(pf)%SensorData%WMO_Sensor_Id    = &
                                            ChannelInfo(1)%WMO_Sensor_ID
       enddo

        num_Channel = size(Sfc(1)%SensorData%Sensor_Channel)
        idx = 0
        do ch = 1 , size(ChannelInfo(1)%Process_Channel)
          if(ChannelInfo(1)%Process_Channel(ch))then
            idx = idx + 1
            if(idx.gt.num_Channel)then
              print*,'Error Set SensorData%Sensor_Channel...'
              istatus = 1
              return
            endif
            do pf = 1 , CRTM_n_Profiles
              Sfc(pf)%SensorData%Sensor_Channel(idx) = ch
            enddo
          endif
        enddo
        istatus = 0
       return
       end subroutine Set_SensorData


! ################################################################
!   Name: Set_Profile
!   Purpose: To assign Atmosphere Data Profile in CRTM Data Array.
! ################################################################
       subroutine Set_Profile(Sourse_Data,CRTM_Data,Layer_Use,istatus)
        implicit none
        real     :: Sourse_Data(CRTM_n_Layers) ! Input
        real(fp) :: CRTM_Data(:)     ! Output
        integer  :: layer, Layer_idx
        integer  :: istatus
        logical  :: Layer_Use(:)


        Layer_idx = 0
        do layer = 1 , CRTM_n_Layers
          if(Layer_Use(layer))then
            Layer_idx = Layer_idx + 1
            CRTM_Data(Layer_idx) = Sourse_Data(layer)
          endif
         enddo

       return
       end subroutine Set_Profile

       Integer Function Set_UnitsIdx(Units_Str)
        implicit none
        character(*) :: Units_Str

        select case(Units_Str)
          case('VOLUME_MIXING_RATIO')
            Set_UnitsIdx = VOLUME_MIXING_RATIO_UNITS
          case('NUMBER_DENSITY')
            Set_UnitsIdx = NUMBER_DENSITY_UNITS
          case('MASS_MIXING_RATIO')
            Set_UnitsIdx = MASS_MIXING_RATIO_UNITS
          case('MASS_DENSITY')
            Set_UnitsIdx = MASS_DENSITY_UNITS
          case('PARTIAL_PRESSURE')
            Set_UnitsIdx = PARTIAL_PRESSURE_UNITS
          case('DEWPOINT_TEMPERATURE_K')
            Set_UnitsIdx = DEWPOINT_TEMPERATURE_K_UNITS
          case('DEWPOINT_TEMPERATURE_C')
           Set_UnitsIdx = DEWPOINT_TEMPERATURE_C_UNITS
          case('RELATIVE_HUMIDITY')
            Set_UnitsIdx = RELATIVE_HUMIDITY_UNITS
          case('SPECIFIC_AMOUNT')
            Set_UnitsIdx = SPECIFIC_AMOUNT_UNITS
          case('INTEGRATED_PATH')
            Set_UnitsIdx = INTEGRATED_PATH_UNITS
          case default
            write(*,*)'Error in CRTM USet_UnitsIdx....'
            write(*,*)'Unknow CRTM Unit: ',trim(Units_Str)
            Set_UnitsIdx = -1
        end select
       Return
       End Function Set_UnitsIdx


       subroutine CRTM_Planck(ChannelIndex,Temperature,Radiance)
        implicit none
        integer :: SensorIndex
        integer :: ChannelIndex
        real(8)    :: Temperature
        real(8)    :: Radiance

        SensorIndex  = 1  !first sensor that you set
      ! ChannelIndex = 4  ! 4th channel

        call CRTM_Planck_Radiance(SensorIndex,ChannelIndex, &
                                  Temperature,Radiance)


       return
       end subroutine CRTM_Planck

  
       




      END MODULE Module_CRTM_Compute
                                                                                                                                                                                                                                                          
