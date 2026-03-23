!#################################################################
!
! Name : Module_CRTM_Parameters
! Purpose : Setting the CRTM Common Parameters.
!
! First Version                     Akey Chen in UMD  2014/04/02
! Modify by                         Akey Chen in UMD  2014/07/15
! Modify by                         Akey Chen in CWB  2017/09/04
!#################################################################
      MODULE Module_CRTM_Parameters !Parameter Module
       implicit none
  
       character(120) :: CRTMP_NWPname     ! NWP Name 


      ! CRTM Directory Setting:
       character(120) :: CRTMP_Coeff_Dir   ! Coefficient Folder
       character(120) :: CRTMP_Input_Dir   ! Model Sourse Folder
       character(120) :: CRTMP_Output_Dir  ! CRTM Output Folder
 
      ! CRTM Setting Contral:
       character(50) :: CRTMP_Sensor_ID      ! ID of Satellite Sensor
       integer :: CRTMP_n_Clouds             ! Number of Clouds
       integer :: CRTMP_n_Absorbers          ! Number of Absorbers
       integer :: CRTMP_n_Aerosols           ! Number of Aerosols
       integer :: CRTMP_n_Output             ! Number of CRTM output
       character(50) :: CRTMP_Force_ch       ! CRTM output channels
       integer, allocatable :: Force_ch_array(:)

      ! CRTM ATM Information Consider:
       character(30) :: CRTMP_Absorber1
       character(30) :: CRTMP_Absorber2
       character(30) :: CRTMP_Absorber3
       character(30) :: CRTMP_Cloud1
       character(30) :: CRTMP_Cloud2
       character(30) :: CRTMP_Cloud3
       character(30) :: CRTMP_Cloud4
       character(30) :: CRTMP_Cloud5
       character(30) :: CRTMP_Cloud6

       character(30) :: CRTMP_output1
       character(30) :: CRTMP_output2
       character(30) :: CRTMP_output3
       character(30) :: CRTMP_output4
       character(30) :: CRTMP_output5

       integer, parameter :: CRTMF_n_outputs=5
       character(30) :: CRTMF_outputs(CRTMF_n_outputs)
  

      ! CRTM Computed Parameters:
       integer :: CRTMP_AssignAngle    ! Assign a Single Value for Angle
       real    :: CRTMP_Sensor_Angle_S ! Scan Angle of Sensor
       real    :: CRTMP_Sensor_Angle_Z ! Zenith Angle of Sensor
       real    :: CRTMP_Source_Angle_Z ! Zenith Angle of Source
       real    :: CRTMP_Source_Angle_A ! Azimuth Angle of Source
       real(8) :: CRTMP_EffecR_Water   ! Effective Radian of Water
       real(8) :: CRTMP_EffecR_Ice     ! Effective Radian of Ice
       real(8) :: CRTMP_EffecR_Rain    ! Effective Radian of Rain
       real(8) :: CRTMP_EffecR_Snow    ! Effective Radian of Snow
       real(8) :: CRTMP_EffecR_Graupel ! Effective Radian of Graupel
       integer :: CRTMP_Coeff_IRland    ! Coefficient File of IR in Land
       integer :: CRTMP_Coeff_IRwater   ! Coefficient File of IR in Water
       integer :: CRTMP_Coeff_MWwater   ! Coefficient File of MW in Water
       integer :: CRTMP_Coeff_VISland   ! Coefficient File of VIS in Land

      ! CRTM input Ratio Parameters:
       real :: Ratio_Cloud_W
       real :: Ratio_Cloud_I
       real :: Ratio_Cloud_R
       real :: Ratio_Cloud_S
       real :: Ratio_Cloud_G
       real :: Ratio_Vapor
       real :: Ratio_O3

      ! CRTM OutPut Information:
       logical :: Debug_CRTMP
       integer :: CRTMP_Debug_Main
       integer :: CRTMP_Debug_CRTMP
       integer :: CRTMP_Debug_ModelLoad
       integer :: CRTMP_Debug_CRTM


       public :: MC_Parameter_Default
       public :: MC_Parameter_Setting
       public :: MC_Parameter_Show
       public :: MC_Parameter_AlbedoCeoeff
       public :: MC_Parameter_WaveLength
       public :: MC_Parameter_ChName

      CONTAINS

       Subroutine MC_Parameter_Default()
        implicit none
        CRTMP_NWPname     = ''
        CRTMP_n_Clouds    = 0
        CRTMP_n_Absorbers = 0
        CRTMP_n_Aerosols  = 0
        CRTMP_n_Output    = 0

        CRTMP_Force_ch   = ''
        CRTMP_Sensor_ID  = ''
        CRTMP_Coeff_Dir  = ''
        CRTMP_Input_Dir  = ''
        CRTMP_Output_Dir = ''

        CRTMP_Absorber1 = ''
        CRTMP_Absorber2 = ''
        CRTMP_Absorber3 = ''
        CRTMP_Cloud1   = ''
        CRTMP_Cloud2   = ''
        CRTMP_Cloud3   = ''
        CRTMP_Cloud4   = ''
        CRTMP_Cloud5   = ''
        CRTMP_Cloud6   = ''
        CRTMP_output1  = ''
        CRTMP_output2  = ''
        CRTMP_output3  = ''
        CRTMP_output4  = ''
        CRTMP_output5  = ''
        CRTMF_outputs  = ''

        CRTMP_AssignAngle    =   0.
        CRTMP_Sensor_Angle_S =   0.
        CRTMP_Sensor_Angle_Z =   0.
        CRTMP_Source_Angle_Z =   0.
        CRTMP_Source_Angle_A =   0.
        CRTMP_EffecR_Water   =  10.
        CRTMP_EffecR_Ice     =  30.
        CRTMP_EffecR_Rain    = 300.
        CRTMP_EffecR_Snow    = 600.
        CRTMP_EffecR_Graupel = 600.
        CRTMP_Coeff_IRland   = 0
        CRTMP_Coeff_IRwater  = 0
        CRTMP_Coeff_MWwater  = 0
        CRTMP_Coeff_VISland  = 0

        Ratio_Cloud_W = 1.
        Ratio_Cloud_I = 1.
        Ratio_Cloud_R = 1.
        Ratio_Cloud_S = 1.
        Ratio_Cloud_G = 1.
        Ratio_Vapor   = 1.
        Ratio_O3      = 1.

       Return
       End Subroutine MC_Parameter_Default


       Subroutine MC_Parameter_Setting(SettingFile,error)
       USE Module_a_function,   only: A_FileExist, A_STR_2_Integer
        implicit none
        integer        :: error
        character(*)   :: SettingFile
        character(120) :: Pset, Info
        character(18)  :: Item
        integer :: idx1,idx2
        logical :: ex
        integer :: err


      ! ### Program Setting Reading:
 111    format(i5)
 112    format(a)
 113    format(f8.4)

      ! ### Setting File Exist Check:
        if(.not.A_FileExist(SettingFile))then
         error = -1
         return
        endif


      ! ### Loading CRTM Parameters:
        call MC_Parameter_Default
        open(21,file=SettingFile,status='old')
        do
         read(21,'(a)',iostat=err)Pset
         if(Pset(1:4).eq.'!end')exit
         if(err.ne.0)then
           Write(*,*)'Reading Error: ',trim(SettingFile)
           error = -2
           return
         endif

         idx1 = index(Pset,'=')
         idx2 = index(Pset,'!')
         if(idx1.le.0)cycle


         if(idx2.gt.0)Pset = Pset(1:idx2-1)
         Item = adjustl(trim(Pset(1:idx1-1)))
         Info = adjustl(trim(Pset(idx1+1:)))
         if(Info.eq.'')cycle
         if(Item(1:1).eq.'#' .or. Item(1:1).eq.'*')cycle
         if(Info(1:1).eq.'#' .or. Info(1:1).eq.'*')cycle


         select case(Item)
           
           case('NWP_Name')        ;read(Info,112)CRTMP_NWPname
           case('CRTM_Coeff_Dir')  ;read(Info,112)CRTMP_Coeff_Dir
           case('CRTM_Input_Dir')  ;read(Info,112)CRTMP_Input_Dir
           case('CRTM_Output_Dir') ;read(Info,112)CRTMP_Output_Dir
  
           case('CRTM_Sensor_ID')  ;read(Info,112)CRTMP_Sensor_ID
           case('CRTM_n_clouds')   ;read(Info,111)CRTMP_n_Clouds
           case('CRTM_n_absorbers');read(Info,111)CRTMP_n_Absorbers
           case('CRTM_n_aerosols') ;read(Info,111)CRTMP_n_Aerosols
           case('CRTM_n_output')   ;read(Info,111)CRTMP_n_Output
           case('CRTM_Force_ch')   ;read(Info,112)CRTMP_Force_ch
!          case('CRTM_Convert_Cnt');read(Info,112)CRTMP_Convert_Cnt

           case('AssignAngle')   ;read(Info,111)CRTMP_AssignAngle
           case('Sensor_Angle_S');read(Info,113)CRTMP_Sensor_Angle_S
           case('Sensor_Angle_Z');read(Info,113)CRTMP_Sensor_Angle_Z
           case('Source_Angle_Z');read(Info,113)CRTMP_Source_Angle_Z
           case('Source_Angle_A');read(Info,113)CRTMP_Source_Angle_A

           case('Absorber1')     ;read(Info,112)CRTMP_Absorber1
           case('Absorber2')     ;read(Info,112)CRTMP_Absorber2
           case('Absorber3')     ;read(Info,112)CRTMP_Absorber3
           case('Cloud1')        ;read(Info,112)CRTMP_Cloud1
           case('Cloud2')        ;read(Info,112)CRTMP_Cloud2
           case('Cloud3')        ;read(Info,112)CRTMP_Cloud3
           case('Cloud4')        ;read(Info,112)CRTMP_Cloud4
           case('Cloud5')        ;read(Info,112)CRTMP_Cloud5
           case('Cloud6')        ;read(Info,112)CRTMP_Cloud6
           case('CRTM_output1')  ;read(Info,112)CRTMP_output1
           case('CRTM_output2')  ;read(Info,112)CRTMP_output2
           case('CRTM_output3')  ;read(Info,112)CRTMP_output3
           case('CRTM_output4')  ;read(Info,112)CRTMP_output4
           case('CRTM_output5')  ;read(Info,112)CRTMP_output5

           case('File_output1')  ;read(Info,112)CRTMF_outputs(1)
           case('File_output2')  ;read(Info,112)CRTMF_outputs(2)
           case('File_output3')  ;read(Info,112)CRTMF_outputs(3)
           case('File_output4')  ;read(Info,112)CRTMF_outputs(4)
           case('File_output5')  ;read(Info,112)CRTMF_outputs(5)



           case('EffecR_Water')  ;read(Info,113)CRTMP_EffecR_Water
           case('EffecR_Ice')    ;read(Info,113)CRTMP_EffecR_Ice
           case('EffecR_Rain')   ;read(Info,113)CRTMP_EffecR_Rain
           case('EffecR_Snow')   ;read(Info,113)CRTMP_EffecR_Snow
           case('EffecR_Graupel');read(Info,113)CRTMP_EffecR_Graupel

           case('Coeff_IRland') ;read(Info,111)CRTMP_Coeff_IRland
           case('Coeff_IRwater');read(Info,111)CRTMP_Coeff_IRwater
           case('Coeff_MWwater');read(Info,111)CRTMP_Coeff_MWwater
           case('Coeff_VISland');read(Info,111)CRTMP_Coeff_VISland

           case('Ratio_Cloud_W');read(Info,113)Ratio_Cloud_W
           case('Ratio_Cloud_I');read(Info,113)Ratio_Cloud_I
           case('Ratio_Cloud_R');read(Info,113)Ratio_Cloud_R
           case('Ratio_Cloud_S');read(Info,113)Ratio_Cloud_S
           case('Ratio_Cloud_G');read(Info,113)Ratio_Cloud_G
           case('Ratio_Vapor')  ;read(Info,113)Ratio_Vapor
           case('Ratio_O3')     ;read(Info,113)Ratio_O3

           case('Debug_Main')     ;read(Info,111)CRTMP_Debug_Main
           case('Debug_CRTMP')    ;read(Info,111)CRTMP_Debug_CRTMP
           case('Debug_ModelLoad');read(Info,111)CRTMP_Debug_ModelLoad
           case('Debug_CRTM')     ;read(Info,111)CRTMP_Debug_CRTM

           case default
             write(*,*)'Unknow Item:',trim(Item),'  ',trim(Info)
         end select

        enddo
        close(21)
  
        if(CRTMP_Debug_CRTMP.eq.1)Debug_CRTMP = .true.

        if(Debug_CRTMP)call MC_Parameter_Show()
        if(Debug_CRTMP)write(*,*)'Reading Setting File Finish.'

        Error = 0
       Return
       End Subroutine MC_Parameter_Setting


       Subroutine MC_Parameter_Show
        implicit none
        write(*,*)'CRTM_n_clouds:    ',CRTMP_n_clouds
        write(*,*)'CRTM_n_absorbers: ',CRTMP_n_absorbers
        write(*,*)'CRTM_n_aerosols:  ',CRTMP_n_aerosols
        write(*,*)'CRTM_n_output:    ',CRTMP_n_output
        write(*,*)'CRTM_Sensor_ID:   ',CRTMP_Sensor_ID
        write(*,*)'CRTM_Coeff_Dir:   ',CRTMP_Coeff_Dir
        write(*,*)'CRTM_Input_Dir:   ',CRTMP_Input_Dir
        write(*,*)'CRTM_Output_Dir:  ',CRTMP_Output_Dir
       Return
       End Subroutine MC_Parameter_Show


       Function MC_Parameter_WaveLength(chidx)
        implicit none
        real    :: MC_Parameter_WaveLength
        integer :: chidx

        select case(CRTMP_Sensor_ID)
         case('v.ahi_himawari8')
           select case(chidx)
             case(1);MC_Parameter_WaveLength = 0.47063
             case(2);MC_Parameter_WaveLength = 0.51000
             case(3);MC_Parameter_WaveLength = 0.63914
             case(4);MC_Parameter_WaveLength = 0.85670
             case(5);MC_Parameter_WaveLength = 1.61010
             case(6);MC_Parameter_WaveLength = 2.25680
             case default
               write(*,*)'Unknow chidx with Sensor ',trim(CRTMP_Sensor_ID)
               MC_Parameter_WaveLength = -999.
           end select

         case default
           write(*,*)'Unknow CRTMP_Sensor_ID (Albedo Kind).'
           MC_Parameter_WaveLength = -999.
        end select

       Return
       End Function MC_Parameter_WaveLength


       Function MC_Parameter_AlbedoCeoeff(chidx)
        implicit none
        real    :: MC_Parameter_AlbedoCeoeff
        integer :: chidx

        select case(CRTMP_Sensor_ID)
         case('v.ahi_himawari8')
           select case(chidx)
             case(1);MC_Parameter_AlbedoCeoeff = 0.0015588
             case(2);MC_Parameter_AlbedoCeoeff = 0.0016612
             case(3);MC_Parameter_AlbedoCeoeff = 0.0019255
             case(4);MC_Parameter_AlbedoCeoeff = 0.0032325
             case(5);MC_Parameter_AlbedoCeoeff = 0.0129632
             case(6);MC_Parameter_AlbedoCeoeff = 0.0418125
             case default
               write(*,*)'Unknow chidx with Sensor ',trim(CRTMP_Sensor_ID)
               MC_Parameter_AlbedoCeoeff = -999.
           end select

         case default
           write(*,*)'Unknow CRTMP_Sensor_ID (Albedo Kind).'
           MC_Parameter_AlbedoCeoeff = -999.
        end select

       Return
       End Function MC_Parameter_AlbedoCeoeff



       Function MC_Parameter_ChName(chidx)
        implicit none
        character(5) :: MC_Parameter_ChName
        integer      :: chidx

        select case(CRTMP_Sensor_ID)
         case('ahi_himawari8')
           write(MC_Parameter_ChName,'(a,i2.2)')'B',chidx
         case('v.ahi_himawari8')
           write(MC_Parameter_ChName,'(a,i2.2)')'B',chidx
         case default
           write(*,*)'Unknow CRTMP_Sensor_ID'
           MC_Parameter_ChName = ''
        end select

       Return
       End Function MC_Parameter_ChName



      END MODULE Module_CRTM_Parameters
