! #############################################################################
!    Name: Module_CRTM_DataArray
!    Purpose: Save the data array that CRTM needed and help user to
!               put these data array into CRTM_Computer.
!             This Module will Save the necessary data for CRTM.
!
!
! Modify by                                    Akey Chen in CWB  2017/09/04
! #############################################################################
      MODULE Module_CRTM_DataArray

!      private

       logical :: CRTM_DataArray_Debug
       real,    allocatable :: CRTM_Result_Array(:,:,:,:)!(Out_Type,Channels,Xsize,Ysize)
       logical, allocatable :: CRTM_Result_Done(:,:)     !(Xsize,Ysize)
       logical :: Created_Result_Array
       integer,public :: CA_Xs,CA_Ys,CA_Zs
       data CA_Xs /0/
       data CA_Ys /0/
       data CA_Zs /0/

      ! ### Input Data Array:
       real, allocatable :: CRTM_ATM_PressureL(:,:,:)  !hPa   (n_Profile,n_Layer)
       real, allocatable :: CRTM_ATM_Pressure(:,:,:)   !hPa   (n_Profile,n_Layer)
       real, allocatable :: CRTM_ATM_Temperature(:,:,:)!K     (n_Profile,n_Layer)
       real, allocatable :: CRTM_ATM_Vapor(:,:,:)      !g/kg  (n_Profile,n_Layer)
       real, allocatable :: CRTM_ATM_O3(:,:,:)         !g/kg  (n_Profile,n_Layer)
       real, allocatable :: CRTM_Cloud_W(:,:,:)        !kg/m2 (n_Profile,n_Layer)
       real, allocatable :: CRTM_Cloud_I(:,:,:)        !kg/m2 (n_Profile,n_Layer)
       real, allocatable :: CRTM_Cloud_R(:,:,:)        !kg/m2 (n_Profile,n_Layer)
       real, allocatable :: CRTM_Cloud_S(:,:,:)        !kg/m2 (n_Profile,n_Layer)
       real, allocatable :: CRTM_Cloud_G(:,:,:)        !kg/m2 (n_Profile,n_Layer)
       real, allocatable :: CRTM_Aerosol_D(:,:,:)      !kg/m2 (n_Profile,n_Layer)

       real, allocatable :: CRTM_SFC_Temperature(:,:)  !K      (n_Profile)
       real, allocatable :: CRTM_SFC_Mask(:,:)         !NA     (n_Profile)
       real, allocatable :: CRTM_SFC_Wind_S(:,:)       !K      (n_Profile)
       real, allocatable :: CRTM_SFC_Wind_D(:,:)       !K      (n_Profile)
       real(8), allocatable :: CRTM_Sensor_Angle_S(:,:)!degree (n_Profile) Scan Angle of Sensor
       real(8), allocatable :: CRTM_Sensor_Angle_Z(:,:)!degree (n_Profile) Zenith Angle of Sensor
       real(8), allocatable :: CRTM_Source_Angle_Z(:,:)!degree (n_Profile) Zenith Angle of Source
       real(8), allocatable :: CRTM_Source_Angle_A(:,:)!degree (n_Profile) Azimuth Angle of Source
       logical, allocatable :: CRTM_Layer_Use(:,:,:)   !NA     (n_Profile,n_Layer)


       character(25) :: CRTM_UNIT_ATM_Vapor
       character(25) :: CRTM_UNIT_ATM_O3



       public :: CRTM_DataArray_Dimension
       public :: CRTM_DataArray
       public :: CRTM_DataUnit
       public :: CRTM_Result


       Interface CRTM_DataArray
         module procedure CRTM_DataArray_Single
         module procedure CRTM_DataArray_1D
         module procedure CRTM_DataArray_2D
         module procedure CRTM_DataArray_3D
       End Interface

      CONTAINS

       Function CRTM_DataArray_Dimension(Xsize,Ysize,Zsize)
        implicit none
        integer :: CRTM_DataArray_Dimension
        integer :: Xsize,Ysize,Zsize

        if(Xsize.gt.0 .and. Ysize.gt.0 .and. Zsize.gt.0)then
          CA_Xs = Xsize
          CA_Ys = Ysize
          CA_Zs = Zsize
          CRTM_DataArray_Dimension = 0
        else
          write(*,*)'Error Model Size:',Xsize,Ysize,Zsize
          CRTM_DataArray_Dimension = -1
        endif

        if(CRTM_DataArray_Debug) &
            write(*,*)'CRTM_DataArray_Dimension ',CA_Xs,CA_Ys,CA_Zs

       Return
       End Function CRTM_DataArray_Dimension



       Function CRTM_DataArray_3D(Data_Type,Data_Array)
        implicit none
        integer                :: CRTM_DataArray_3D
        character(*)           :: Data_Type
        real, dimension(:,:,:) :: Data_Array
        integer                :: Xsize, Ysize, Zsize

        CRTM_DataArray_3D = 0
        Xsize = size(Data_Array,1)
        Ysize = size(Data_Array,2)
        Zsize = size(Data_Array,3)
        if(CA_Xs.ne.Xsize)write(*,*)'Mismatch X size:',CA_Xs,Xsize
        if(CA_Ys.ne.Ysize)write(*,*)'Mismatch Y size:',CA_Ys,Ysize
        if(CA_Zs.ne.Zsize)write(*,*)'Mismatch Z size:',CA_Zs,Zsize


        select case (Data_Type)
          case('ATM_Pressure')
            allocate(CRTM_ATM_Pressure(CA_Xs,CA_Ys,CA_Zs))
            CRTM_ATM_Pressure = Data_Array
          case('ATM_PressureL')
            allocate(CRTM_ATM_PressureL(CA_Xs,CA_Ys,CA_Zs))
            CRTM_ATM_PressureL = Data_Array
          case('ATM_Temperature')
            allocate(CRTM_ATM_Temperature(CA_Xs,CA_Ys,CA_Zs))
            CRTM_ATM_Temperature = Data_Array
          case('ATM_Vapor')
            allocate(CRTM_ATM_Vapor(CA_Xs,CA_Ys,CA_Zs))
            CRTM_ATM_Vapor = Data_Array
          case('ATM_O3')
            allocate(CRTM_ATM_O3(CA_Xs,CA_Ys,CA_Zs))
            CRTM_ATM_O3 = Data_Array
          case('Cloud_W')
            allocate(CRTM_Cloud_W(CA_Xs,CA_Ys,CA_Zs))
            CRTM_Cloud_W = Data_Array
          case('Cloud_I')
            allocate(CRTM_Cloud_I(CA_Xs,CA_Ys,CA_Zs))
            CRTM_Cloud_I = Data_Array
          case('Cloud_R')
            allocate(CRTM_Cloud_R(CA_Xs,CA_Ys,CA_Zs))
            CRTM_Cloud_R = Data_Array
          case('Cloud_S')
            allocate(CRTM_Cloud_S(CA_Xs,CA_Ys,CA_Zs))
            CRTM_Cloud_S = Data_Array
          case('Cloud_G')
            allocate(CRTM_Cloud_G(CA_Xs,CA_Ys,CA_Zs))
            CRTM_Cloud_G = Data_Array
          case default
            write(*,*)'Unknow Data_Type:',Data_Type
            write(*,*)'Only for: ATM_Pressure'
            write(*,*)'          ATM_PressureL  '
            write(*,*)'          ATM_Temperature '
            write(*,*)'          ATM_Vapor       '
            write(*,*)'          ATM_O3          '
            write(*,*)'          ATM_Cloud_W     '
            write(*,*)'          ATM_Cloud_I     '
            write(*,*)'          ATM_Cloud_R     '
            write(*,*)'          ATM_Cloud_S     '
            write(*,*)'          ATM_Cloud_G     '
            CRTM_DataArray_3D = 1
        end select

       Return
       End Function CRTM_DataArray_3D



       Function CRTM_DataArray_2D(Data_Type,Data_Array)
        implicit none
        integer              :: CRTM_DataArray_2D
        character(*)         :: Data_Type
        real, dimension(:,:) :: Data_Array
        integer              :: Xsize, Ysize

        CRTM_DataArray_2D = 0
        Xsize = size(Data_Array,1)
        Ysize = size(Data_Array,2)
        if(CA_Xs.ne.Xsize)write(*,*)'Mismatch X size:',CA_Xs,Xsize
        if(CA_Ys.ne.Ysize)write(*,*)'Mismatch Y size:',CA_Ys,Ysize

        select case (Data_Type)
          case('SFC_Temperature')
            allocate(CRTM_SFC_Temperature(CA_Xs,CA_Ys))
            CRTM_SFC_Temperature = Data_Array
          case('SFC_Mask')
            allocate(CRTM_SFC_Mask(CA_Xs,CA_Ys))
            CRTM_SFC_Mask = Data_Array
          case('SFC_Wind_Speed')
            allocate(CRTM_SFC_Wind_S(CA_Xs,CA_Ys))
            CRTM_SFC_Wind_S = Data_Array
          case('SFC_Wind_Dir')
            allocate(CRTM_SFC_Wind_D(CA_Xs,CA_Ys))
            CRTM_SFC_Wind_D = Data_Array
          case('Sensor_Angle_S')
            allocate(CRTM_Sensor_Angle_S(CA_Xs,CA_Ys))
            CRTM_Sensor_Angle_S = Data_Array
          case('Sensor_Angle_Z')
            allocate(CRTM_Sensor_Angle_Z(CA_Xs,CA_Ys))
            CRTM_Sensor_Angle_Z = Data_Array
          case('Source_Angle_Z')
            allocate(CRTM_Source_Angle_Z(CA_Xs,CA_Ys))
            CRTM_Source_Angle_Z = Data_Array
          case('Source_Angle_A')
            allocate(CRTM_Source_Angle_A(CA_Xs,CA_Ys))
            CRTM_Source_Angle_A = Data_Array
          case default
            write(*,*)'Unknow Data_Type: ',Data_Type
            write(*,*)'Only for: SFC_Temperature'
            write(*,*)'          SFC_Mask       '
            write(*,*)'          SFC_Wind_Speed '
            write(*,*)'          SFC_Wind_Dir   '
            write(*,*)'          Angle_S        '
            write(*,*)'          Angle_S        '
            CRTM_DataArray_2D = 1
        end select

       Return
       End Function CRTM_DataArray_2D



       Function CRTM_DataArray_1D(Data_Type,Data_Array)
        implicit none
        integer            :: CRTM_DataArray_1D
        character(*)       :: Data_Type
        real, dimension(:) :: Data_Array
        integer            :: Zsize, z

        CRTM_DataArray_1D = 0
        Zsize = size(Data_Array,1)
        if(CA_Zs.ne.Zsize)write(*,*)'Mismatch Z size:',CA_Zs,Zsize

        select case (Data_Type)
          case('ATM_Pressure')
            allocate(CRTM_ATM_Pressure(CA_Xs,CA_Ys,CA_Zs))
            do z = 1 , Zsize
               CRTM_ATM_Pressure(:,:,z) = Data_Array(z)
            enddo
          case default
            write(*,*)'Unknow Data_Type: ',Data_Type
            write(*,*)'Only for: ATM_Pressure'
            CRTM_DataArray_1D = 1
        end select
       Return
       End Function CRTM_DataArray_1D



      Function CRTM_DataArray_Single(Data_Type,Data_Array)
       implicit none
       integer            :: CRTM_DataArray_Single
       character(*)       :: Data_Type
       real :: Data_Array

       CRTM_DataArray_Single = 0
       select case (Data_Type)
         case('Sensor_Angle_S')
           allocate(CRTM_Sensor_Angle_S(CA_Xs,CA_Ys))
           CRTM_Sensor_Angle_S = Data_Array
         case('Sensor_Angle_Z')
           allocate(CRTM_Sensor_Angle_Z(CA_Xs,CA_Ys))
           CRTM_Sensor_Angle_Z = Data_Array
         case('Source_Angle_Z')
           allocate(CRTM_Source_Angle_Z(CA_Xs,CA_Ys))
           CRTM_Source_Angle_Z = Data_Array
         case('Source_Angle_A')
           allocate(CRTM_Source_Angle_A(CA_Xs,CA_Ys))
           CRTM_Source_Angle_A = Data_Array
         case default
           write(*,*)'Unknow Data_Type: ',Data_Type
           write(*,*)'Only for: Sensor_Angle_S'
           write(*,*)'          Sensor_Angle_Z'
           write(*,*)'          Source_Angle_Z'
           write(*,*)'          Source_Angle_Z'
           CRTM_DataArray_Single = 1
       end select
      Return
      End Function CRTM_DataArray_Single



      Function CRTM_DataUnit(Data_Type,Data_Unit)
       implicit none
       integer            :: CRTM_DataUnit
       character(*)       :: Data_Type
       character(*)       :: Data_Unit

       CRTM_DataUnit = 0
       select case (Data_Type)
         case('ATM_Vapor') ; CRTM_UNIT_ATM_Vapor = Data_Unit
         case('ATM_O3')    ; CRTM_UNIT_ATM_O3    = Data_Unit
         case default
           write(*,*)'Unknow Data_Type: ',Data_Type
           write(*,*)'Only for: ATM_Vapor'
           write(*,*)'          ATM_O3'
           CRTM_DataUnit = 1
       end select
      Return
      End Function CRTM_DataUnit



      Subroutine CRTM_Result_ArrayCreate(N_CRTMout,N_Channel,Error)
       implicit none
       integer :: Error
       integer :: N_CRTMout, N_Channel
       Error = 0
       if(CA_Xs.le.0 .or. CA_Ys.le.0)Error = 1
       if(N_CRTMout.le.0)Error = 2
       if(N_Channel.le.0)Error = 3

       if(Error.ne.0)return
       allocate(CRTM_Result_Array(N_CRTMout,N_Channel,CA_Xs,CA_Ys))
       allocate(CRTM_Result_Done(CA_Xs,CA_Ys))
       CRTM_Result_Array = -999.
       CRTM_Result_Done  = .false.
      Return
      End Subroutine CRTM_Result_ArrayCreate


      Function CRTM_Result(Dx,Dy)
      Use Module_CRTM_Check
      Use Module_CRTM_Compute, only: CRTM_n_Output, &
                                     CRTM_n_Channels, &
                                     inCRTM_ATM_Temp, &
                                     inCRTM_ATM_Vapor, &
                                     inCRTM_ATM_O3, &
                                     inCRTM_ATM_Press, &
                                     inCRTM_Cloud_W, inCRTM_Cloud_W_set, &
                                     inCRTM_Cloud_I, inCRTM_Cloud_I_set, &
                                     inCRTM_Cloud_R, inCRTM_Cloud_R_set, &
                                     inCRTM_Cloud_S, inCRTM_Cloud_S_set, &
                                     inCRTM_Cloud_G, inCRTM_Cloud_G_set, &
                                     inCRTM_SFC_Temp, &
                                     inCRTM_SFC_Mask, &
                                     inCRTM_Sensor_Angle_S, &
                                     inCRTM_Sensor_Angle_Z, &
                                     inCRTM_Source_Angle_Z, &
                                     inCRTM_Source_Angle_A, &
                                     inCRTM_UNIT_ATM_Vapor, &
                                     inCRTM_UNIT_ATM_O3, &
                                     CRTM_Layer_Use, &
                                     CRTM_ComputeOut
       implicit none
       real    :: CRTM_Result_1(CRTM_n_Output,CRTM_n_Channels,1)
       real    :: CRTM_Result(CRTM_n_Output,CRTM_n_Channels)
       integer :: Dx, Dy
       integer :: pf, n_Layers, nout,nch
       integer :: Error
       integer :: l


       nout = CRTM_n_Output
       nch  = CRTM_n_Channels
       pf   = 1
       CRTM_Check_Quite = .true.

       if(.not.allocated(CRTM_Result_Array)) &
                            call CRTM_Result_ArrayCreate(nout,nch,Error)
       if(CRTM_Result_Done(Dx,Dy))then
         CRTM_Result = CRTM_Result_Array(:,:,Dx,Dy)
         return
       endif

       inCRTM_ATM_Press(pf,:)    = CRTM_ATM_Pressure(Dx,Dy,:)
       inCRTM_ATM_Temp(pf,:)     = CRTM_ATM_Temperature(Dx,Dy,:)
       inCRTM_ATM_Vapor(pf,:)    = CRTM_ATM_Vapor(Dx,Dy,:)
       inCRTM_ATM_O3(pf,:)       = CRTM_ATM_O3(Dx,Dy,:)
       inCRTM_SFC_Temp(pf)       = CRTM_SFC_Temperature(Dx,Dy)
       inCRTM_SFC_Mask(pf)       = CRTM_SFC_Mask(Dx,Dy)
       inCRTM_Sensor_Angle_S(pf) = CRTM_Sensor_Angle_S(Dx,Dy)
       inCRTM_Sensor_Angle_Z(pf) = CRTM_Sensor_Angle_Z(Dx,Dy)
       inCRTM_Source_Angle_Z(pf) = CRTM_Source_Angle_Z(Dx,Dy)
       inCRTM_Source_Angle_A(pf) = CRTM_Source_Angle_A(Dx,Dy)
  
       if(inCRTM_Cloud_W_set)inCRTM_Cloud_W(pf,:) = CRTM_Cloud_W(Dx,Dy,:)
       if(inCRTM_Cloud_I_set)inCRTM_Cloud_I(pf,:) = CRTM_Cloud_I(Dx,Dy,:)
       if(inCRTM_Cloud_R_set)inCRTM_Cloud_R(pf,:) = CRTM_Cloud_R(Dx,Dy,:)
       if(inCRTM_Cloud_S_set)inCRTM_Cloud_S(pf,:) = CRTM_Cloud_S(Dx,Dy,:)
       if(inCRTM_Cloud_G_set)inCRTM_Cloud_G(pf,:) = CRTM_Cloud_G(Dx,Dy,:)

       inCRTM_UNIT_ATM_Vapor = CRTM_UNIT_ATM_Vapor
       inCRTM_UNIT_ATM_O3    = CRTM_UNIT_ATM_O3

   
       CRTM_Layer_Use(pf,0) =  .true.
       if(inCRTM_SFC_Temp(pf).lt.0) CRTM_Layer_Use(pf,0) = .false.
       if(inCRTM_SFC_Mask(pf).lt.0) CRTM_Layer_Use(pf,0) = .false.
       if(.not.CRTM_Layer_Use(pf,0))then
         write(*,*)'Array x: ',Dx
         write(*,*)'Array y: ',Dy
         write(*,*)'CRTM_Layer_Use  :  ',CRTM_Layer_Use
         write(*,*)'SFC Type        :  ',CRTM_SFC_Mask(Dx,Dy)
         write(*,*)'SFC Temperature :  ',inCRTM_SFC_Temp(pf)
         write(*,*)'SFC Mask        :  ',inCRTM_SFC_Mask(pf)
         write(*,*)'ATM_Temperature :  ',inCRTM_ATM_Temp(pf,:)
       endif

       !~ Bad Data Value:
       n_Layers = size(inCRTM_ATM_Vapor,2)
       Error = CRTM_Check_Negative(inCRTM_ATM_Vapor(pf,:),2,'Vapor')
       Error = CRTM_Check_Negative(inCRTM_ATM_O3(pf,:),2,'O3')
       Error = CRTM_Check_Negative(inCRTM_ATM_Temp(pf,:),1,'Temperature')
       Error = CRTM_Check_Pressure(inCRTM_ATM_Press(pf,:), &
                                   CRTM_Layer_Use(1,1:n_Layers),2)
       CRTM_Result_1    = CRTM_ComputeOut(pf,Error)
       CRTM_Result(:,:) = CRTM_Result_1(:,:,1)


       !~ Write Result into Result_Array for mult-read:
!       print*,'Write Result into Result_Array for mult-read:'
       CRTM_Result_Array(:,:,Dx,Dy) = CRTM_Result(:,:)
       CRTM_Result_Done(Dx,Dy)      = .true.
!       print*,'CRTM_Result End'


      Return
      End Function CRTM_Result



      END MODULE Module_CRTM_DataArray

