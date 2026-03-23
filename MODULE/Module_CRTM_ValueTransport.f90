      MODULE Module_CRTM_ValueTransport
       implicit none

      ! Geophysical Parameter:
       real, parameter, private :: grav = 9.80665 ! m/s^2

       public :: Reverse_Float
       public :: Level_Pressure_get
       public :: Level_Pressure_get_8
       public :: WContent2LWPath
       public :: WC2LWP_by_LayerP
       public :: MR2LWP_by_LayerP
       public :: SHumidity2LWPath
       public :: MRatio2LWPath

      CONTAINS

        subroutine Reverse_Float(n_Layers,Array)
         implicit none
         integer :: nn, nn_rev,n_Layers
         real :: Array(n_Layers)
         real :: rev_array(n_Layers)

         do nn = 1 , n_Layers
           nn_rev = n_Layers - nn + 1
           rev_array(nn) = Array(nn_rev)
         enddo
         Array = rev_array
        return
        end subroutine Reverse_Float

        subroutine Level_Pressure_get(n_Layers,P,Level_P)
         implicit none
         integer :: nn, n_Layers
         real    :: P(n_Layers), Level_P(0:n_Layers)
         do nn = 1 , n_Layers-1
           Level_P(nn) = (P(nn) + P(nn+1)) / 2
         enddo
         Level_P(0)        = P(1)        - (Level_P(1)         -P(1))
         Level_P(n_Layers) = P(n_Layers) - &
                             (Level_P(n_Layers-1)-P(n_Layers))
        return
        end subroutine Level_Pressure_get

        subroutine Level_Pressure_get_8(n_Layers,P,Level_P)
         implicit none
         integer :: nn, Ls, n_Layers
         real(8) :: P(n_Layers), Level_P(0:n_Layers)

         Ls = n_Layers
         do nn = 1 , Ls-1
           Level_P(nn) = (P(nn) + P(nn+1)) / 2
         enddo
         Level_P(0)  = P(1) -(Level_P(1)   -P(1))
         Level_P(Ls) = P(Ls)-(Level_P(Ls-1)-P(Ls))
         if(Level_P(0).lt.0)Level_P(0) = P(1) / 2.
        return
        end subroutine Level_Pressure_get_8



      ! Water Content to Liquit Water Path: (g/m3 > kg/m2)
        subroutine WContent2LWPath(n_Layers,Level_H,WC)
         implicit none
         integer :: nn,n_Layers
         integer :: Level_H(0:n_Layers)
         real :: WC(n_Layers)  ! Water Content (Input data: g/m3)

         do nn = 1 , n_Layers
           if(WC(nn).gt.0)then
             WC(nn) = WC(nn) * abs(Level_H(nn) - Level_H(nn-1)) / 1000

           endif
         enddo

        return
        end subroutine WContent2LWPath


      ! ##### Water Contant to Liquid Water Path:
        subroutine WC2LWP_by_LayerP(Layer_P,WC)
         implicit none
         integer :: l,n_Layers
         real    :: layer_P(:)
         real    :: WC(:)
         real,allocatable :: level_P(:)

         n_Layers = size(Layer_P)
         allocate(level_P(0:n_Layers))

         Level_P(0) = Layer_P(1) / 2
         do l = 1 , n_Layers-1
           Level_P(l) = (Layer_P(l)+Layer_P(l+1)) / 2
         enddo
         Level_P(n_Layers) = Level_P(n_Layers-1) + &
                             (Layer_P(n_Layers)-Layer_P(n_Layers-1))


         do l = 1 , n_Layers
           WC(l) = WC(l) * (Level_P(l) - Level_P(l-1)) / grav
         enddo

        return
        end subroutine WC2LWP_by_LayerP


      ! Mixing Ratio to Liquit Water Path by LayerP: (g/kg > kg/m2)
        Subroutine MR2LWP_by_LayerP(Layer_P,MR)
         implicit none
         integer :: l, n_Layers
         real    :: layer_P(:)
         real    :: MR(:)
         real    :: MR2LWP
         real,allocatable :: level_P(:)

         n_Layers = size(layer_P)
         allocate(level_P(0:n_Layers))

         Level_P(0) = Layer_P(1) / 2
         do l = 1 , n_Layers-1
           Level_P(l) = (Layer_P(l)+Layer_P(l+1)) / 2
         enddo
         Level_P(n_Layers) = Level_P(n_Layers-1) + &
                             (Layer_P(n_Layers)-Layer_P(n_Layers-1))

         do l = 1 , n_Layers
           if(MR(l).gt.0)then
             MR2LWP = (Level_P(l) - Level_P(l-1)) * 100 / grav
             MR(l)  = MR(l) * MR2LWP / 1000.
           endif
         enddo

        Return
        End Subroutine MR2LWP_by_LayerP



      ! Specific Humidity to Liquit Water Path:
        subroutine SHumidity2LWPath(n_Layers,P,Level_P,SH)
         implicit none
         integer :: nn,n_Layers
         real :: P(n_Layers), Level_P(0:n_Layers)
         real :: SH(n_Layers)  ! Specific Humidity (Input data)
         real :: MR            ! Mixing Ratio
         real :: MR2LWP

         do nn = 1 , n_Layers
           if(SH(nn).gt.0)then
             MR2LWP = (Level_P(nn) - Level_P(nn-1)) * 100 / grav
             MR     = SH(nn) / (1+SH(nn))
             SH(nn) = MR * MR2LWP
           endif
         enddo
        return
        end subroutine SHumidity2LWPath


      ! Mixing Ratio to Liquit Water Path: (g/kg > kg/m2)
        subroutine MRatio2LWPath(n_Layers,P,Level_P,MR)
         implicit none
         integer :: nn,n_Layers
         real :: P(n_Layers), Level_P(0:n_Layers)
         real :: MR(n_Layers) ! Mixing Ratio
         real :: MR2LWP

         do nn = 1 , n_Layers
           if(MR(nn).gt.0)then
             MR2LWP = (Level_P(nn) - Level_P(nn-1)) * 100 / grav
             MR(nn) = MR(nn) * MR2LWP / 1000.
           endif
         enddo
        return
        end subroutine MRatio2LWPath


      END MODULE Module_CRTM_ValueTransport
