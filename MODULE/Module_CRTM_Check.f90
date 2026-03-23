! #############################################################################
!    Name: Module_CRTM_Check
!    Purpose: Check the input array for CRTM_Computer.
!
!  First Version Create by                     Akey Chen in CWB    2015/04/03
!  Modify by                                   Akey Chen in CWB    2017/09/04
! #############################################################################
      MODULE Module_CRTM_Check
       implicit none
       public  :: CRTM_Check_Cloud
       public  :: CRTM_Check_Pressure
       public  :: CRTM_Check_Vapor
       public  :: CRTM_Check_SFCtype
       public  :: CRTM_Check_Negative
       logical :: CRTM_Check_Quite

      CONTAINS

      !######################################################################
      !    Check and Set Value of Cloud Water Content:
      !########################################## akey in CWB 2013/06/04 ####
       Subroutine CRTM_Check_Cloud(Cloud,idx,Layer_Use)
       implicit none
       integer       :: n, idx, n_Layer
       real          :: Cloud(:)
       character(18) :: SPECK
       logical, optional :: Layer_Use(:)
      !######################################################################
       if(idx.eq.0)then
       write(*,'(a)')'################ User Guide #####################'
       write(*,'(a)')'call CRTM_Check_Cloud(Cloud_W,n_Layer,idx)  '
       write(*,'(a)')'#################################################'
       write(*,'(a)')' idx = 0 , User Guide'
       write(*,'(a)')' idx = 1 , Check and stop if error'
       write(*,'(a)')' idx = 2 , Correct the Error value to 0'
       write(*,'(a)')' idx = 3 , set Layer_Use as "F" if negitive value'
       write(*,'(a)')'#################################################'
       stop
       endif
       SPECK = 'CRTM_INPUT_CHECK:'
      !######################################################################
       n_Layer = size(Cloud)
       if(idx.eq.3 .and. .not.present(Layer_Use))then
         print*,'Check_Cloud: Please give Layer_Use for setting....'
         stop
       endif

       do n = 1 , n_Layer
         if(Cloud(n).lt.0.)then

           select case(idx)
             case(1)
              write(*,'(2a,i4,a)') &
               SPECK,' Cloud Water in Layer ',n,' is small then 0 ! '
              stop

             case(2) ; Cloud(n) = 0.
             case(3) ; Layer_Use(n) = .false.
             case default
              print*,SPECK,' Unknow index ',idx
              stop
           end select

         endif
       enddo

       Return
       End Subroutine CRTM_Check_Cloud




      !####################################################################
      !    Check Pressure Data Value:
      ! Modify by                           Akey Chen in CWB  2017/08/29 ##
      !####################################################################
       Integer Function CRTM_Check_Pressure(Pressure,Layer_Use,idx)
        implicit none
        integer      :: idx, l, num_Layer
        real         :: Pressure(:)
        logical      :: Layer_Use(:)
        logical      :: P_err

        if(idx.eq.0)then
        write(*,'(a)')'################ User Guide ####################'
        write(*,'(a)')' Function CRTM_Check_Pressure(P ,Layer_Use,idx) '
        write(*,'(a)')'################################################'
        write(*,'(a)')' idx = 0 , User Guide.'
        write(*,'(a)')' idx = 1 , Stop if any Error Layer Exist.'
        write(*,'(a)')' idx = 2 , To Ignore the Error Layer.'
        write(*,'(a)')'################################################'
        stop
        endif

        CRTM_Check_Pressure = 0
        P_err     = .false.
        num_Layer = size(Pressure)

       ! ## Pressure array check:
        do l = 2 , num_Layer
          if(Pressure(l).lt.Pressure(l-1))then
           write(*,'(2a,i4)')'CRTM_Check_Pressure: Error in LEVEL ',l
           P_err = .true.
           if(idx.eq.2)Layer_Use(l) = .false.
           CRTM_Check_Pressure = 1
          endif
        enddo

        if(P_err) then
          write(*,*)'### Checking the Value ! ###'
          write(*,*)' l    pressure'
          do l = 1 , num_Layer
            write(*,'(4x,f12.2)')Pressure(l)
          enddo
          write(*,*)'###########################'
          if(idx.eq.1)stop
        endif
       
       Return
       End Function CRTM_Check_Pressure


       Integer Function CRTM_Check_Negative(Value,idx,descript)
        implicit none
        integer :: idx, num_Value, l
        real    :: Value(:)
        logical :: V_err
        character(*) descript
  
        if(idx.eq.0)then
        write(*,'(a)')'################ User Guide ####################'
        write(*,'(a)')'   Function CRTM_Check_Negative(Value,idx) '
        write(*,'(a)')'################################################'
        write(*,'(a)')' idx = 0 , User Guide'
        write(*,'(a)')' idx = 1 , Stop if any Nagetive Data'
        write(*,'(a)')' idx = 2 , Correct the Nagetive to ZERO'
        write(*,'(a)')' idx = 3 , Keep the Negative Value'
        write(*,'(a)')'################################################'
        stop
        endif
 
        CRTM_Check_Negative = 0
        num_Value = size(Value)
        V_err = .false.
        do l = 1 , num_Value
          if(Value(l).lt.0)then
            V_err = .true.
            CRTM_Check_Negative = 1
            if(idx.eq.2)Value(l) = 0
          endif
        enddo

      
        if(V_err)then
          if(idx.eq.1)then 
            write(*,*)'### Checking the ',descript
            do l = 1 , num_Value
              write(*,'(i3,1x,f12.8)'),l,Value(l)
            enddo
            write(*,*)'###############################'
          endif 
        endif

   
      Return
      End Function CRTM_Check_Negative


      !######################################################################
      !    Check Water Vapor data value:
      !########################################## akey in CWB 2013/06/03 ####
       Subroutine CRTM_Check_Vapor(Vapor,n_Layer,idx)
       implicit none
       integer       :: n, idx, n_Layer
       real          :: Vapor(n_Layer)
       character(18) :: SPECK
      !######################################################################
       if(idx.eq.0)then
       write(*,'(a)')'################ User Guide #####################'
       write(*,'(a)')'   call CRTM_Check_Vapor(Vapor,n_Layer,idx)  '
       write(*,'(a)')'#################################################'
       write(*,'(a)')' idx = 0 , User Guide'
       write(*,'(a)')'*idx = 1 , Check and stop if error'
       write(*,'(a)')' idx = 2 , Check the value without stop program'
       write(*,'(a)')' idx = 3 , Correct the Error value to ZERO'
       write(*,'(a)')' idx = 4 , Correct just like idx=3 and be Quiet'
       write(*,'(a)')' '
       write(*,'(a)')' Defult is *'
       write(*,'(a)')'#################################################'
       stop
       endif
       SPECK = 'CRTM_INPUT_CHECK:'
      !######################################################################

       do n = 1 , n_Layer
         if(Vapor(n).lt.0.)then

           select case(idx)

             case(1)
               write(*,'(2a,i4,a)') &
                SPECK,' Water Vapor in Layer ',n,' is small then 0 ! '
               stop

             case(2)
               write(*,'(2a,i4,a)') &
                SPECK,' Water Vapor in Layer ',n,' is small then 0 ! '

             case(3)
               Vapor(n) = 0.
               write(*,'(2a,i4,a)') &
                 SPECK,' Water Vapor in Layer ',n,' is small then 0 ! '
               write(*,'(2a,f9.6)')' Correcting Water Vapor  ',Vapor(n)

             case(4)
               Vapor(n) = 0.

             case default
              print*,SPECK,' Unknow index ',idx
              stop

           end select

         endif
       enddo

       Return
       End Subroutine CRTM_Check_Vapor


      !######################################################################
      !    Check Surface Type:
      !########################################## akey in CWB 2013/08/23 ####
       Subroutine CRTM_Check_SFCtype(surface_type)
       real :: surface_type
       if(surface_type.ne.0 .and. surface_type.ne.1)then
          surface_type = 0.
       endif
       Return
       End Subroutine CRTM_Check_SFCtype

      END MODULE Module_CRTM_Check
