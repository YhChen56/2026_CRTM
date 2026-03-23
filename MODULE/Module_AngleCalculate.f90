!#########################################################################
! Name: Module_AngleCalculate.f90
! Purpose :     Calculate the Angle of Solar and Geostation Satellite.

!  Modify for SolarAngle used Month Day    Akey Chen in CWB   2017/05/02
!#########################################################################


      MODULE Module_AngleCalculate

       private

       Interface Mday2Sday
         module procedure Mday2Sday_int
         module procedure Mday2Sday_flt
       End Interface

       public :: GeoSat_Angle
       public :: sunae
       public :: SolarAngle
       public :: SolarAngle_JD
       public :: Sday2Mday
       public :: Mday2Sday
      CONTAINS

       Function GeoSat_Angle(Surf_Lon,Surf_Lat,Sat_Lon)
        implicit none
        real,intent(in) :: Surf_Lon
        real,intent(in) :: Surf_Lat
        real,intent(in) :: Sat_Lon
        real :: rE
        real :: rA
        real :: S,rS
        real :: N,rN
        real :: L,rL
        real :: G,rG
        real :: rE1, rE2
        real :: rA1, rA2
        real :: PI
        real :: GeoSat_Angle(2)


        GeoSat_Angle = -999.
        if(Surf_Lat.gt.90.  .or.Surf_Lat.lt. -90.)return
        if(Surf_Lon.gt.180. .or.Surf_Lon.lt.-180.)return
        if(Sat_Lon .gt.180. .or.Sat_Lon .lt.-180.)return

        PI = acos(-1.)
        L = Surf_Lat
        N = Surf_Lon
        S = Sat_Lon
        G = S - N

        rL = L*PI/180.
        rN = N*PI/180.
        rS = S*PI/180.
        rG = rN - rS

        rE1 = cos(rG)*cos(rL)-.1512
        rE2 = (1-cos(rG)**2*cos(rL)**2)**.5
        rE  = atan(rE1/rE2)
        rA1 = tan(rG)
        rA2 = tan(rL)
        rA  = atan(rA1/rA2) + PI

        GeoSat_Angle(1) = rA*180/PI
        GeoSat_Angle(2) = rE*180/PI
        if(L.lt.0)GeoSat_Angle(1) = GeoSat_Angle(1) + 180.
        if(GeoSat_Angle(1).ge.360.)GeoSat_Angle(1) = GeoSat_Angle(1)-360

       Return
       End Function GeoSat_Angle

       Function SolarAngle(year, month, day, hour, lat, lon)
        implicit none
        Real, intent(in) :: year,month,day,hour,lat,lon
        Real :: Jday
        Real :: az,el,ha,dec,soldst
        real :: SolarAngle(5)

        Jday = Mday2Sday_flt(year, month, day)
        call sunae(year,Jday,hour,lat,lon,az,el,ha,dec,soldst)
        SolarAngle(1) = az
        SolarAngle(2) = el
        SolarAngle(3) = ha
        SolarAngle(4) = dec
        SolarAngle(5) = soldst
       Return
       End Function SolarAngle

       Function SolarAngle_JD(year, day, hour, lat, lon)
        implicit none
        Real, intent(in) :: year,day,hour,lat,lon
        Real :: az,el,ha,dec,soldst
        real :: SolarAngle_JD(5)
        call sunae(year,day,hour,lat,lon,az,el,ha,dec,soldst)
        SolarAngle_JD(1) = az
        SolarAngle_JD(2) = el
        SolarAngle_JD(3) = ha
        SolarAngle_JD(4) = dec
        SolarAngle_JD(5) = soldst
       Return
       End Function SolarAngle_JD



      !-------------------------------------------------------------------------------+
      !  This subroutine calculates the local azimuth and elevation of the sun at               |
      !  a specific location and time using an approximation to equations used                  |
      !  to generate tables in The Astronomical Almanac.                                        |
      !  Refraction correction is added so sun position is apparent one.                        |
      !                                                                                         |
      !The Astronomical Almanac, U.S. Gov't Printing Office, Washington,D.C. (1985).            |
      !                                                                                         |
      !    input parameters                                                                     |
      !      year=year, e.g., 1986                                                              |
      !      day=day of year, e.g., feb 1=32                                                    |
      !      hour=hours plus fraction in UT, e.g., 8:30 am eastern daylight                     |
      !        time is equal to 8.5 + 5(5 hours west of Greenwich) -1(for                       |
      !        daylight savings time correction)                                                |
      !      lat=latitude in degrees (north is positive)                                        |
      !      long=longitude in degrees (east is positive)                                       |
      !                                                                                         |
      !    output parameters                                                                    |
      ! az= sun azimuth angle (measured east from north, 0 to 360 degs)                         |
      ! el= sun elevation angle (degs)                                                          |
      ! ha= solar hour angle                                                                    |
      ! dec = declination                                                                       |
      ! soldst = solar distance                                                                 |       
      !                                                                                         |
      ! Modify for NAN output around sun elevation angle near 90                                |
      ! 
      !-------------------------------------------------------------------------------+
      ! Copyright (C) 2011, Juan Pablo Justiniano  <jpjustiniano@gmail.com>
       Subroutine sunae(year,day,hour, lat, long,az,el,ha,dec,soldst)  ! Sun's position, Michalsky
       implicit none
       Real, intent(in) :: year
       Real, intent(in) :: day
       Real, intent(in) :: hour
       Real, intent(in) :: lat
       Real, intent(in) :: long
       Real, intent(out) :: az
       Real, intent(out) :: el
       Real, intent(out) :: ha
       Real, intent(out) :: dec
       Real, intent(out) :: soldst

       real :: twopi, pi, rad
       real :: delta, leap, jd, time
       real :: mnlong, mnanom, eclong, oblqec, num, den, ra
       real :: gmst, lmst, latrad, elc, refrac, soldia

       data twopi,pi,rad/6.2831853,3.1415927,.017453293/
       real :: az_sin
       real :: cosAzPos, sinAzNeg

      !   get the current julian date (actually add 2,400,000 for jd)
            delta=year-1949.
            leap=aint(delta/4.)
            jd=32916.5+delta*365.+leap+day+hour/24.
      !   1st no. is mid. 0 jan 1949 minus 2.4e6; leap=leap days since 1949
      !  the last yr of century is not leap yr unless divisible by 400
            if (amod(year,100.).eq.0.0.and.amod(year,400.).ne.0.0) &
                jd=jd-1.

      !   calculate ecliptic coordinates
            time=jd-51545.0
      !   51545.0 + 2.4e6 = noon 1 jan 2000

      !   force mean longitude between 0 and 360 degs
            mnlong=280.460+.9856474*time
            mnlong=mod(mnlong,360.)
            if(mnlong.lt.0.)mnlong=mnlong+360.

      !   mean anomaly in radians between 0 and 2*pi
            mnanom=357.528+.9856003*time
            mnanom=mod(mnanom,360.)
            if(mnanom.lt.0.)mnanom=mnanom+360.
            mnanom=mnanom*rad

      !   compute the ecliptic longitude and obliquity of ecliptic in radians
            eclong=mnlong+1.915*sin(mnanom)+.020*sin(2.*mnanom)
            eclong=mod(eclong,360.)
            if (eclong.lt.0.) eclong=eclong+360.
            oblqec=23.439-.0000004*time
            eclong=eclong*rad
            oblqec=oblqec*rad

      !   calculate right ascension and declination
            num=cos(oblqec)*sin(eclong)
            den=cos(eclong)
            ra=atan(num/den)
      !   force ra between 0 and 2*pi
            if (den.lt.0) then
                ra=ra+pi
            elseif (num.lt.0) then
                ra=ra+twopi
            endif

      !   dec in radians
            dec=asin(sin(oblqec)*sin(eclong))

      !   calculate Greenwich mean sidereal time in hours
            gmst=6.697375+.0657098242*time+hour

      !   hour not changed to sidereal time since 'time' includes the fractional day 
            gmst = mod(gmst,24.)
            if(gmst.lt.0.) gmst=gmst+24.

      !   calculate local mean sidereal time in radians 
            lmst=gmst+long/15.
            lmst=mod(lmst,24.)
            if(lmst.lt.0.) lmst=lmst+24.
            lmst=lmst*15.*rad

      !   calculate hour angle in radians between -pi and pi
            ha=lmst-ra

            if(ha.lt.-pi) ha=ha+twopi
            if(ha.gt.pi) ha=ha-twopi
      !   change latitude to radians
            latrad=lat*rad


      !   calculate azimuth and elevation
            el=asin(sin(dec)*sin(latrad)+cos(dec)*cos(latrad)*cos(ha))
            az_sin = -cos(dec)*sin(ha)/cos(el)
            if(az_sin.lt.-1)az_sin = -1.    !! akey add 20160130
            if(az_sin.gt. 1)az_sin =  1.
            az=asin(az_sin)
!            az=asin(-cos(dec)*sin(ha)/cos(el))
!            write(*,'(a,7f12.5)')'SunAngle 0 :',az,az_sin,dec,ha,el

      !!   this puts azimuth between 0 and 2*pi radians
            if (sin(dec)-sin(el)*sin(latrad).ge.0.) then
              if(sin(az).lt.0.) az=az+twopi
            else
              az=pi-az
            endif

!            write(*,'(a,7f12.5)')'SunAngle 1 :',az,az_sin
      !   if az=90 degs, elcritical=asin(sin(dec)/sin(latrad))
!          elc=asin(sin(dec)/sin(latrad))
!          if(el.ge.elc)az=pi-az
!          if(el.le.elc.and.ha.gt.0.)az=twopi+az

!            write(*,'(a,7f12.5)')'SunAngle 2 :',az,az_sin

      !   calculate refraction correction for US stan. atmosphere
      !   need to have el in degs before calculating correction
            el=el/rad

            if(el.ge.19.225) then
               refrac=.00452*3.51823/tan(el*rad)
            else if (el.gt.-.766.and.el.lt.19.225) then
               refrac=3.51823*(.1594+.0196*el+.00002*el**2)/ &
                              (1.+.505*el+.0845*el**2)
            else if (el.le.-.766) then
               refrac=0.0
            end if


      !   note that 3.51823=1013.25 mb/288 C
            el=el+refrac
      !   elevation in degs
      !   calculate distance to sun in A.U. & diameter in degs
            soldst=1.00014-.01671*cos(mnanom)-.00014*cos(2.*mnanom)
            soldia=.5332/soldst

          do while(az.gt.twopi)   !! akey add 20160130
            az = az - twopi
          enddo
          do while(az.lt.0)
            az = az + twopi
          enddo
!         write(*,'(a,9f12.5)')'SunAngle 3 :',lat,az/rad,az,az_sin,el,elc
!     &          ,sin(dec)/sin(latrad)


      !   convert az and lat to degs before returning
          az=az/rad
          ha=ha/rad
          dec=dec/rad





      !   mnlong in degs, gmst in hours, jd in days if 2.4e6 added;
      !   mnanom,eclong,oblqec,ra,and lmst in radians
       End subroutine sunae



      !############################################################


        Function Mday2Sday_int(Year,MM,DD)
         implicit none
         integer :: Year,MM,DD,Mday2Sday_int
         integer :: i,md(12),sumday
         data md /31,28,31,30,31,30,31,31,30,31,30,31/
         call Fed_day(Year,md(2))
      !  Error SDay Check:
         if(MM.gt.12 .or. MM.lt.1)then
           print*,'Error Month:',MM
           stop
         elseif(DD.gt.md(MM) .or. DD.lt.1)then
           print*,'Input Month: ',MM
           print*,'Error Days:',DD
           stop
         endif

         Mday2Sday_int = DD
         do i = 1 , MM-1
            Mday2Sday_int = Mday2Sday_int + md(i)
         enddo
        Return
        End Function Mday2Sday_int


        Function Mday2Sday_flt(Year,MM,DD)
         implicit none
         real :: Year,MM,DD,Mday2Sday_flt
         integer :: iYear, iMM
         integer :: i,md(12),sumday
         data md /31,28,31,30,31,30,31,31,30,31,30,31/
         iYear = Year
         iMM   = MM
         call Fed_day(iYear,md(2))
      !  Error SDay Check:
         if(iMM.gt.12 .or. iMM.lt.1)then
           print*,'Error Month:',iMM
           stop
         elseif(DD.gt.md(iMM) .or. DD.lt.1)then
           print*,'Input Month: ',iMM
           print*,'Error Days:',DD
           stop
         endif

         Mday2Sday_flt = DD
         do i = 1 , iMM-1
            Mday2Sday_flt = Mday2Sday_flt + md(i)
         enddo
        Return
        End Function Mday2Sday_flt




        subroutine Sday2Mday(Year,SDay,MM,DD)
         implicit none
         integer Year,SDay,MM,DD
         integer i,md(12),sumday
         data md /31,28,31,30,31,30,31,31,30,31,30,31/

         call Fed_day(Year,md(2))

      !  Error SDay Check:
         sumday=0
         do i = 1 , 12
           sumday = sumday + md(i)
         enddo

         if(SDay.gt.sumday .or. SDay .le. 0)then
           print*,"Fed_day: Error input in SDay = ",SDay
           stop
         endif

      !  Compute Month and day:
         MM=1
         DD=0
         do i = 1 , 12
           if(SDay.gt.md(i))then
             MM = MM + 1
             SDay = SDay - md(i)
           else
             DD = SDay
             return
           endif
         enddo

         return
         end subroutine Sday2Mday


      !C============================================================
         subroutine Fed_day(year,days)
          implicit none
          integer year,days

          days = 28   !normal
          if(mod(year,4).eq.0)days=29
          if(mod(year,100).eq.0)days=28
          if(mod(year,400).eq.0)days=29
          if(mod(year,4000).eq.0)days=28

         return
         end subroutine Fed_day
      !C============================================================


      End MODULE Module_AngleCalculate

