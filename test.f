      Program test

      integer :: d1,d2,d3,d4
      real    :: array(4,5,6,7)



      do d1 = 1 , 4
      do d2 = 1 , 5
      do d3 = 1 , 6
      do d4 = 1 , 7

        array(d1,d2,d3,d4) = d1*1000+d2*100+d3*10+d4
      enddo
      enddo
      enddo
      enddo


      open(31,file='test.dat',form='unformatted',access='stream')
      write(31)array
      close(31)

      stop
      end
