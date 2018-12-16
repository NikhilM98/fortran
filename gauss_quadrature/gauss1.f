c    Program for gauss quadrature method
      dimension c(100), y(100)
      open(unit=1, file='gauss1.in', status='unknown')
      open(unit=2, file='ans1.out', status='unknown')
      read(1,*) a, b, n
      z = 0
      do i=1,n
       read(1, *) c(i), y(i)
       z = z + c(i)*func(y(i), a, b)
      end do
      write(*,*) z
      write(2,*) z
      end

      function func(x, a, b)
       w = (b-a)/2.0*x + (b+a)/2
       func = (w**2 + 2*w + 1)*(b-a)/2.0
      return
      end

