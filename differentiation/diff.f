c     Method of differentiation
      dimension f(100), df(100)
      open(unit=1, file='rich1.in', status='unknown')
      open(unit=2, file='rich1.out', status='unknown')
      read(1,*) n, h
      do i = 1, n
       read(1,*) f(n)
      end do
      df(1) = (-3.*f(1) + 4.*f(2) - f(3))/(2*h)
      do j = 1, n-1
       df(j) = (-f(j-1) + f(j+1))/(2.*h)
      end do
      df(n) = (f(n-2) - 4.*(f(n-1) + 3.*f(n)))/(2*h)
      do k = 1, n
       write(*, *) df(k)
       write(2, *) f(k), df(k)
      end do
      end
