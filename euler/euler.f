c     Code for Euler's Method
      function df(x,y)
       df = x + y + x*y
      return
      end
      open(unit=1, file='euler1.in', status='unknown')
      open(unit=2, file='ans1.out', status='unknown')
      read(1,*) x, y, h, n
      dfx = df(x, y)
      write(2, *) x, dfx, y
      write(*, *) x, dfx, y
      xp = x
      yp = y
      do i = 1, n
       xp = xp + h
       yp = yp + h*dfx
       dfx = df(xp, yp)
       write(2, *) xp, dfx, yp
       write(*, *) xp, dfx, yp
      end do
      end
