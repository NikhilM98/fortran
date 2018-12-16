c     Runge Kutta Second Order
      open(unit=1, file='data.in', status='unknown')
      open(unit=2, file='ans.out', status='unknown')
      read(1, *) h, xlast
      write(2, *) h, xlast
      x = 1.0
      y = 2.0
      y1 = 1.75

10    del1 = h*f(x, y, y1)
      del2 = h*f(x + h/2.0, (y + h/2.0)*y1 + h/8.0*del1, y1 + del1/2.0)
      del3 = h*f(x + h/2.0, (y + h/2.0)*y1 + h/8.0*del1, y1 + del2/2.0)
      del4 = h*f(x + h, y + h*y1 + h/2.0*del3, y1 + del3)

      y = y + h*( y1 + (del1 + del2 + del3)/6.0 )
      y1 = y1 + (del1 + 2.0*del2 + 2.0*del3 + del4)/6.0
      x = x + h
      write(*, *) x, y, y1, del1, del2, del3, del4
      write(2, *) y
      if (x.ne.xlast) go to 10
      stop
      end
      function f(x, y, y1)
       f = (1.0 - y1**2.0)/y
      return
      end
