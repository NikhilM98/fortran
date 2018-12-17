c     Program for numerov's method
      open(unit=1, file='data2.in', status='unknown')
      open(unit=2, file='res2.out', status='unknown')
      read(1, *) xi, h, y0, y1
      write(2, *) xi, h, y0, y1
      write(*, *) xi, h, y0, y1

      a = ym(y0, y1, xi, h)
      
      write(2, *) a
      write(*, *) a
      end

      function ym(yp, yn, x, h)
        xp = x
        xm = x+h
        xn = x+2*h
        ym=(-h**2/12*(f(xp, yp)+f(xn,yn)+10*xm**2)+yp+yn)/(2-10*h**2/12)
      return
      end

      function f(x,y)
       f = x**2 - y
      return
      end

