c     Program for numerov's method
      open(unit=1, file='data1.in', status='unknown')
      open(unit=2, file='res1.out', status='unknown')
      read(1, *) h, y0, y1
      write(2, *) h, y0, y1
      write(*, *) h, y0, y1

      a = ym(y0, y1, h)
      
      write(2, *) a
      write(*, *) a
      end

      function ym(yp, yn, h)
        ym = (yp + yn)*(1 - h**2/12.0)/(2+10/12.0*h**2)
      return
      end
