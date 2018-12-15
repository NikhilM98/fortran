c     Intersection method
      function f(x)
       f= (cos(x)+3.0)/2.0
       return       
      end
      x0 = 0.0
      esp=1.0e-5
10     x1=f(x0)
      if (abs(x1-x0).lt.esp) goto 20
      x0=x
      goto 10 
20     write(*,*)x, f(x)
      stop
      end
                   
                   
                   
                   
                   
