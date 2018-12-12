c Newton Raphson metohd
      function f(x)
       f=4.0*exp(-x)*sin(x)-1
      return
      end
      function g(x)
       g=4.0*exp(-x)*(cos(x)-sin(x))
      return
      end
      x0 = 0.0
      eps=1.0e-5
      do 10 i=1,100 
       x = x0 - f(x0)/g(x0)
       if (abs(x-x0).lt.eps) goto 20
       x0=x
       write(*,*) x ,f(x)
10    continue 
20    write(*,*)x
      stop
      end