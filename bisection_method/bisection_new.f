c     Bisection Method
      open(unit=1, file='bisection.csv', status='unknown')
      eps=10**(-4)
      xl=0.0
      xu=0.50
10    xm=(xl+xu)/2.0
      if(f(xl)*f(xm)) 5,6,6
5      xl=xl
       xu=xm
       go to 20
6      xl=xm
       xu=xu
       go to 20
20    if (abs(f(xm)).gt.eps) then
       write(1,*) xm, ',', f(xm)
       go to 10
      end if
      end
      function f(x)
      f = 4.0*exp(-x)*sin(x) - 1.0
      return
      end
