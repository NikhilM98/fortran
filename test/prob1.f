c     Program or classroom problem
      x = 0.75
      summ = 1.0
      yi = 1.0
      i = 1.0
   10 yj = ((-1.0)**(i)*x**(2*i))/(2.0**(2*i)*fac(i)**2)
      summ = summ + yj
      write(*, *) i, yi, yj
      if(abs(yi - yj).gt.(10**(-5))) then
        yi = yj
        i = i + 1
        goto 10
      end if
      write(*, *) summ
      stop
      end

      function fac(i)
       fac = 1.0
       j = i
   12  if (j.gt.0) then
        fac = fac*j
        j = j - 1
        goto 12
       end if
       write(*, *) 'Fac', fac
       return
      end
