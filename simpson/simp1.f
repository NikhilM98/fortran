c     Program for simpson's method
      function func(x)
       func = x**2 + 2*x + 1
      return
      end
      open(unit=1, file='simp1.in', status='unknown')
      open(unit=2, file='ans1.out', status='unknown')
      read(1,*) a, b, n
      h = (b-a)/float(n)
      tegral = func(b) + func(a)
      do i=1,n-1
       x = a + i*h
       if(MOD(i,2) .eq. 0) then
        tegral = tegral + 2*func(x)
       else if(MOD(i,2) .eq. 1) then
        tegral = tegral + 4*func(x)
       end if
      end do
      tegral = tegral*h/3
      write(*,*) tegral
      write(2,*) tegral
      end
