c     Least Square Method
      dimension  x(100), y(100)
      open(unit=1, file='prob1.in', status='unknown')
      open(unit=2, file='prob1.out', status='unknown')
      read(1,*) n
      do i=1,n
       read(1, *) x(i), y(i)
      end do
      smXiYi = 0.0
      smXi = 0.0
      smYi = 0.0
      smXi2 = 0.0
      do i=1,n
       smXiYi = smXiYi +  x(i)*y(i)
       smXi = smXi + x(i)
       smYi = smYi + y(i)
       smXi2 = smXi2 + x(i)**2
      end do
      a1 = ( n*smXiYi - smXi*smYi )/( n*smXi2 - smXi**2)
      a0 = (smYi - a1*smXi)/float(n)

      write(*,*) 'Enter the value of x and press Enter'
      read(*,*) z
      w = a0 + a1*z
      write(*,*) z, w
      write(2,*) z, w
      end
