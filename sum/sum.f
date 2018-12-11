c Program For 
      open(unit=1, file='sum.in', status='unknown')
      open(unit=2, file='sum.res', status='unknown')
      read(1, *) x
      write(2, *) x
      read(1, *) a, b, c
      write(2, *) a, b, c
      y = a + b*x + c*(x**2)
      write(2, *) y
      end
