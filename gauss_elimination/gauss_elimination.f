c     Program for Gauss Elimination
      dimension a(50, 50), x(50), y(50)
      integer s
      open(unit=1, file='gauss1.in', status='unknown')
      open(unit=2, file='ans1.out', status='unknown')
      read(1, *) n
      read(1, *)((a(i, j), j=1,n), i=1,n)
      read(1, *)(y(i), i=1,n)
      write(*, *) a(1, 1), a(1, 2), a(1, 3), y(1)
      write(*, *) a(2, 1), a(2, 2), a(2, 3), y(2)
      write(*, *) a(3, 1), a(3, 2), a(3, 3), y(3)
      m = n - 1
      do 10 i=1,m
      s = i + 1
      do 10 j=s,n
      if(a(j,i)) 6, 10, 6
   6  do 8 k=s,n
   8  a(j, k) = a(j, k) - (a(i, k) * a(j, i))/a(i, i)
      y(j) = y(j) - (y(i)*a(j, i))/a(i, i)
   10 continue
      x(n) = y(n)/a(n, n)
      write(2, *) n, x(n)
      do 30 i=1,m
      k = n - i
      s = k + 1
      do 20 j=s,n
   20 y(k) = y(k) - x(j)*a(k, j)
      x(k) = y(k)/a(k, k)
   30 write(2, *) k, x(k)
      stop
      end
