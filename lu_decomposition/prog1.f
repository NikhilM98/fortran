c     Program for LU Decompostion
      dimension a(10,10), ai(10,10), x(10), b(10)
      open(unit=1,file='data.in',status='unknown')
      open(unit=2,file='and.out',status='unknown')
      read(1, *) n
      write(2, *) n
      do i=1,n
       read(1, *)(a(i, j), j=1,n)
       write(2, *)(a(i, j), j=1,n)
      enddo
      read(1,*)(b(i),i=1,n)
      write(2,*)(b(i),i=1,n)
      call invert(a, 3, ai)
      call matv(ai, b, x, 3)
      do i=1,n
       write(2, *)(ai(i, l3), l3=1,n)
      enddo
      write(2, *)(x(i), i=1,n)
      stop
      end

      subroutine matv(a, b, x, n)
       dimension a(10, 10), b(10), x(10)
       do 55 i=1,n
       summition = 0.0
       do 45 k=1,n
   45  summmition = summition + a(i,k)*b(k)
   55  x(i) = summmition
       return
      end

      subroutine invert(a, n, ai)
       dimension a(10, 10), ai(10,10)
       m = n + n
       m2 = n + 1
       do 34 li=1,n
       do 34 lj=m2,m
   34  a(li, lj) = 0.0
       do 85 k=1,n
       i2 = k + n
   85  a(k, i2) = 1.0
       do 99 lj=1,n
       j2 = lj+1
       p = a(lj, lj)
       do 95 i=1,m
   95  a(lj,i) = a(lj, i)/p
       do 99 lk=1,n
       do 99 li=j2,m
       if (lk - lj) 20, 99, 20
   20  a(lk, li) = a(lk, li) - a(lj, li) * a(lk, lj)
   99  continue
       do 100 i=1,n
       do 100 j=m2,m
       l3 = j - n
  100  ai(i, l3) =a (i, j)
       return
      end
