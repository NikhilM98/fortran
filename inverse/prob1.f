c     Program to take inverse of a matrix
      dimension a(10, 10), ai(10, 10), b(10), x(10)
      open(unit=1,file='data1.in',status='unknown')
      open(unit=2,file='ans1.out',status='unknown')
      read(1, *) n
      write(2, *) n
      do i=1,n
       read(1, *)(a(i, j), j=1,n)
       write(2, *)(a(i, j), j=1,n)
      enddo
      call invert(a, n, ai)
      do i=1,n
       write(*, *) (ai(i, j), j=1,n)
       write(2, *) (ai(i, j), j=1,n)
      end do
      stop
      end

      subroutine invert(a, n, ai)
       dimension a(10, 10), ai(10,10)
       m = n + n
       m2 = n + 1
       do 10 li=1,n
       do 10 lj=m2,m
   10  a(li, lj) = 0.0
       do 11 k=1,n
       i2 = k + n
   11  a(k, i2) = 1.0
       do 12 lj=1,n
       j2 = lj+1
       p = a(lj, lj)
       do 13 i=1,m
   13  a(lj,i) = a(lj, i)/p
       do 12 lk=1,n
       do 12 li=j2,m
       if (lk - lj) 14, 12, 14
   14  a(lk, li) = a(lk, li) - a(lj, li) * a(lk, lj)
   12  continue
       do 15 i=1,n
       do 15 j=m2,m
       l3 = j - n
  15   ai(i, l3) = a(i, j)
       return
      end

