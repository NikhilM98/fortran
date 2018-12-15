c     Program for Lagrange Inerpolation
      dimension  x(100), y(100)
      write(*,*) 'Enter Input File Name'
      read(*,*) input
      if (input .eq. 1) then
        open(unit=1, file='lagrange1.in', status='unknown')
      else if (input .eq. 2) then
        open(unit=1, file='lagrange2.in', status='unknown')
      else if (input .eq. 2) then
        open(unit=1, file='lagrange3.in', status='unknown')
      end if
      read(1,*) n
      do i=1,n
       read(1, *) x(i), y(i)
      end do
      write(*,*) 'Enter the value of x and press Enter'
      read(*,*) z
      q = 0
      do j=1,n
       p = 1
       do k=1,n
        if (j .ne. k) then
         p = p*(z-x(j))/(x(k)-x(j))
        end if
       end do
       p = p*y(j)
       q = q + p
      end do
      do i=1,n 
       write(*,*) x(i), y(i)
      end do
      write(*,*) z, q
      end

