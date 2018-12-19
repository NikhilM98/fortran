c     Program for Gauss Sidel Method
      parameter(ndim = 3)
      dimension a(ndim, ndim), xg(ndim), c(ndim), x(ndim), err(ndim)
      open(unit=1, file='data3.in', status='unknown')
      open(unit=2, file='ans3.out', status='unknown')
      read(1, *) niter
      read(1, *)(xg(i), i=1, ndim)
      write(2, *)(xg(i), i=1, ndim)
      read(1, *)(c(i), i=1, ndim)
      write(2, *)(c(i), i=1, ndim)
      do i=1,ndim
       do j=1,ndim
        read(1, *) a(i, j)
        write(2, *) a(i, j)
       end do
      end do
      do i=1,ndim
       x(i)=xg(i)
      end do
      do iter=1,niter
       do i=1,ndim
        term = 0.0
        do j=1,ndim
         if(i.eq.j) goto 10
          term = term + a(i, j)*x(j)
   10    continue
        end do
        x(i) = (c(i) - term)/a(i,i)
       end do
       do i=1,ndim
        err(i) = 100*abs((x(i) - xg(i))/x(i))
        xg(i) = x(i)
       end do
       write(2, *)(x(i), err(i), i=1,ndim)
      end do
      stop
      end
