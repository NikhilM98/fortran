c       Hermite Method
        dimension x(50),f(50),al(50),alp(50),fd(50)
        open(unit=5,file='herm.in',status='unknown')
        open(unit=6,file='herm.res',status='unknown')
        read(5,*)n,xx
        write(6,*)n,xx
        
        read(5,*) (x(i),f(i),fd(i),i=1,n)
        write(6,*) (x(i),f(i),fd(i),i=1,n)

        call poly(x,f,n,xx,al,alp)
        term=0.0
        do 1 i=1,n
        term1=(1-2*(xx-x(i))*alp(i))*al(i)*al(i)*f(i)
        term2=(xx-x(i))*al(i)*al(i)*fd(i)
1       term = term + term1 + term2
        write(6,*)xx,term
        close(unit=6)
        close(unit=5)
        stop
        end
        subroutine poly(x,f,n,xx,p,pd)
        dimension p(50),x(50),f(50),pd(50),pp(50)
        do 10 j=1,n
        p(j)=1.0 
        do 11 i =1,n
        if (i.eq.j) goto 11
        p(j)=p(j)*(xx-x(i))/(x(j)-x(i))
11      continue
10      continue
        do 20 j=1,n
        xxx = x(j)
        pp(j)=1.0
        pd(j)=1.0
        do 15 i = 1,n
        if(i.eq.j) goto 15
        pp(j)=pp(j)*(xxx-x(i))/(x(j)-x(i))
15      continue
        ss=0.0
        do 13 i=1,n
        if (i.eq.j) goto 13
        ss = ss + 1.0/(xxx-x(i))
13      continue 
        pd(j)=pp(j)*ss
20      continue
        return
        end

