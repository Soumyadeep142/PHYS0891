      implicit double precision(a-h, o-z)
      real :: ma(2,2), mb(2,2), mc(2,2), m1(2,2), m2(2,2), m3(2,2)
      real :: m4(2,2), t
      open(8, file='fibo_output.txt')
      
      E1=-5.d0
      Ea=0.d0
      Eb=0.d0
      Ec=0.d0
      tl=2.d0
      ts=1.d0
      En=E1

      do 25 ix=1,7000000
        if (En.gt.abs(E1)) goto 1
        En=En+0.00001
        ma(1,1)=(En-Ea)/tl
        ma(1,2)=-1
        ma(2,1)=1
        ma(2,2)=0
        
        mb(1,1)=(En-Eb)/ts
        mb(1,2)=-tl/ts
        mb(2,1)=1
        mb(2,2)=0
        
        mc(1,1)=(En-Ec)/tl
        mc(1,2)=-ts/tl
        mc(2,1)=1
        mc(2,2)=0
        
        m1=ma
        
        call trace_sum(m1, t, 2)
        if (abs(t).le.2) then
        	write(8,*) 1, En
        endif
        
        
        call matrix_mult(mc,mb, m2, 2,2,2)
        call trace_sum(m2, t, 2)
        if (abs(t).le.2) then
        	write(8,*) 2, En
        endif
	
	do 7 l=3,10
	  call matrix_mult(m1,m2,m3,2,2,2)
	  call trace_sum(m3, t, 2)
	  if (abs(t).le.2) then
        	write(8,*) l, En
          endif
	  
	  m1=m2
	  m2=m3
 7      continue
 25   continue	  
 
 
      
1     stop
      end
      
	subroutine matrix_mult(A, B, C, m, n, p)
	implicit none
	integer :: m, n, p, i, j, k
	real :: A(m,n), B(n,p), C(m,p)

c Perform matrix multiplication
	do 39 i = 1, m
	    do 42 j = 1, p
		C(i,j) = 0.0
		do 23 k = 1, n
		    C(i,j) = C(i,j) + A(i,k)*B(k,j)
 23		continue
 42	    continue
 39	continue

	end subroutine matrix_mult
	
	subroutine trace_sum(A,t,n)
	implicit none
	integer :: n,p,i
	real :: A(n,n), t
c Trace Sum
        t=0.d0
        do 3 i=1,n
        	t=t+A(i,i)
 3      continue
	
	end subroutine trace_sum
