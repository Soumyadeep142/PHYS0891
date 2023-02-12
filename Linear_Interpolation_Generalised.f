      implicit double precision(a-h, o-z)
      dimension x(100), y(100),z(100)
      open(7, file='Data.txt')
      
      write(*,*) 'Give the desired point'
      read(*,*) x_int
      do 17 i=1,100
      read(7,*,end=3)x(i), y(i)
 17   continue
      
 3    n=i-1
      j=0
      iy_len=n
      s=y(1)
      do 25 i=1,n-1
      	 do 2 ix=1,iy_len
      	     z(ix)=0
 2       continue
         j=j+1
         k=1
         do 39 l=1,iy_len
             z(l)=(y(l+1)-y(l))/(x(k+j)-x(k))
             k=k+1
 39      continue
      
         y=z
         c=1.0
         do 1 m=1,j
            c=c*(x_int-x(m))
 1       continue
         s=s+c*y(1)
      iy_len=iy_len-1
 25   continue
 
      
      write(*,*) s
      stop
      end
