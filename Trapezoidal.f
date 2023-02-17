      implicit double precision(a-h, o-z)
      dimension A(10000000),N_range(100)
      open(8, file='output.txt')
      
      pi=acos(-1.0)
      upper_limit=pi
      lower_limit=0
      
      do 39 l=1,6
      	N=10**l
      	
        h=(upper_limit-lower_limit)/N
      
        do 25 i=1,N+1
          A(i)=lower_limit+(i-1)*h
 25     continue
 
        s=0
        do 7 j=1,N
          s=s+(h/2)*(f(A(j))+f(A(j+1)))
 7      continue
 
        exact_soln=2.0
        error=abs(2.0-s)
  
  
        write(8,*) N, s, log(error)
39      continue  
      stop 
      end
      
      double precision function f(x)
      implicit double precision(a-h,o-z)
      f=sin(x)
      return
      end
