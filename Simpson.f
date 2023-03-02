      implicit double precision(a-h, o-z)
      dimension A(10000000)
      open(8, file='output_Simp.txt')
      
      pi=acos(-1.0)
      upper_limit=pi
      lower_limit=0
      
      do 39 l=1,6
      	N=10**l
      	
        h=(upper_limit-lower_limit)/(2*N)
        do 25 i=1,2*N+1
          A(i)=lower_limit+(i-1)*h
 25     continue
 
        s=0
        do 7 j=1,2*N-1, 2
          s=s+(h/3.0)*(f(A(j)) + 4*f(A(j+1)) + f(A(j+2)))
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
