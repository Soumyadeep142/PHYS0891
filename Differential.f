      implicit double precision(a-h, o-z)
      dimension h_array(11), y_e(11), y_me(11)
      dimension y_t(11), et(11)
      open(8, file='op.txt')
      
      x_ini=0.d0
      h=0.1d0
      do 7 i=1,11
        h_array(i)=x_ini+(i-1)*h
 7    continue

c Euler Method 
      y=1 
      do 25 j=1,11
      	y_e(j)=y
        x=h_array(j)
        dely=h*f(x, y)
        y=y+dely
 25   continue
       
c Modified Euler
      y=1
      do 39 k=1,11
        y_me(k)=y
        x=h_array(k)
        dely=(h/2.d0)*(f(x,y)+f(x+h, y+h*f(x,y)))
        y=y+dely
 39   continue
c Taylor
      y=1
      do 42 ic=1,11
        y_t(ic)=y
        x=h_array(ic)
        h1=h*(x+y)
        h2=h**2.d0*(1+x+y)/(2.d0*1d0)
        h3=h**3.d0*(1+x+y)/(3.d0*2.d0)
        h4=h**4.d0*(1+x+y)/(4.d0*3.d0*2.d0*1d0)
        dely=h1+h2+h3+h4
        y=y+dely
 42   continue
 
c   et Soln
      do 23 m=1,11
        x=h_array(m)
        y=2*exp(x)-(x+1)
        et(m)=y
 23   continue 
 

      do 1 l=1,11
       write(8,*) h_array(l), y_e(l), y_me(l), y_t(l), et(l)
 1    continue
      stop
      end
          
      double precision function f(x,y)
      implicit double precision(a-h, o-z)
      f=x+y
      return 
      end
