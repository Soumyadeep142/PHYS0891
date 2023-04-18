      implicit double precision(a-h, o-z)
      dimension h_array(11), y_me(11)
      dimension y_t_2(11), et(11), y_t_4(11)
      open(8, file='op_Tay.txt')

      x_ini=0.d0
      h=0.1d0
      do 7 i=1,11
        h_array(i)=x_ini+(i-1)*h
 7    continue

c Modified Euler
      y=1
      do 39 k=1,11
        y_me(k)=y
        x=h_array(k)
        dely=(h/2.d0)*(f(x,y)+f(x+h, y+h*f(x,y)))
        y=y+dely
 39   continue
c Taylor 4th order
      y=1
      do 42 ic=1,11
        y_t_4(ic)=y
        x=h_array(ic)
        h1=h*(x+y)
        h2=h**2.d0*(1+x+y)/(2.d0*1d0)
        h3=h**3.d0*(1+x+y)/(3.d0*2.d0)
        h4=h**4.d0*(1+x+y)/(4.d0*3.d0*2.d0*1d0)
        dely=h1+h2+h3+h4
        y=y+dely
 42   continue
 
c Taylor 2nd order
      y=1
      do 142 is=1,11
        y_t_2(is)=y
        x=h_array(is)
        h1=h*(x+y)
        h2=h**2.d0*(1+x+y)/(2.d0*1d0)
        dely=h1+h2
        y=y+dely
 142  continue
c   exact Soln
      do 23 m=1,11
        x=h_array(m)
        y=2*exp(x)-(x+1)
        et(m)=y
 23   continue 
 

      do 1 l=1,11
       write(8,*) h_array(l), y_me(l), y_t_2(l), y_t_4(l), et(l)
 1    continue
      stop
      end
          
      double precision function f(x,y)
      implicit double precision(a-h, o-z)
      f=x+y
      return 
      end
