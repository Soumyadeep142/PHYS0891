      implicit double precision(a-h, o-z)
      dimension h_array(11), y_rk4(11)
      real k1,k2,k3,k4
      open(8, file='diff_rk4.txt')
      
      x_ini=0.d0
      h=0.1d0
      do 7 i=1,11
        h_array(i)=x_ini+(i-1)*h
 7    continue
c RK4 method      
      y=1.d0
      do 39 i=1,11
      	y_rk4(i)=y
      	x=h_array(i)
      	k1=h*f(x,y)
      	k2=h*f(x+h/2.d0, y+k1/2.d0)
      	k3=h*f(x+h/2.d0, y+k2/2.d0)
      	k4=h*f(x+h/2.d0, y+k3)
      	dely=(1.d0/6.d0)*(k1+2*k2+2*k3+k4)
      	y=y+dely
 39   continue  
 
      do 1 l=1,11
       write(8,*) h_array(l), y_rk4(l)
 1    continue     
      stop
      end      
      
      double precision function f(x,y)
      implicit double precision(a-h, o-z)
      f=x+y
      return
      end    
