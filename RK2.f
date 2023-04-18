      implicit double precision(a-h, o-z)
      dimension h_array(11), y_rk2_1(11), y_rk2_2(11), y_rk2_3(11)
      real k1,k2
      open(8, file='diff_rk2.txt')
      
      x_ini=0.d0
      h=0.1d0
      do 7 i=1,11
        h_array(i)=x_ini+(i-1)*h
 7    continue
c RK2 method      
      y1=1.d0
      y2=1.d0
      y3=1.d0
      do 39 i=1,11
      	y_rk2_1(i)=y1
      	y_rk2_2(i)=y2
      	y_rk2_3(i)=y3
      	x=h_array(i)
      	call RK2(1.d0/2.d0,1.d0/2.d0,1.d0,1.d0,x,y1,h,dely)
      	y1=y1+dely
      	
      	call RK2(1.d0/3.d0,2.d0/3.d0,3.d0/4.d0,3.d0/4.d0,x,y2,h,dely)
      	y2=y2+dely
      	
      	call RK2(1.d0/7.d0,6.d0/7.d0,7.d0/12.d0,7.d0/12.d0,x,y3,h,dely)
      	y3=y3+dely
 39   continue  
 
      do 1 l=1,11
       write(8,*) h_array(l), y_rk2_1(l), y_rk2_2(l), y_rk2_3(l)
 1    continue     
      stop
      end      
      
      double precision function f(x,y)
      implicit double precision(a-h, o-z)
      f=x+y
      return
      end
      
      subroutine RK2(a,b,alpha,beta,x,y,h,dely)
      implicit double precision(a-h, o-z)
      real k1,k2
      
      k1=h*f(x,y)
      k2=h*f(x+alpha*h, y+beta*k1)
      dely=a*k1+b*k2
      
      end subroutine RK2
