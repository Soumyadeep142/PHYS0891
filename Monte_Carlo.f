       implicit double precision(a-h, o-z)
       dimension y_range(10000001)
       open(8, file='pi_op.txt')

       
       low_lim=0
       up_lim=1
       
       h=(up_lim-low_lim)/10000000
       x=a
c Finding max value of the function
       do 1 k=1,10000001
       x= a+(k-1)*h
       y_range(k)=f(x)       
 1     continue
 
       f_max=maxval(y_range)
       
       iseed1=92937103
       iseed2=93839371
       
c Area of Rectangle
       Area_rect=(up_lim-low_lim)*f_max
       
c Monte Carlo
       
       do 39 j=1,100000
           N=j
           ic=0
           write(*,*) j
           
           do 25 i=1,N
              x=low_lim+(up_lim-low_lim)*rand(iseed1)
              y=f_max*rand(iseed2)

              if (y.le.f(x)) then
              ic=ic+1
              endif
              
            iseed1=iseed1*1234567
            iseed2=iseed2*234567
 25    continue
       Area_curve=(ic*1.d0)/(N*1.d0)*Area_rect
       
       Ans=Area_curve*4
       write(8,*) N, Ans
       
       
 39    continue  

       stop
       end
           
       double precision function f(x)
       implicit double precision(a-h, o-z)
       f=sqrt(1-x**2)
       return
       end
