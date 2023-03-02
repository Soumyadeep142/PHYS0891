       implicit double precision(a-h, o-z)
       dimension y_range(10000001)
       open(8, file='sin_snap_1.txt')
       open(9, file='sin_snap_2.txt')
       
       pi=acos(-1.0)
       low_lim=0
       up_lim=pi
       
       h=(up_lim-low_lim)/10000000

c Finding max value of the function
       do 1 k=1,10000001
       x= a+(k-1)*h
       y_range(k)=f(x)       
 1     continue
 
       f_max=maxval(y_range)
       
       iseed1=92937103
       iseed2=93839371
       
       do 39 j=1,6
           N=10**j
           ic=0
           
           do 25 i=1,N
              x=low_lim+(up_lim-low_lim)*rand(iseed1)
              y=f_max*rand(iseed2)

              if (y.le.f(x)) then
              ic=ic+1
              if (j.eq.3) then
              write(8,*) x, y
              endif
              
              else
              if (j.eq.3) then
              write(9,*) x,y
              endif
              endif
              
            iseed1=iseed1*1234567
            iseed2=iseed2*234567
            

              
 25    continue
       Area=(ic*1.d0)/(N*1.d0)*pi
       write(*,*) N, Area
 39    continue  

       stop
       end
           
       double precision function f(x)
       implicit double precision(a-h, o-z)
       f=sin(x)
       return
       end
