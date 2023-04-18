      implicit double precision(a-h, o-z)
      real Lat(100,100), Lat_new(100,100)
      open(8, file='Laplace.txt')
      open(9, file='Laplace_relax.txt')
      
      do 7 i=1,100
      	Lat(1,i)=5.d0
      	Lat(i,100)=5.d0
      	Lat(i,1)=0.d0
      	Lat(100,i)=0.d0
 7    continue
      Lat(1,1)=5.d0
      Lat(100,100)=5.d0
 
      do 25 i=2,99
     	do 39 j=2,99
     		Lat(i,j)=5*rand()
 39  	continue
 25   continue
      
      Lat_new=Lat
      e=100.d0
      ic=1
      do 21 ix=1,50000
      	if (e.lt.10e-4) goto 2
      	do 19 i=2,99
      		do 1 j=2,99
      			Lat_new(i,j)=(Lat(i+1,j)+Lat(i-1,j)+Lat(i,j+1)+Lat(i,j-1))*0.25d0
 1		continue
 19	continue
       e=0.d0
       do 23 i=1,100
       	do 13 j=1,100
       		e=e+abs(Lat(i,j)-Lat_new(i,j))**2
 13     continue
 23    continue
       write(9,*)ic, log(e)
      Lat=Lat_new
      ic=ic+1
 21   continue
 
 2    do 139 k=1, 100
      	write(8,*) (Lat(k, is), is=1,100)
 139  continue
      stop
      end
     
