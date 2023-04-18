      implicit none
      integer L,S,i,j, k,m, ix,iy
      integer Lat(200),Lat_new(200)
      character,dimension(200)::x
      open(8, file='Fibo_Chain.txt')
      L=1
      S=2
      Lat(1)=L
      
      do 7 i=1,10      	
      	k=1
      	ix=0
      	iy=0
      	do 25 m=1,200
	      	if (Lat(m).eq.1) then
	      	  Lat_new(k)=L
	      	  Lat_new(k+1)=S
	      	  x(k)='L'
	      	  x(k+1)='S'
	      	  ix=ix+1
	      	  iy=iy+1
	      	  k=k+2
	      	endif
	      	
	      	if  (Lat(m).eq.2) then
	      	  Lat_new(k)=L
	      	  x(k)='L'
	      	  k=k+1
	      	  ix=ix+1
	      	endif
	Lat=Lat_new
 25     continue
 7      continue
      
      write(8,*)(x)
      
      stop
      end

c output
c LSLLSLSLLSLLSLSLLSLSLLSLLSLSLLSLLSLSLLSLSLLSLLSLSLLSLSLLSLLSLSLLSLLSLSLLSLSLLSLLSLSLLSLLSLSLLSLSLLSLLSLSLLSLSLLSLLSLSLLSLLSLSLLSLSLLSLLSLSLLSLLSLSLLSLSLLSLLSLSLLSLSLLSLLSLSLLSLLSLSLLSLSLLSLLSLSLLSLSLL
