      implicit double precision(a-h, o-z)
      double complex tprl, tprs, tl, ts, eim, ea, eb, ec, epra
      double complex eprb, eprc, Ga, Gb, Gc
      
      open(8, file='dena.txt')
      open(9, file='denb.txt')
      open(10, file='denc.txt')
      
      En=-2.5d0
      pi=acos(-1.d0)
      abs_E=abs(En)
      
      do 25 while (En.le.abs_E)
        eim=dcmplx(En, 0.01d0)
        ea=dcmplx(0.d0, 0.d0)
        eb=dcmplx(0.d0, 0.d0)
        ec=dcmplx(0.d0, 0.d0)
        tl=dcmplx(1.d0, 0.d0)
        ts=dcmplx(1.d0, 0.d0)
        
        abs_t=cdabs(tl)+cdabs(ts)
        do 39 while (abs_t.ge.10e-9)
            epra=ec+(tl*tl+ts*ts)/(eim-eb)
            eprb=ec+(ts*ts)/(eim-eb)
            eprc=ea+(tl*tl)/(eim-eb)
            tprl=(tl*ts)/(eim-eb)
            tprs=tl
            
            ea=epra
            eb=eprb
            ec=eprc
            tl=tprl
            ts=tprs
            abs_t=cdabs(tl)+cdabs(ts)
 39     continue
        
        Ga=1/(eim-ea)
        Gb=1/(eim-eb)
        Gc=1/(eim-ec)
        rhoa=(-1/pi)*dimag(Ga)
        rhob=(-1/pi)*dimag(Gb)
        rhoc=(-1/pi)*dimag(Gc)
        write(8,*) En, rhoa
        write(9,*) En, rhob
        write(10,*) En, rhoc
        
        En=En+0.0001d0
 25   continue
 
      stop
      end
            
