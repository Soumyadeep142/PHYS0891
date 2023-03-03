      implicit double precision(a-h, o-z)
      double complex tpr, t, eim, e, epr, G 
      
      open(8, file='Lattice_op.txt')
      En=-2.5d0
      pi=acos(-1.d0)
      abs_E=abs(En)
      
      do 25 while (En.le.abs_E)
        eim=dcmplx(En, 0.01d0)
        e=dcmplx(0.d0, 0.d0)
        t=dcmplx(1.d0, 0.d0)
        
        abs_t=cdabs(t)
        do 39 while (abs_t.ge.10e-9)
            tpr=t*t/(eim-e)
            epr=e+2*tpr
            t=tpr
            e=epr
            abs_t=cdabs(t)
 39     continue
        
        G=1/(eim-e)
        rho=(-1/pi)*dimag(G)
        
        write(8,*) En, rho
        En=En+0.01d0
 25   continue
 
 
      stop
      end
            
