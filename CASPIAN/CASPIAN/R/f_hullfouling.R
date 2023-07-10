# HULL FOWLING KERNEL, sylvester 2011, eq. 9

# N is the number of organisms on a given ship,
# Gain is a function of the number of organisms present (determined by K1 and K2),
# the degree of accumulation (Ap), and the efficacy of antifouling paint (Sp).
# We hypothesize that accumulation depends on the duration of the stay at port p (Dp),
# and that Sp depends on the time since the last application of antifouling paint (Qp). WE ASSUME NO ANTIFOULING PAINT
#
# K1 and K2 are scalars,
# x is a dummy variable that allows the sampling ports to differ. ASSUME 0 FOR HOMOGENEOUS CONDITIONS?
# Tp is travel time since departure from port p, (days)
# V is typical sailing speed, not included (knots, 1knot= 1.852 km/h = 44.448 km/d)
# VTp is distance travelled. (in km, function takes care of conversion to nautical miles. 1 nm=1.852 Km. 1Km= 0,54 nm)
# Dp: duration of the stay at port p (days)
# Qp: time since the last application of antifouling paint (days)
#
# sylvester 2011: we treat avery node as a port, assuming very short Dp, Qp=0
# Turn into probabilities: K1=1,K2=0,x irrelevant
# a =5.85 * 10^-20, c1 =20.9, g =1.03 * 10^-10, c2 =3.63, b =3.15 * 10^-7, and c3 =2.39

f_hullfouling<-function(K1=1,K2=0,x=1,
                        a ,c1,
                        g, c2,
                        b,c3,
                        Dp,Qp,VTp){

  VTp<-VTp*0.54 #Km to nautical miles

  ifelse (is.na(Qp),
          yes= Qp_term<-1,
          no= Qp_term<-1-exp(-g*(Qp^c2))
  )
  ifelse (is.na(Dp),
          yes= Dp_term<-1,
          no= Dp_term<-1-exp(-a*(Dp^c1))
  )
  Nind<- (K1+K2*x) * Dp_term * Qp_term * exp(-b*(VTp^c3))

  return(Nind)
}
