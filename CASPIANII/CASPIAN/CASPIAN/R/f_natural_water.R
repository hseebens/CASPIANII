# RIVER NATURAL DISPERSAL KERNEL, Elliot 2003, eq. 2a. Designed for DAILY dispersal
# a: scale parameter
# b:shape parameter
# d: distance (in Km)

f_natural_water<-function(a,b,d){
d<-d *1000 #convert Km to m
p<-a*d^(-b)
p<-pUnion(rep(p,30)) #calculate monthly probability as combination of getting there one day OR the next OR the next..
return(p)
}

