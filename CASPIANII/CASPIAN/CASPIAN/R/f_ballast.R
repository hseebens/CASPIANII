#probability kernel for alien species due to ballast water release, depending on number of ship in port.
# num: number of ships
# a: curve coefficient, advised >1000, arbitrary

f_ballast<-function(num,a) {num/(a+num)}
