#probability kernel for alien species, depending on number of containers opened.
# num: number of containers
# a: curve coefficient, advised >1000, arbitrary

f_container<-function(num,a) {num/(a+num)}
