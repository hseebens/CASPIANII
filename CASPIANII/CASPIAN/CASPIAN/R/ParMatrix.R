ParMatrix<-function(par_att0_Roads,par_att0_Railways,par_att1,par_att2,par_att3,
                    par_air0_Roads,par_air0_Railways,par_air1,par_air2,
                    par_nat1,par_nat2,par_nat_riverside1,par_nat_riverside2,par_est_T,par_cont,par_pall,
                    par_nat_a,par_nat_b,par_ball,
                    par_hull0,par_a,par_c1,par_g,par_c2,par_b,par_c3,par_est_W){

  pars<-matrix(c(par_att0_Roads,par_att0_Railways,par_att1,par_att2,par_att3,
                       par_air0_Roads,par_air0_Railways,par_air1,par_air2,
                       par_nat1,par_nat2,par_nat_riverside1,par_nat_riverside2,par_est_T,par_cont,par_pall,
                       par_nat_a,par_nat_b,par_ball,
                       par_hull0,par_a,par_c1,par_g,par_c2,par_b,par_c3,par_est_W
  ),nrow=1,byrow=T)
  colnames(pars)<-c("S_att0", "R_att0","att1","att2","att3","S_air0","R_air0", "air1","air2","nat1","nat2","nat_riverside1","nat_riverside2","estT","cont1","pall1",
                          "nat_a","nat_b","ball1","hull0","alpha","c1","gamma","c2","beta","c3","estW")
return(pars)
  }
