source('CoralIPMs.R')

recZ <- seq(20, 25, length.out=10)
Ls <- rep(0, length(recZ))
for(i in 1:length(recZ)){
  Ps <- list(g.int = -0.295459528,
             g.slp = 0.897642821,
             g.var = 0.148459262,
             s.int = 6.619306118,
             s.slp = 4.356206835,
             s.slp2 = 0.60252172,	
             rec.size =	-2.868638,
             min.size = -3,
             max.size = 2,
             n = 100,
             rec_val = recZ[i],
             fishbiomort = 0.7)
  
  IPM   <- IPMFunc(Ps, SENS = FALSE)
  Ls[i] <- IPM$Lambda
  
}
Ls

recs <- c(13,20,26)
