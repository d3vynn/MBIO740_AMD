source('~/GitHub/MBIO740_AMD/CoralIPMs.R', echo=TRUE)
Dirc <- "~/GitHub/MBIO740_AMD/output/CoralIPM_GS_test/"
# Write up a global sensitivity analysis 
#     with a local elasticity also

Ps <- list(g.int = -0.295459528,
           g.slp = 0.897642821,
           g.var = 0.148459262,
           s.int = 6.619306118,
           s.slp = 4.356206835,
           s.slp2 = 0.60252172,	
           rec.size =	-2.868638,
           min.size = -3,
           max.size = 2,
           n = 100)

recs <- seq(0.5,20,length.out = 10)
fbm  <- seq(0.05, 0.95, length.out = 10)

for (i in 1:length(recs)){
  
  Ps$rec_val <- recs[i]
  for (j in 1:length(fbm)){
    
    Ps$fishbiomort <- fbm[j]
    
    Data <- IPMFunc(Parameters = Ps)
    name <- paste0("rec", round(recs[i],2), "_fish", round(fbm[j],2),"_GS.rdata")
    save(Data, file=paste0(Dirc,name))
  }
}

################################################################################
################################################################################
ls <- list.files(Dirc)

load(paste0(Dirc,ls[1])) # this is an .rdata file with the data named "Data"

popgrowthrate      <- Data$Lambda
sizeclasses        <- Data$y
stablesizedist     <- Data$StableSizeDist
Total_num_colonies <- 10000
Vector_inds        <- stablesizedist * Total_num_colonies # This is the thing Mollie would care about!

plot(10^sizeclasses, Vector_inds, xlab = "Coral Diameter (cm)", 
     ylab = "# of individuals in each size class",
     main= paste0("Population Growth Rate = ", popgrowthrate))

plot(sizeclasses, Vector_inds, xlab = "Coral Diameter (log-scale)", 
     ylab = "# of individuals in each size class", 
     main= paste0("Population Growth Rate = ", popgrowthrate))


