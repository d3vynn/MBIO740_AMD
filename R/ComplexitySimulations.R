#### Complexity Simulations
## here we are building and running the simulations with the different comp curves

### No Burn in!
# 3 curve versions, 10 (fishing pressure) x 10 (rec values)
# Will save the models into a folder to deal with later
# make figures out of!


source('Herbivore_Model.R')
source('CoralIPMs.R')

Dir <- "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\ComplexitySims\\"

ComplexVer <- c("Logit", "Exp", "Log")[3]
FishPress  <- seq(0, 0.5, length.out = 10)
RecVals    <- seq(1, 15, length.out = 10)
HName      <- as.character(round(FishPress,2))
CName      <- as.character(round(RecVals,2))

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
           rec_val = 5,
           fishbiomort = 0)


for(d in 1:length(ComplexVer)){
  for(c in 1:length(RecVals)){
    Ps$rec_val  <- RecVals[c]
    Comp <- fbm <- rep(0,length=timesteps)
    complexity_deviation <- IPMlams  <- rep(0,length=timesteps)
    IPMssds     <- matrix(ncol=timesteps,nrow=Ps$n)
    IPM         <- IPMFunc(Ps, SENS = FALSE)
    SSDs        <- IPM$StableSizeDist
    IPMssds[,1] <- SSDs
    IPMlams[1]  <- 0
    Comp[1]     <- 0
    fbm[1]      <- 0
    complexity_deviation[1] <- 1
    for(h in 1:length(FishPress)){
      fishing_effort <- FishPress[h]
      for(t in 2:timesteps){
        for(i in 1:number_patches){
          #population growth
          #new IF statement so that if area is zero in a patch population is zero
          if (patch_area[i] > 0) { 
            Ps$fishbiomort = fbm[t-1]
            IPM  <- IPMFunc(Ps,SENS = FALSE)
            SSDs <- IPM$StableSizeDist
            sizeclasses <- IPM$y
            IPMssds[,t] <- SSDs
            IPMlams[t] <- IPM$Lambda
            
            Comp[t] <- Complexity(IPMlams[t], SSDs, sizeclasses, ModType = ComplexVer[d])
            complexity_deviation[t] <- Comp[t] / Comp[t-1] 
            
            carrying_capacity_complex[t] <- calculate_carrying_capacity_complex(carrying_capacity, 
                                                                                complexity_deviation[t])
            
            population[t,i] <- calculate_population_growth(population[t-1,i], 
                                                           intrinsic_growth_rate, 
                                                           carrying_capacity_complex[t])
          } else {
            population[t,i] <- 0
          }
        }
        
        
        
        for(i in 1:number_patches){
          #add recruit dispersal back to population. 
          # Population after growth - original recruits + recruits after dispersal
          # harvest
          fraction_harvested[t, i] <- calculate_fraction_harvested(fishing_effort[i], 
                                                                   catchability)
          harvest[t, i] <- calculate_fisheries_harvest(population[t, i], 
                                                       fraction_harvested[t, i], 
                                                       patch_area_m2[i]) 
          
          #escapement
          population[t, i] <- calculate_escaped_stock_biomass(population[t, i], 
                                                              fraction_harvested[t, i])
        }
        #adult dispersal
        fbm[t] <- calculate_fishbiomort(population[t, 1])[1]
        #calculate market equivalent revenue
        revenue <- (price * harvest) * value_added_ratio
        # after the above population dynamics run for time t 
        # and each patch, the next timestep (t+1) will proceed using values from time t.
      }
      
      FullModel <- list(FishPopulation=population,
                        Harvest=harvest,
                        FishMort=fbm,
                        rawComplex=Comp,
                        dComplex=complexity_deviation,
                        Lambdas=IPMlams)
      
      name <- paste0(HName[h],2, "_Effort_", CName[c],"_Rec_",ComplexVer[d],"_ComplexityFunc.rdata")
      
      save(FullModel, file=paste0(Dir,name))
      
      
    }
  }
  
}

