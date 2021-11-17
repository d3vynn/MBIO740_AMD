source('Herbivore_Model.R')
source('CoralIPMs.R')
source("BurninParams.R")

HavScenarios  <- c(0.05,0.1, 0.3) # Low and High
HName <- c("Low","Medium", "High")
CoralPopScens <- c(3,7,13) #l = 0.5, l = 1, l = 1.5
CName <- c("Declining", "Stable", "Increasing")


lambdaInt
fishBiomort
for(c in 1:length(CoralPopScens)){
  Ps$rec_val <- CoralPopScens[c]
  Comp <- fbm <- rep(0,length=timesteps)
  complexity_deviation <- IPMlams  <- rep(0,length=timesteps)
  IPMssds <- matrix(ncol=timesteps,nrow=Ps$n)
  IPM <- IPMFunc(Ps, SENS = FALSE)
  SSDs <- IPM$StableSizeDist
  IPMssds[,1] <- SSDs
  IPMlams[1] <- lambdaInt[c]
  Comp[1]    <- ComplexityInt[c]
  fbm[1]     <- fishBiomort[c]
  complexity_deviation[1] <- ChgComplexInt[c]
  for(h in 1:length(HavScenarios)){
    fishing_effort <- HavScenarios[h]
    for(t in 2:timesteps){
      for(i in 1:number_patches){
        #population growth
        #new IF statement so that if area is zero in a patch population is zero
        if (patch_area[i] > 0) { 
          Ps$fishbiomort = fbm[t-1]
          IPM <- IPMFunc(Ps,SENS = FALSE)
          SSDs <- IPM$StableSizeDist
          sizeclasses <- IPM$y
          IPMssds[,t] <- SSDs
          IPMlams[t] <- IPM$Lambda
          
          Comp[t] <- Complexity(IPMlams[t], SSDs, sizeclasses)
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
    
    name <- paste0(HName[h], "_Effort_", CName[c], "_Pop_model.rdata")
    Dir  <-  "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\Another\\"
    
    save(FullModel, file=paste0(Dir,name))
    
    
  }
}

# population, harvest, complexity values, complexity deviance, fbm, lambdas, and sensitivities/elasts.
