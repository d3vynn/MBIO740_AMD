source('Herbivore_Model.R')
source('CoralIPMs.R')

# time step 0: 

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
           fishbiomort = 0.5)

Comp <- fbm <- complexity_deviation <- IPMlams  <- rep(0,length=timesteps)
IPMssds <- matrix(ncol=timesteps,nrow=Ps$n)
IPM <- IPMFunc(Ps, SENS = FALSE)
SSDs <- IPM$StableSizeDist
IPMssds[,1] <- ssd
IPMlams[1] <- IPM$Lambda
sizeclasses <- IPM$y
Comp[1] <- Complexity(IPMlams[1], SSDs, sizeclasses)
fbm[1] <- calculate_fishbiomort(population[1,])[1]



for(t in 2:timesteps){
  tmp1 <- dispersal(patch_area, number_patches, S)
  tmp2 <- dispersal(patch_area, number_patches, S_recruit)
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
      
      carrying_capacity_complex[t] <- calculate_carrying_capacity_complex(carrying_capacity, complexity_deviation[t])
  
      population[t, i] <- calculate_population_growth(population[t-1, i], 
                                                      intrinsic_growth_rate, carrying_capacity_complex[t])
    } else {
      population[t, i] <- 0
    }
    
    #separate recruits from adults
    recruits[t, i] <- population[t, 1] - population[t-1, 1]
  }
  
  #recruit disperse
  recruits_dispersal[t,] <- recruits[t,] %*% tmp2
  
  for(i in 1:1){
    #add recruit dispersal back to population. Population after growth - original recruits + recruits after dispersal
    population[t, i] <- population[t, i] - recruits[t, i] + recruits_dispersal[t, i]
    # harvest
    fraction_harvested[t, i] <- calculate_fraction_harvested(fishing_effort[i], catchability)
    harvest[t, i] <- calculate_fisheries_harvest(population[t, i], fraction_harvested[t, i], patch_area_m2[i]) 
    
    #escapement
    escapement[t, i] <- calculate_escaped_stock_biomass(population[t, i], fraction_harvested[t, i])
  }
  #adult dispersal
  population[t,] <- escapement[t,] %*% tmp1
  fbm[t] <- calculate_fishbiomort(population[t, 1])[1]
  #calculate market equivalent revenue
  revenue <- (price * harvest) * value_added_ratio
  # after the above population dynamics run for time t and each patch, the next timestep (t+1) will proceed using values from time t.
}
