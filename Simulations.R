############################### Simulations ####################################
library(tidyverse)
Dir <- "~/Documents/GitHub/MegaModelCubed/code/"
source(paste0(Dir, 'Functions.R'))
################################################################################
############################ Model Parameters ##################################
############################### FISHERIES ######################################

FishParams <- list(timesteps=100,
                   carrying_capacity=342,
                   fishBio_slp=-0.002,
                   patch_area=1,
                   number_patches=1, 
                   patch_area_m2=(1 * 5.04e+8),
                   fishing_effort=0.1,
                   catchability=1,
                   intrinsic_growth_rate=0.3, 
                   price=0.01448438, 
                   value_added_ratio=0.9,
                   starting_population=150) 

population <- 
carrying_capacity_complex <- 
fraction_harvested <- 
harvest <- 
escapement <- array(NA, dim = c(FishParams$timesteps, 
                                FishParams$number_patches))

population[1] <- FishParams$starting_population

################################ CORAL IPMS ####################################

Params <- list(g.int = -0.295459528,
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

######################### COMPLEXITY PARAMETERS ################################
ComplexVer <- c("Logit", "Exp", "Log")
compMean   <- c(7,50,90)
compScale  <- c(2,10,20)

#########################################################

### Model sensitivity analysis

Dir_put <- "~/Documents/GitHub/MegaModelCubed/output/Sensitivity/"

FishPress  <- seq(0, 0.5, length.out = 10)
FishPress  <- c(FishPress[1],FishPress[5],FishPress[10])
RecVals    <- seq(1, 15, length.out = 10)
RecVals    <- c(RecVals [1],RecVals [5],RecVals [10])
BioSlope   <- seq(-0.0015, -0.0035, length.out = 10)
BioInt     <- seq(0.1, 1, length.out = 10)
slpName    <- as.character(round(BioSlope,4))
intName    <- as.character(round(BioInt,2))
HName      <- as.character(round(FishPress,2))
CName      <- as.character(round(RecVals,2))

d <- 1
for(c in 1:length(RecVals)){
  Ps          <- Params
  Ps$rec_val  <- RecVals[c]
  Comp <- fbm <- IPMlams <- complexity_deviation <- 
    rep(0,length=FishParams$timesteps)
  IPMssds     <- matrix(ncol=FishParams$timesteps,nrow=Ps$n)
  IPM         <- IPMFunc(Ps, SENS = FALSE)
  SSDs        <- IPM$StableSizeDist
  IPMssds[,1] <- SSDs
  IPMlams[1]  <- 0
  Comp[1]     <- 0.5
  fbm[1]      <- 0
  complexity_deviation[1] <- 1
  
  for(h in 1:length(FishPress)){
    fishing_effort <- FishPress[h]
    for(slp in 1:length(BioSlope)){
      for(intc in 1:length(BioInt)){
        for(t in 2:FishParams$timesteps){
          for(i in 1:FishParams$number_patches){
            
            if (FishParams$patch_area[i] > 0) { 
              Ps$fishbiomort = fbm[t-1]
              IPM  <- IPMFunc(Ps,SENS = FALSE)
              SSDs <- IPM$StableSizeDist
              sizeclasses <- IPM$y
              IPMssds[,t] <- SSDs
              IPMlams[t] <- IPM$Lambda
              
              Comp[t] <- ComplexityV2(IPMlams[t], SSDs, sizeclasses, 
                                      mean=compMean[d], scale=compScale[d])
              complexity_deviation[t] <- Comp[t] / Comp[t-1] 
              
              carrying_capacity_complex[t] <- 
                calculate_carrying_capacity_complex(FishParams$carrying_capacity, 
                                                    complexity_deviation[t])
              
              population[t,i] <- 
                calculate_population_growth(population[t-1,i], 
                                            FishParams$intrinsic_growth_rate, 
                                            carrying_capacity_complex[t])
            } else {
              population[t,i] <- 0
            }
          }
          
          for(i in 1:FishParams$number_patches){
            
            fraction_harvested[t,i] <- 
              calculate_fraction_harvested(FishParams$fishing_effort[i], 
                                           FishParams$catchability)
            harvest[t,i] <- 
              calculate_fisheries_harvest(population[t,i], 
                                          fraction_harvested[t,i], 
                                          FishParams$patch_area_m2[i]) 
            
            
            population[t,i] <- 
              calculate_escaped_stock_biomass(population[t,i], 
                                              fraction_harvested[t,i])
          }
          
          fbm[t] <- calculate_fishbiomort(BioSlope[slp],
                                          population[t,1],
                                          BioInt[intc])[1]
          
          revenue <- (FishParams$price * harvest) * FishParams$value_added_ratio
          
        }
        
  
    
    FullModel <- list(FishPopulation=population,
                      Harvest=harvest,
                      FishMort=fbm,
                      rawComplex=Comp,
                      dComplex=complexity_deviation,
                      Lambdas=IPMlams)
    
    name <- paste0(HName[h], "_Effort_", 
                   CName[c],"_Rec_",
                   slpName[slp], "_ibioSlp_",
                   intName[intc], "_ibioInt_",
                   ComplexVer[d],".rdata")
    
    save(FullModel, file=paste0(Dir_put,name))
      }
    }
  }
}
  






################################################################################
######################### RUNNING SIMULATIONS ##################################
################### FISHING PRESSURE VS. RECRUITMENT ###########################

Dir_put <- "~/Documents/GitHub/MegaModelCubed/output/RecvsPressure/"

FishPress  <- seq(0, 0.5, length.out = 10)
RecVals    <- seq(1, 15, length.out = 10)
HName      <- as.character(round(FishPress,2))
CName      <- as.character(round(RecVals,2))

for(d in 1:length(ComplexVer)){
  for(c in 1:length(RecVals)){
    Ps          <- Params
    Ps$rec_val  <- RecVals[c]
    Comp <- fbm <- IPMlams <- complexity_deviation <- 
                              rep(0,length=FishParams$timesteps)
    IPMssds     <- matrix(ncol=FishParams$timesteps,nrow=Ps$n)
    IPM         <- IPMFunc(Ps, SENS = FALSE)
    SSDs        <- IPM$StableSizeDist
    IPMssds[,1] <- SSDs
    IPMlams[1]  <- 0
    Comp[1]     <- 0.5
    fbm[1]      <- 0
    complexity_deviation[1] <- 1
    
    for(h in 1:length(FishPress)){
      fishing_effort <- FishPress[h]
      for(t in 2:FishParams$timesteps){
        for(i in 1:FishParams$number_patches){
          
          if (FishParams$patch_area[i] > 0) { 
            Ps$fishbiomort = fbm[t-1]
            IPM  <- IPMFunc(Ps,SENS = FALSE)
            SSDs <- IPM$StableSizeDist
            sizeclasses <- IPM$y
            IPMssds[,t] <- SSDs
            IPMlams[t] <- IPM$Lambda
            
            Comp[t] <- ComplexityV2(IPMlams[t], SSDs, sizeclasses, 
                                    mean=compMean[d], scale=compScale[d])
            complexity_deviation[t] <- Comp[t] / Comp[t-1] 
            
            carrying_capacity_complex[t] <- 
              calculate_carrying_capacity_complex(FishParams$carrying_capacity, 
              complexity_deviation[t])
            
            population[t,i] <- 
              calculate_population_growth(population[t-1,i], 
                                          FishParams$intrinsic_growth_rate, 
                                          carrying_capacity_complex[t])
          } else {
            population[t,i] <- 0
          }
        }
        
        for(i in 1:FishParams$number_patches){
          
          fraction_harvested[t,i] <- 
            calculate_fraction_harvested(FishParams$fishing_effort[i], 
                                         FishParams$catchability)
          harvest[t,i] <- 
            calculate_fisheries_harvest(population[t,i], 
                                        fraction_harvested[t,i], 
                                        FishParams$patch_area_m2[i]) 
          
          
          population[t,i] <- 
            calculate_escaped_stock_biomass(population[t,i], 
                                            fraction_harvested[t,i])
        }
        
        fbm[t] <- fishBio_sens
        # fbm[t] <- calculate_fishbiomort(-0.0037,
        #                                 population[t,1])[1]
        
        revenue <- (FishParams$price * harvest) * FishParams$value_added_ratio
        
      }
      
      FullModel <- list(FishPopulation=population,
                        Harvest=harvest,
                        FishMort=fbm,
                        rawComplex=Comp,
                        dComplex=complexity_deviation,
                        Lambdas=IPMlams)
      
      name <- paste0(HName[h], "_Effort_", 
                     CName[c],"_Rec_",
                     ComplexVer[d],"_ComplexityFunc_V2.rdata")
      
      save(FullModel, file=paste0(Dir_put,name))
      
      
    }
  }
  
}
##################################################################################

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



CoralPopScens <- c(3,8,10) #l = 0.5, l = 1, l = 1.5
CName <- c("Declining", "Stable", "Increasing")
fishing_effort <- 0 # no fishing effort to allow for burnin

for(c in 1:length(CoralPopScens)){
  Comp <- fbm <- complexity_deviation <- IPMlams  <- rep(0,length=timesteps)
  IPMssds     <- matrix(ncol=timesteps,nrow=Ps$n)
  IPM         <- IPMFunc(Ps, SENS = FALSE)
  SSDs        <- IPM$StableSizeDist
  IPMssds[,1] <- SSDs
  IPMlams[1]  <- IPM$Lambda
  sizeclasses <- IPM$y
  Comp[1]     <- Complexity(IPMlams[1], SSDs, sizeclasses)
  fbm[1]      <- calculate_fishbiomort(FishParams$fishBio_slp,
                                       population[1,])[1]
  Ps$rec_val <- CoralPopScens[c]
  for(t in 2:timesteps){
    for(i in 1:number_patches){
      #population growth
      #new IF statement so that if area is zero in a patch population is zero
      if (FishParams$patch_area[i] > 0) { 
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
                                                       FishParams$intrinsic_growth_rate, 
                                                       carrying_capacity_complex[t])
      } else {
        population[t,i] <- 0
      }
    }
    
    
    
    for(i in 1:FishParams$number_patches){
      
      fraction_harvested[t, i] <- calculate_fraction_harvested(fishing_effort[i], 
                                                               catchability)
      harvest[t, i] <- calculate_fisheries_harvest(population[t, i], 
                                                   fraction_harvested[t, i], 
                                                   FishParams$patch_area_m2[i]) 
      
      
      population[t, i] <- calculate_escaped_stock_biomass(population[t, i], 
                                                          fraction_harvested[t, i])
    }
   
    fbm[t] <- calculate_fishbiomort(FishParams$fishBio_slp, 
                                    population[t, 1])[1]
    
    revenue <- (price * harvest) * value_added_ratio
    
  }
  
  Burnin <- list(FishPopulation=population,
                 Harvest=harvest,
                 FishMort=fbm,
                 rawComplex=Comp,
                 dComplex=complexity_deviation,
                 Lambdas=IPMlams)
  
  name <- paste0( "Burnin30_", CName[c], "_Pop_model.rdata")
  Dir  <-  "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\Burnin\\"
  
  save(Burnin, file=paste0(Dir,name))
  
  
}
################################################################################
####################### DISTURBANCE/PERTURBATIONS ##############################
################################################################################
####### Distrubance model

### Fish distubance = 50 % decrease in fish biomass
##### - 3 coral levels & 3 fishing pressures
### Complexity distubrance = 50 % decrease in complexity
##### - 3 coral levels & 3 fishing pressures
### Coral disturbance = 50 % increase in mortality for a particular year
##### - 3 coral levels & 3 fishing pressures

#### Use same burn in as first analysis
#### Start disturbance at year 20 or 25, 
###### (maybe run a few versions at earlier times too?) <- compare the impact/ recovery?



BaseD <- "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\"
CType <- c("LowIntComp", "IntIntComp")
v <- 1
Dir <- paste0(BaseD, "\\DisturbanceSims\\", CType[v], "\\")

ComplexVer <- c("Logit", "Exp", "Log")

FishPress  <- seq(0, 0.5, length.out = 10)[3]
RecVals    <- seq(2, 13, length.out = 10)[5]
HName      <- as.character(round(FishPress,2))
CName      <- as.character(round(RecVals,2))



DistTime  <- 51
Disturb <- round(seq(0.1,0.95,length.out = 10),2)
DistType <- c("Hurricane", "Disease", "Bleaching")

for(cx in 1:3){
  for(dis in 1:3){
    for(d in 1:length(Disturb)){
      for(c in 1:length(RecVals)){
        Ps          <- Params
        Ps$rec_val  <- RecVals[c]
        Comp <- fbm <- rep(0,length=timesteps)
        complexity_deviation <- IPMlams  <- rep(0,length=timesteps)
        IPMssds     <- matrix(ncol=timesteps,nrow=Ps$n)
        IPM         <- IPMFunc(Ps, SENS = FALSE)
        SSDs        <- IPM$StableSizeDist
        IPMssds[,1] <- SSDs
        IPMlams[1]  <- 0
        Comp[1]     <- 0.001
        fbm[1]      <- 0.001
        complexity_deviation[1] <- 1
        for(h in 1:length(FishPress)){
          fishing_effort <- FishPress[h]
          for(t in 2:timesteps){
            if(t == DistTime){
              if(dis == 1){
                Ps$fishbiomort = fbm[t-1]
                IPM  <- IPMFunc(Ps,SENS = FALSE)
                IPMlams[t] <- IPM$Lambda
                SSDs <- IPM$StableSizeDist
                sizeclasses <- IPM$y
                IPMssds[,t] <- SSDs
                
                #IPMlams[t] <- IPMlams[t] - (IPMlams[t] * (Disturb-0.2))
                Comp[t] <- Comp[t-1] - (Comp[t-1] * Disturb[d])
                complexity_deviation[t] <- Comp[t] / Comp[t-1] 
                
                carrying_capacity_complex[t] <- calculate_carrying_capacity_complex(carrying_capacity, 
                                                                                    complexity_deviation[t])
                
                population[t,1] <- calculate_population_growth(population[t-1,1], 
                                                               FishParams$intrinsic_growth_rate, 
                                                               carrying_capacity_complex[t])
                
                fraction_harvested[t, 1] <- calculate_fraction_harvested(fishing_effort[1], 
                                                                         catchability)
                harvest[t, 1] <- calculate_fisheries_harvest(population[t, 1], 
                                                             fraction_harvested[t, 1], 
                                                             FishParams$patch_area_m2[1]) 
                
                #escapement
                population[t, i] <- calculate_escaped_stock_biomass(population[t, 1], 
                                                                    fraction_harvested[t, 1])
                fbm[t] <- calculate_fishbiomort(FishParams$fishBio_slp,
                                                population[t, 1])[1]
                #calculate market equivalent revenue
                revenue <- (price * harvest) * value_added_ratio
              } 
              # simulating a hurricane
              
              if(dis == 2){
                Ps$fishbiomort = fbm[t-1]
                IPM  <- IPMFunc(Ps,SENS = FALSE)
                SSDs <- IPM$StableSizeDist
                sizeclasses <- IPM$y
                IPMssds[,t] <- SSDs
                IPMlams[t] <- IPM$Lambda
                
                
                Comp[t] <- ComplexityV2(IPMlams[t], SSDs, sizeclasses, 
                                        mean=compMean[cx], scale=compScale[cx])
                
                complexity_deviation[t] <- Comp[t] / Comp[t-1] 
                
                carrying_capacity_complex[t] <- calculate_carrying_capacity_complex(carrying_capacity, 
                                                                                    complexity_deviation[t])
                
                population[t,1] <- calculate_population_growth(population[t-1,1], 
                                                               intrinsic_growth_rate, 
                                                               carrying_capacity_complex[t])
                
                fraction_harvested[t, 1] <- calculate_fraction_harvested(fishing_effort[1], 
                                                                         catchability)
                harvest[t, 1] <- calculate_fisheries_harvest(population[t, 1], 
                                                             fraction_harvested[t, 1], 
                                                             FishParams$patch_area_m2[1]) 
                
                population[t,1] <- population[t,1] - (population[t,1] * Disturb[d])
                #escapement
                population[t, 1] <- calculate_escaped_stock_biomass(population[t, 1], 
                                                                    fraction_harvested[t, 1])
                fbm[t] <- calculate_fishbiomort(FishParams$fishBio_slp,
                                                population[t, 1])[1]
                #calculate market equivalent revenue
                revenue <- (price * harvest) * value_added_ratio
              }
              
              # simulating a disease outbreak
              
              if(dis == 3){
                Ps$fishbiomort = fbm[t-1]
                IPM  <- IPMFunc(Ps,SENS = FALSE)
                SSDs <- IPM$StableSizeDist
                sizeclasses <- IPM$y
                IPMssds[,t] <- SSDs
                IPMlams[t] <- IPM$Lambda
                IPMlams[t] <- IPMlams[t] - (IPMlams[t] * Disturb[d])
                
                Comp[t] <- ComplexityV2(IPMlams[t], SSDs, sizeclasses, 
                                        mean=compMean[cx], scale=compScale[cx])
                
                complexity_deviation[t] <- Comp[t] / Comp[t-1] 
                
                carrying_capacity_complex[t] <- calculate_carrying_capacity_complex(carrying_capacity, 
                                                                                    complexity_deviation[t])
                
                population[t,1] <- calculate_population_growth(population[t-1,1], 
                                                               intrinsic_growth_rate, 
                                                               carrying_capacity_complex[t])
                
                fraction_harvested[t, 1] <- calculate_fraction_harvested(fishing_effort[1], 
                                                                         catchability)
                harvest[t, 1] <- calculate_fisheries_harvest(population[t, 1], 
                                                             fraction_harvested[t, 1], 
                                                             FishParams$patch_area_m2[1]) 
                
                #escapement
                population[t, 1] <- calculate_escaped_stock_biomass(population[t, 1], 
                                                                    fraction_harvested[t, 1])
                fbm[t] <- calculate_fishbiomort(FishParams$fishBio_slp,
                                                population[t, 1])[1]
                #calculate market equivalent revenue
                revenue <- (price * harvest) * value_added_ratio}
            }
            
            
            # attempting to simulate a bleaching event
            else{
              for(i in 1:number_patches){
                #population growth
                #new IF statement so that if area is zero in a patch population is zero
                if (FishParams$patch_area[i] > 0) { 
                  Ps$fishbiomort = fbm[t-1]
                  IPM  <- IPMFunc(Ps,SENS = FALSE)
                  SSDs <- IPM$StableSizeDist
                  sizeclasses <- IPM$y
                  IPMssds[,t] <- SSDs
                  IPMlams[t] <- IPM$Lambda
                  
                  Comp[t] <- ComplexityV2(IPMlams[t], SSDs, sizeclasses, 
                                          mean= compMean[cx], scale=compScale[cx])
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
              fbm[t] <- calculate_fishbiomort(FishParams$fishBio_slp,
                                              population[t, 1])[1]
              #calculate market equivalent revenue
              revenue <- (price * harvest) * value_added_ratio
              # after the above population dynamics run for time t 
              # and each patch, the next timestep (t+1) will proceed using values from time t
            }
          }
          
          FullModel <- list(FishPopulation=population,
                            Harvest=harvest,
                            FishMort=fbm,
                            rawComplex=Comp,
                            dComplex=complexity_deviation,
                            Lambdas=IPMlams)
          
          name <- paste0(DistType[dis],"_",
                         as.character(Disturb[d]),"_",ComplexVer[cx],".rdata")
          
          save(FullModel, file=paste0(Dir,name))
          
          
        }
      }
      
    }
    
    
  }
}

#### Complexity Simulations
## here we are building and running the simulations with the different comp curves

### No Burn in!
# 3 curve versions, 10 (fishing pressure) x 10 (rec values)
# Will save the models into a folder to deal with later
# make figures out of!












