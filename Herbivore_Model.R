#####################################POP DY FUNCTIONS########################################################################


############NEW FUNCTION: Calculate Catchability based on complexity
source('Complexity_Herbivore_Relationship.R')

#complexity will be change each year based on mollie's model
complexity <- 0.05
carrying_capacity <- 342
carrying_capacity_complex <- carrying_capacity * (1 + complexity)






##everything else can probably stay the same

##Population growth - Ricker model - no temp dependent mortality
calculate_population_growth <- function(population, intrinsic_growth_rate, carrying_capacity_complex) {
  population <-population * exp(intrinsic_growth_rate * (1 - population / carrying_capacity_complex))
  return(population)
}

#Fraction of stock harvested
calculate_fraction_harvested <- function(fishing_effort, catchability) {
  fraction_harvested <- 1 - exp(-fishing_effort * catchability)
  return(fraction_harvested)
}

#Fisheries Harvest
calculate_fisheries_harvest <- function(population, fraction_harvested, patch_area_m2) {
  harvest <- population * fraction_harvested * patch_area_m2
  return(harvest)
}

#Escaped stock biomass
calculate_escaped_stock_biomass <- function(population, fraction_harvested) {
  escaped_stock_biomass <- population * (1 - fraction_harvested)
  return(escaped_stock_biomass)
}


############################DEFINE PARAMETERS#############################################################
##Time
timesteps <- 50 #years
##Area
patch_area <- c(1, 0) #MAKING IT NONSPATIAL - ALL ONE PATCH. NO PROTECTED AREA. so there isn't any dispersal happening
number_patches <- length(patch_area) 

#patch area in m2
#reef area of 504km2 https://www.soest.hawaii.edu/pibhmc/cms/data-by-location/main-hawaiian-islands/oahu/
#subregion size 1600km2 https://en.wikipedia.org/wiki/Hawaii_water_resource_region
#not sure which to use. using reef area for now.
patch_area_m2 <- patch_area * 5.04e+8

#fishing
fishing_effort <- c(0.13, 0)
catchability <- 1

#fish
intrinsic_growth_rate <- 0.3 #0.3 from fishbase

###Economic inputs
price <- 0.01448438 #price per g in 2021, calculated in seafoodprices.R script 
value_added_ratio <- 0.9  #from Grafeld et al. 2017

##dispersal
S <- 0.8 # site-fidelity parameter, value of 0 results in common pool dispersal and a value of 1 means no movement between patches (diagonal values in the matrix = 1)
S_recruit <- 0 # low site fidelity for recruits


#Adult dispersal
dispersal <- function(patch_area, number_patches, S) {
  
  # Create the common pool matrix
  D_common_pool <- matrix(rep(patch_area, number_patches), ncol = number_patches, byrow = TRUE) # common pool dispersal such that the movement is proportional to patch relative area
  
  require(ramify) # need this for matrix functions (e.g., triu)
  
  if(S > 0){
    tmp1 <- D_common_pool
    tmp1_triu <- triu(tmp1, 1) # set values in the upper triangular part of the matrix (in the 2x2 matrix this mean the cross patch movement)
    tmp1_tril <- tril(tmp1,-1) # set values in the lower triangular part of the matrix (in the 2x2 matrix this mean the cross patch movement)
    tmp1_offdiag <- tmp1_triu + tmp1_tril # Original off diagonal, or cross patch movement (used below)
  } else {
    return(D_common_pool)
  }
  
  for(D_row in 1:length(diag(tmp1))){ # Repeat for each patch, taking the length of the diagonal is equivalent to the total number of patches
    if(tmp1[D_row,D_row] > 0){ # If that site has positive habitat area  
      if((sum(tmp1[D_row, ]) - tmp1[D_row, D_row]) > 0){ # and the cross recruit sites have positive habitat area, this should always be true for our case
        # calculate the difference between larval pool recruitment and 100% self-recruitment
        diff_self_pool <- 1 - tmp1[D_row, D_row] # the 1 being 100% self-recruitment
        # add enhanced site-fidelity to common pool
        enhanced_site_fidelity <- tmp1[D_row, D_row] + (diff_self_pool * S) # tmp1[D_row, D_row] indicates self recruitment to a patch 
        # Standardize the proportion of the off diagonals such that they sum to 1 (see offdiag above)
        tmp1_offdiag_row_standardized <- tmp1_offdiag[D_row, ] / sum(tmp1_offdiag[D_row, ])      
        # Subtract from the common pool cross-recruit probability the standardized diff*proportion enhancement
        reduced_cross_recruit <- tmp1_offdiag[D_row, ] - (tmp1_offdiag_row_standardized * (diff_self_pool*S))
        # Insert the new values
        tmp1[D_row, ] <- reduced_cross_recruit 
        tmp1[D_row, D_row] <- enhanced_site_fidelity
        
      }
    } 
  }
  
  return(tmp1) 
}

##JUVENILE DISPERSAL - different matrix for juveniles
dispersal_juvenile <- function(patch_area, number_patches, S_recruit) {
  if(S_recruit > 0){
    tmp2 <- D_common_pool
    tmp2_triu <- triu(tmp2, 1) # set values in the upper triangular part of the matrix (in the 2x2 matrix this mean the cross patch movement)
    tmp2_tril <- tril(tmp2,-1) # set values in the lower triangular part of the matrix (in the 2x2 matrix this mean the cross patch movement)
    tmp2_offdiag <- tmp2_triu + tmp2_tril # Original off diagonal, or cross patch movement (used below)
  } else {
    return(D_common_pool)
  }
  
  for(D_row in 1:length(diag(tmp2))){ # Repeat for each patch, taking the length of the diagonal is equivalent to the total number of patches
    if(tmp2[D_row,D_row] > 0){ # If that site has positive habitat area  
      if((sum(tmp2[D_row, ]) - tmp2[D_row, D_row]) > 0){ # and the cross recruit sites have positive habitat area, this should always be true for our case
        # calculate the difference between larval pool recruitment and 100% self-recruitment
        diff_self_pool <- 1 - tmp2[D_row, D_row] # the 1 being 100% self-recruitment
        # add enhanced site-fidelity to common pool
        enhanced_site_fidelity <- tmp2[D_row, D_row] + (diff_self_pool * S_recruit) # tmp1[D_row, D_row] indicates self recruitment to a patch 
        # Standardize the proportion of the off diagonals such that they sum to 1 (see offdiag above)
        tmp2_offdiag_row_standardized <- tmp2_offdiag[D_row, ] / sum(tmp2_offdiag[D_row, ])      
        # Subtract from the common pool cross-recruit probability the standardized diff*proportion enhancement
        reduced_cross_recruit <- tmp2_offdiag[D_row, ] - (tmp2_offdiag_row_standardized * (diff_self_pool*S_recruit))
        # Insert the new values
        tmp2[D_row, ] <- reduced_cross_recruit 
        tmp2[D_row, D_row] <- enhanced_site_fidelity
        
      }
    } 
  }
  
  return(tmp2) 
}

#####################################INITIALIZE STATE VARIABLES##########################################################
# State variables
population <- array(NA, dim = c(timesteps, number_patches))
recruits <- array(NA, dim = c(timesteps, number_patches)) 
recruits_dispersal <- array(NA, dim = c(timesteps, number_patches)) 

# set initial starting population in each patch
starting_population <- c(14,0) #14g/m2 median herbivore biomass value for Oahu - from Donovan et al. in prep
population[1, ] <- starting_population

#fisheries
fraction_harvested <- array(NA, dim = c(timesteps, number_patches))
harvest <- array(NA, dim = c(timesteps, number_patches))

#escaped biomass
escapement <- array(NA, dim = c(timesteps, number_patches))


###############################################MODEL CODE#################################################################
for(t in 2:timesteps){
  tmp1 <- dispersal(patch_area, number_patches, S)
  tmp2 <- dispersal(patch_area, number_patches, S_recruit)
  for(i in 1:number_patches){
    #population growth
    #new IF statement so that if area is zero in a patch population is zero
    if (patch_area[i] > 0) {
      population[t, i] <- calculate_population_growth(population[t-1, i], 
                                                      intrinsic_growth_rate, carrying_capacity_complex)
    } else {
      population[t, i] <- 0
    }
    
    #separate recruits from adults
    recruits[t, i] <- population[t, i] - population[t-1, i]
  }
  
  #recruit disperse
  recruits_dispersal[t,] <- recruits[t,] %*% tmp2
  
  for(i in 1:number_patches){
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
  
  #calculate market equivalent revenue
  revenue <- (price * harvest) * value_added_ratio
  # after the above population dynamics run for time t and each patch, the next timestep (t+1) will proceed using values from time t.
}
