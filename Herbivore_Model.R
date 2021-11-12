#####################################POP DY FUNCTIONS########################################################################


############NEW FUNCTION: Calculate Catchability based on complexity
#source('Complexity_Herbivore_Relationship.R')

#complexity will be change each year based on mollie's model
#new complexity - starting complexity
#complexity_deviation <- 0.05
carrying_capacity <- 342


calculate_carrying_capacity_complex <- function(carrying_capacity, complexity_deviation) {
  carrying_capacity_complex <- carrying_capacity * complexity_deviation
}


calculate_fishbiomort <- function(population) {
  fbm <- (-0.002 * population + 1)
}



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
patch_area <- 1 #MAKING IT NONSPATIAL - ALL ONE PATCH. NO PROTECTED AREA. so there isn't any dispersal happening
#number_patches <- length(patch_area) 
number_patches <- 1 


#patch area in m2
#reef area of 504km2 https://www.soest.hawaii.edu/pibhmc/cms/data-by-location/main-hawaiian-islands/oahu/
#subregion size 1600km2 https://en.wikipedia.org/wiki/Hawaii_water_resource_region
#not sure which to use. using reef area for now.
patch_area_m2 <- patch_area * 5.04e+8

#fishing
fishing_effort <- 0.1
catchability <- 1

#fish
intrinsic_growth_rate <- 0.3 #0.3 from fishbase

###Economic inputs
price <- 0.01448438 #price per g in 2021, calculated in seafoodprices.R script 
value_added_ratio <- 0.9  #from Grafeld et al. 2017



#####################################INITIALIZE STATE VARIABLES##########################################################
# State variables
population <- array(NA, dim = c(timesteps, number_patches))
#recruits <- array(NA, dim = c(timesteps, number_patches)) 
#recruits_dispersal <- array(NA, dim = c(timesteps, number_patches)) 

# set initial starting population in each patch
starting_population <- 150 #gm2
population[1] <- starting_population

#fisheries
fraction_harvested <- array(NA, dim = c(timesteps, number_patches))
harvest <- array(NA, dim = c(timesteps, number_patches))

#escaped biomass
escapement <- array(NA, dim = c(timesteps, number_patches))

