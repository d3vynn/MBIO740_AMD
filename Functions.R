############################ Model Functions ###################################
################## 1. Coral Integral Projection Model ##########################
#### Model assumptions:
##     - no spatial differences
##     - single coral species 
##     - no stock recruitment relationship (open recruitment) 
##     - yearly time steps
##     - fish biomass results in a background mortality term
##     - piece-wise recruitment with a uniform input of individuals > min size

#### Useful functions ####
inv.logit <- function(x){exp(x)/(1 + exp(x))}
logit     <- function(x){log(x/(1-x))}

#### Growth Function ####
# Dornelas et al 2017 
AllometricGrowth <- function(y, x, g.int, g.slp, g.var){
  mu   <- (g.int + g.slp * x)
  sig  <- sqrt(g.var)
  grow <- dnorm(y, mean=mu, sd=sig)
  return(grow)}

#### Survival Function ####
# Quadratic formulation for survival: Madin et al 2014 Figure 2 & Table 1.
Survival <- function(x, s.int, s.slp, s.slp2, fishbiomort){
  surv <- inv.logit(s.int + s.slp * x + s.slp2 * x^2) * (1 - fishbiomort)
  return(surv)}

#### Recruitment Function ####
OpenRecruitment <- function(y, x, rec_val, rec.size, fishbiomort){
  sizes      <- which(x <= rec.size)
  out        <- rep(0, length(x))
  out[sizes] <- rec_val * (1 - fishbiomort)
  return(out)}

#### Integral Projection Model ####
IPMFunc <- function(Parameters, SENS=TRUE){
  
  min.size <- Parameters$min.size
  max.size <- Parameters$max.size
  rec.size <- Parameters$rec.size
  n        <- Parameters$n
  g.int    <- Parameters$g.int
  g.slp    <- Parameters$g.slp
  g.var    <- Parameters$g.var
  s.int    <- Parameters$s.int
  s.slp    <- Parameters$s.slp
  s.slp2   <- Parameters$s.slp2
  fishbiom <- Parameters$fishbiomort
  rec_val  <- Parameters$rec_val
  
  bin.size <- min.size + c(0:n) * (max.size - min.size)/n
  y        <- 0.5 * (bin.size[1:n] +bin.size[2:(n+1)])
  ds       <- y[2] - y[1] # size of the bin step siz
  
  G <- ds * outer(y, y, 
                  AllometricGrowth, 
                  g.int=g.int, 
                  g.slp=g.slp, 
                  g.var=g.var)
  
  G <- t(t(G) / apply(G, 2, sum))
  
  S <- Survival(x=y, 
                s.int=s.int, 
                s.slp=s.slp, 
                s.slp2=s.slp2,
                fishbiomort=fishbiom)
  
  R <- ds * outer(y, y, 
                  OpenRecruitment, 
                  rec_val=rec_val, 
                  rec.size=rec.size, 
                  fishbiomort=fishbiom)
  P <- G
  
  for(i in 1:n){
    P[,i] <- G[,i] * S[i]}
  
  K   <- P + R
  lam <- Re(eigen(K)$values[1])
  w   <- Re(eigen(K)$vectors[,1])
  ssD <- w/sum(w) # stable size distribution
  
  v.eigen     <- Re(eigen(t(K))$vectors[,1]) #left eigenvector
  reprodvalue <- v.eigen/v.eigen[1] #reproductive value
  
  if (SENS == TRUE){
# compute sensitivity and elasticity matrices using the eigenvectors and lambda
    v.dot.w <- sum(ssD * reprodvalue) * ds
    sens    <- outer(reprodvalue,ssD) / v.dot.w
    elas    <- matrix(as.vector(sens) * as.vector(K) / lam, nrow = n)
    
    return(list(K=K, Lambda=lam, StableSizeDist=ssD, y=y,
                ReproVec=reprodvalue, Sensitivity=sens, Elasticity=elas))}
  
  if (SENS == FALSE){
    return(list(K=K, Lambda=lam, StableSizeDist=ssD,
                ReproVec=reprodvalue, y=y))
  }
}

################################################################################
################## 2. Benthic Complexity Connection ############################
###### Assumptions:
##      - Depends on two parameters to make functions like log, logistic, exp


## Functional form for the complexity-size relationships
CDFs <- function(x, mean, scale){
  CDF <- 1 / (exp(-(x-mean)/ scale) + 1)
  return(CDF)}


ComplexityV2 <- function(Lambda, StableSizeDistibution, 
                         SizeClasses, mean, scale){
  sizespecific_comp <- CDFs(10^SizeClasses, mean, scale)
  Comp_full         <- (Lambda * StableSizeDistibution) * sizespecific_comp
  complexity        <- sum(Comp_full)
  return(complexity)}

################################################################################
################# 3. Fish Biomass and Harvest Model ############################
#### Complexity Function ####
##       - carrying capacity is changed by the difference in complexity
 # Do we need this??

calculate_carrying_capacity_complex <- function(carrying_capacity, 
                                                complexity_deviation){
  carrying_capacity_complex <- carrying_capacity * complexity_deviation
  return(carrying_capacity_complex)}

#### Fish biomass proportion mortality ####
calculate_fishbiomort <- function(Mortslope, population, Interc){
  fbm <- (Mortslope * population +  Interc)
  return(fbm)}

#### Population growth ####
##      - Ricker model 
##      - no temp dependent mortality

calculate_population_growth <- function(population, 
                                        intrinsic_growth_rate, 
                                        carrying_capacity_complex){
  population <- population * exp(intrinsic_growth_rate * 
                                  (1 - population / carrying_capacity_complex))
  return(population)}

#### Fraction of stock harvested ####
calculate_fraction_harvested <- function(fishing_effort, 
                                         catchability){
  fraction_harvested <- 1 - exp(-fishing_effort * catchability)
  return(fraction_harvested)}

#### Fisheries Harvest ####
calculate_fisheries_harvest <- function(population, 
                                        fraction_harvested, 
                                        patch_area_m2){
  harvest <- population * fraction_harvested * patch_area_m2
  return(harvest)}

#### Escaped stock biomass
calculate_escaped_stock_biomass <- function(population, fraction_harvested) {
  escaped_stock_biomass <- population * (1 - fraction_harvested)
  return(escaped_stock_biomass)}






################################################################################
