# Coral Demographics Model 

#### Model assumptions, no spatial differences, single coral species (?), 
# open system (no stock recruitment relationship, 
# just getting yearly pulses of recruits. #####

#### Branching morphology
##### Complexity increase almost linearly with age 
# <- so we can get a measure from the stable size distribution..
#### Growth Function ####

AllometricGrowth <- function(y, x, g.int, g.slp, g.var){
  mu   <- (g.int + g.slp * x)
  sig  <- sqrt(g.var)
  grow <- dnorm(y, mean=mu, sd=sig)
  return(grow)}

#### Survival Function
###### fish abundance will go in as a background mortality term. 
# The fewer fish the higher the mortality term
# probably the background mortality should affect the smaller size classes more 
# than the larger (add a Poisson distribution or something like that)
# Fish biomass mortality is a single term that corresponds to a negative 
# correlation between coral mortality rate and biomass 

Survival <- function(x, s.int, s.slp, s.slp2, fishbiomort){
  surv <- inv.logit(s.int + s.slp * x + s.slp2 * x^2) * (1 - fishbiomort)
  return(surv)}

#### Recruitment Function
######## Recruitment values will be a mean # of individuals at a range of sizes 
# smaller than the recruitment size, will select from a 
# normal distribution will a constant variance!
## ^^^^ not what is done yet!

### Recruitment is a constant value for all size classes less than the recruit
# size, this term is then modified by the fish biomass mortality term to reduce 
# the number input

OpenRecruitment <- function(y, x, rec_val, rec.size, fishbiomort){
  sizes      <- which(x <= rec.size)
  out        <- rep(0, length(x))
  out[sizes] <- rec_val * (1 - fishbiomort)
  return(out)}

inv.logit <- function(x){
  exp(x)/(1 + exp(x))}

logit <- function(x){
  log(x/(1-x))
}
# Example input values
# n <- 100
# min.size <- -2.7  # (2 * sqrt(10^-2.7)/pi) # approx 5 cm diameter
# max.size <- 2  # and 11 m coral diameter
# rec.size <- -2.5 # approx 6 cm

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
  
  G <- ds * outer(y, y, AllometricGrowth, g.int=g.int, g.slp=g.slp, g.var=g.var)
  G <- t(t(G) / apply(G, 2, sum))
  S <- Survival(x=y, s.int=s.int, s.slp=s.slp, s.slp2=s.slp2,fishbiomort=fishbiom)
  R <- ds * outer(y, y, OpenRecruitment, rec_val=rec_val, 
                  rec.size=rec.size, fishbiomort=fishbiom)
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
    #compute sensitivity and elasticity matrices using the eigenvectors and eigenvalue
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
# Complexity from stable size distributions

Complexity <- function(Lambda, StableSizeDistibution, SizeClasses){
  
  sizespecific_comp <- inv.logit(SizeClasses)
  Comp_full         <- (Lambda * StableSizeDistibution) * sizespecific_comp
  complexity        <- sum(Comp_full)
  
}

Complexity.exp <- function(Lambda, StableSizeDistibution, SizeClasses){
  
  sizespecific_comp <- exp(SizeClasses)
  Comp_full         <- (Lambda * StableSizeDistibution) * sizespecific_comp
  complexity        <- sum(Comp_full)
  
}

Complexity.log <- function(Lambda, StableSizeDistibution, SizeClasses){
  
  sizespecific_comp <- log(SizeClasses)
  Comp_full         <- (Lambda * StableSizeDistibution) * sizespecific_comp
  complexity        <- sum(Comp_full)
  
}

