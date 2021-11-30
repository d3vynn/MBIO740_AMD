
library(stringr)

BaseD <- "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\"
IntCp <- c("LowIntComp","IntIntComp")
SimTy <- c("ComplexitySims", "DisturbanceSims")
s <- 1



### Combine data into a loooooong dataframe
# to then use for ggplots
dataf <- data.frame()
for (c in 1:2){
  Dir <-  paste0(BaseD, SimTy[s], "\\", IntCp[c], "\\")
  ls  <- list.files(Dir)
for (l in 1:length(ls)){
  load(paste0(Dir,ls[l]))
  name <- str_split(ls[l], pattern = "_")[[1]]
  
  time  <- seq(100)
  
  fpMod <- lm(FullModel$FishPopulation[3:20,1] ~ time[3:20])
  fhMod <- lm(FullModel$Harvest[3:20,1] ~ time[3:20])
  lMod  <- lm(FullModel$Lambdas[3:20] ~ time[3:20])
  cMod  <- lm(FullModel$rawComplex[3:20] ~ time[3:20])
  
  fishPopSlp <- coef(fpMod)[[2]]
  fishPopFin <- FullModel$FishPopulation[100,1]
  harvestSlp <- coef(fhMod)[[2]]
  harvestFin <- FullModel$Harvest[100,1]
  lambdaSlp  <- coef(lMod)[[2]]
  lambdaFin  <- FullModel$Lambdas[100]
  complxSlp  <- coef(cMod)[[2]]
  complxFin  <- FullModel$rawComplex[100]
  compDevFin <- FullModel$dComplex[100]
  compDevVar <- sd(FullModel$dComplex)
  CompType   <- name[5]
  
  if(length(name)  == 7){
    CompVers <- str_split(name[7], pattern = ".rdata")[[1]][1]
  }
  else{CompVers <- "V1"}
  
  
  ds <- data.frame(fishPopSlp=fishPopSlp,
                   fishPopFin=fishPopFin,
                   harvestSlp=harvestSlp,
                   harvestFin=harvestFin,
                   lambdaSlp=lambdaSlp,
                   lambdaFin=lambdaFin,
                   complxSlp=complxSlp,
                   complxFin=complxFin,
                   compDevFin=compDevFin,
                   compDevVar=compDevVar,
                   cpStartV=IntCp[c],
                   CompType=CompType,
                   CompVers=CompVers,
                   RecVal=as.factor(name[3]),
                   FishingEffrt=as.factor(name[1]),
                   IntComp=IntCp[c])
  
  dataf <- rbind(dataf, ds)
}
}
ns <- paste0(SimTy[s], "_meatmapData_fixed.rdata")
save(dataf, file=paste0(BaseD,ns))

