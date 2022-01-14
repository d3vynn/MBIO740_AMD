library(stringr)

BaseD <- "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\"
IntCp <- c("LowIntComp","IntIntComp")[1]
SimTy <- c("ComplexitySims", "DisturbanceSims")[2]

dataf <- recovf <- data.frame()
Dir <-  paste0(BaseD, SimTy, "\\", IntCp, "\\")
ls  <- list.files(Dir)[1:90]
for (l in 1:length(ls)){
  a <- str_split(ls[l], pattern = ".rdata")[[1]][1]
  b <- str_split(a, pattern = "_")[[1]]

  load(paste0(Dir, ls[l]))
  
  # recovery steps, 
  # maybe instead should look at the number of steps it takes 
  # to return back to the state before disturbance
  
  ff <- which(round(FullModel$FishPopulation[49:100,1],1) >= 
          round(FullModel$FishPopulation[50,1],1))
  dd <- ff[2:length(ff)] - ff[1:(length(ff)-1)]
  FishRecv <- dd[which(dd > 1)]
  
  ff <- which(round(FullModel$Harvest[49:100,1],1) >= 
                round(FullModel$Harvest[50,1],1))
  dd <- ff[2:length(ff)] - ff[1:(length(ff)-1)]
  HarvRecv <- dd[which(dd > 1)]
  
  ff <- which(round(FullModel$Lambdas[49:100],3) >= 
                round(FullModel$Lambdas[50],3))
  dd <- ff[2:length(ff)] - ff[1:(length(ff)-1)]
  LmRecv  <- dd[which(dd > 1)]
  
  ff <- which(round(FullModel$rawComplex[49:100],3) >= 
                round(FullModel$rawComplex[50],3))
  dd <- ff[2:length(ff)] - ff[1:(length(ff)-1)]
  ComRecv  <- dd[which(dd > 1)]
  

  rs <- data.frame(DisturbanceType=b[1],
                   Magnitude=b[2],
                   ComplexType=b[3],
                   RecoveryTime=c(FishRecv,HarvRecv,LmRecv,ComRecv),
                   ResponseType=c("FishRecv","HarvRecv","LmRecv","ComRecv"))  
  
  recovf <- rbind(recovf,rs)
  
  ds <- data.frame(DisturbanceType=b[1],
                   Magnitude=b[2],
                   ComplexType=b[3],
                   FishPop=FullModel$FishPopulation[,1],
                   FishHav=FullModel$Harvest[,1],
                   Lambs=FullModel$Lambdas,
                   Complex=FullModel$rawComplex,
                   dComplex=FullModel$dComplex,
                   Time=seq(100))
  
  dataf <- rbind(dataf, ds)
  }

save(dataf, file=paste0(BaseD,"Disturbance_full_range_AllComplx.rdata"))
save(recovf, file=paste0(BaseD,"Disturbance_times_range_AllComplx.rdata"))
