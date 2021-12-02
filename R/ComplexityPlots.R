library(ggplot2)
library(latex2exp)
library(stringr)

########### Isolating Comp data & adding stuff

BaseD <- "C:\\Users\\Mollie.Asbury\\GitHub\\MBIO740_AMD\\output\\"
IntCp <- c("LowIntComp","IntIntComp")
SimTy <- c("ComplexitySims", "DisturbanceSims")
s <- 1

dataf <- data.frame()
for (c in 1:2){
  Dir <-  paste0(BaseD, SimTy[s], "\\", IntCp[c], "\\")
  ls  <- list.files(Dir)
  for (l in 1:length(ls)){
    load(paste0(Dir,ls[l]))
    name <- str_split(ls[l], pattern = "_")[[1]]
    
    time  <- seq(100)
    
    #fpMod <- lm(FullModel$FishPopulation[3:20,1] ~ time[3:20])
    #fhMod <- lm(FullModel$Harvest[3:20,1] ~ time[3:20])
    #lMod  <- lm(FullModel$Lambdas[3:20] ~ time[3:20])
    cMod  <- lm(FullModel$rawComplex[3:20] ~ time[3:20])
    
    #fishPopSlp <- coef(fpMod)[[2]]
    #fishPopFin <- FullModel$FishPopulation[100,1]
    #harvestSlp <- coef(fhMod)[[2]]
    #harvestFin <- FullModel$Harvest[100,1]
    #lambdaSlp  <- coef(lMod)[[2]]
    #lambdaFin  <- FullModel$Lambdas[100]
    complxSlp  <- coef(cMod)[[2]]
    complxFin  <- FullModel$rawComplex[100]
    compDevFin <- FullModel$dComplex[100]
    compDevVar <- sd(FullModel$dComplex)
    CompType   <- name[5]
    compDevMax <- max(FullModel$dComplex)
    
    if(length(name)  == 7){
      CompVers <- str_split(name[7], pattern = ".rdata")[[1]][1]
    }
    else{CompVers <- "V1"}
    
    
    ds <- data.frame(#fishPopSlp=fishPopSlp,
      #fishPopFin=fishPopFin,
      #harvestSlp=harvestSlp,
      #harvestFin=harvestFin,
      #lambdaSlp=lambdaSlp,
      #lambdaFin=lambdaFin,
      complxSlp=complxSlp,
      complxFin=complxFin,
      compDevFin=compDevFin,
      compDevVar=compDevVar,
      compDevMax=compDevMax,
      #cpStartV=IntCp[c],
      CompType=CompType,
      CompVers=CompVers,
      RecVal=as.factor(name[3]),
      FishingEffrt=as.factor(name[1]),
      IntComp=IntCp[c])
    
    dataf <- rbind(dataf, ds)
  }
}

ns <- paste0(SimTy[s], "_heatmapCompData_MA.rdata")
save(dataf, file=paste0(BaseD,ns))


########### Functions
CDFs <- function(x, mean, scale){
  CDF <- 1 / (exp(-(x-mean)/ scale) + 1)
  return(CDF)
}
compMean   <- c(7,50,90)
compScale  <- c(7,10,20)

size <- read.csv("SizeClasses.csv")
size <- size[-c(1)]

data1 <- CDFs(10^size, compMean[1], compScale[1])
plot(sort(10^size$x), sort(data1$x), type = 'l')
data2 <- CDFs(10^size, compMean[2], compScale[2])
plot(sort(10^size$x), sort(data2$x), type = 'l')
data3 <- CDFs(10^size, compMean[3], compScale[3])

par(mfrow = c(1,1))
plot(sort(10^size$x), sort(data1$x), type = 'l', lwd = 2, col = "purple", ylim = c(0,1), xlim = c(0,95),
     xlab = "Size (cm)", ylab = "Complexity (unitless)")
lines(sort(10^size$x), sort(data2$x), type = 'l', lwd = 2, col = "red")
lines(sort(10^size$x), sort(data3$x), type = 'l', lwd = 2, col = "darkorange")

########### Load data 

BaseD <- "C:\\Users\\Mollie.Asbury\\GitHub\\MBIO740_AMD\\output\\"
ns    <- "ComplexitySims_heatmapCompData_MA.rdata"
load(file=paste0(BaseD,ns)) ### THIS IS ONLY V2 DATA

V2datsL <- dataf[dataf$IntComp == "LowIntComp",]
V2datsI <- dataf[dataf$IntComp == "IntIntComp",]
Rvals  <- unique(V2datsL$RecVal)

CompFun <- c("Exp", "Logit", "Log")
med.col <- c("orange","red","purple")
high.col <- c("dark orange","red","purple")

########### Complexity slope (t 0 to 20)

for (c in 1:length(CompFun)){
  data <- V2datsL[V2datsL$CompType== CompFun[c] ,]

  
  
  plot <- ggplot(data, aes(x=RecVal, y=FishingEffrt))+ 
    #geom_tile() + #Tiled
    geom_raster(aes(fill = complxSlp),interpolate = TRUE) + #Smooth
    stat_density() +
    scale_y_discrete(TeX("Fishing Pressure"),
                     limits=c("0","0.06", "0.11", "0.17",
                              "0.22", "0.28", "0.33","0.39",
                              "0.44", "0.5")) +
    scale_x_discrete(TeX("Coral Recruitment parameter"),
                     limits = c("1", "2.56", "4.11", "5.67", 
                                "7.22", "8.78", "10.33", "11.89", 
                                "13.44", "15")) +
    scale_fill_gradientn(TeX(""),
                         colours= c(low="white",
                                    middle = med.col[c],
                                    high= high.col[c]),
                         limits = c(min(V2datsL$complxSlp),max(V2datsL$complxSlp)))+
    coord_fixed()+
    guides(fill = guide_colourbar(barwidth = 1,
                                  barheight = 8)) +
    ggtitle(paste0(CompFun[c]))
  
  ggsave(paste0(BaseD, "PNGs\\Comp_MA\\", "LowIntComp.complxSlp.", CompFun[c], ".tiled.png"), width = 6, height = 6, units = "in", dpi = 300)
}


for (c in 1:length(CompFun)){
  data <- V2datsI[V2datsI$CompType== CompFun[c] ,]
  
  plot <- ggplot(data, aes(x=RecVal, y=FishingEffrt, fill = complxSlp))+ #
    geom_tile() + #Tiled
    #geom_raster(aes(fill = complxSlp), interpolate = TRUE) + #Smooth
    scale_y_discrete(TeX("Fishing Pressure"),
                     limits=c("0","0.06", "0.11", "0.17",
                              "0.22", "0.28", "0.33","0.39",
                              "0.44", "0.5")) +
    scale_x_discrete(TeX("Coral Recruitment parameter"),
                     limits = c("1", "2.56", "4.11", "5.67", 
                                "7.22", "8.78", "10.33", "11.89", 
                                "13.44", "15")) +
    scale_fill_gradientn(TeX(""),
                         colours= c(low="white",
                                    middle = med.col[c],
                                    high= high.col[c]),
                         limits = c(min(V2datsI$complxSlp),max(V2datsI$complxSlp)))+
    coord_fixed()+
    guides(fill = guide_colourbar(barwidth = 1,
                                  barheight = 8)) +
    ggtitle(paste0(CompFun[c]))
  
  ggsave(paste0(BaseD, "PNGs\\Comp_MA\\", "IntIntComp.complxSlp.", CompFun[c], ".tiled.png"), width = 6, height = 6, units = "in", dpi = 300)
}

########### max Complexity deviation 

for (c in 1:length(CompFun)){
  data <- V2datsL[V2datsL$CompType== CompFun[c] ,]
  
  plot <- ggplot(data, aes(x=RecVal, y=FishingEffrt, fill = compDevMax))+ 
    #geom_tile() + #Tiled
    geom_raster(interpolate = TRUE) + #Smooth
    scale_y_discrete(TeX("Fishing Pressure"),
                     limits=c("0","0.06", "0.11", "0.17",
                              "0.22", "0.28", "0.33","0.39",
                              "0.44", "0.5")) +
    scale_x_discrete(TeX("Coral Recruitment parameter"),
                     limits = c("1", "2.56", "4.11", "5.67", 
                                "7.22", "8.78", "10.33", "11.89", 
                                "13.44", "15")) +
    scale_fill_gradientn(TeX(""),
                         colours= c(low="white",
                                    middle = med.col[c],
                                    high= high.col[c]),
                         limits = c(min(V2datsL$compDevMax),max(V2datsL$compDevMax)))+
    coord_fixed()+
    guides(fill = guide_colourbar(barwidth = 1,
                                  barheight = 8)) +
    ggtitle(paste0(CompFun[c]))
  
  ggsave(paste0(BaseD, "PNGs\\Comp_MA\\", "LowIntComp.compDevMax.", CompFun[c], ".smooth.png"), width = 6, height = 6, units = "in", dpi = 300)
}


for (c in 1:length(CompFun)){
  data <- V2datsI[V2datsI$CompType== CompFun[c] ,]
  
  plot <- ggplot(data, aes(x=RecVal, y=FishingEffrt, fill = compDevMax))+ 
    geom_tile() + #Tiled
    #geom_raster(interpolate = TRUE) + #Smooth
    scale_y_discrete(TeX("Fishing Pressure"),
                     limits=c("0","0.06", "0.11", "0.17",
                              "0.22", "0.28", "0.33","0.39",
                              "0.44", "0.5")) +
    scale_x_discrete(TeX("Coral Recruitment parameter"),
                     limits = c("1", "2.56", "4.11", "5.67", 
                                "7.22", "8.78", "10.33", "11.89", 
                                "13.44", "15")) +
    scale_fill_gradientn(TeX(""),
                         colours= c(low="white",
                                    middle = med.col[c],
                                    high= high.col[c]),
                         limits = c(min(V2datsI$compDevMax),max(V2datsI$compDevMax)))+
    coord_fixed()+
    guides(fill = guide_colourbar(barwidth = 1,
                                  barheight = 8)) +
    ggtitle(paste0(CompFun[c]))
  
  ggsave(paste0(BaseD, "PNGs\\Comp_MA\\", "IntIntComp.compDevMax.", CompFun[c], ".tiled.png"), width = 6, height = 6, units = "in", dpi = 300)
}

########### variation Complexity deviation 

for (c in 1:length(CompFun)){
  data <- V2datsL[V2datsL$CompType== CompFun[c] ,]
  
  plot <- ggplot(data, aes(x=RecVal, y=FishingEffrt, fill = compDevVar))+ 
    geom_tile() + #Tiled
    #geom_raster(interpolate = TRUE) + #Smooth
    scale_y_discrete(TeX("Fishing Pressure"),
                     limits=c("0","0.06", "0.11", "0.17",
                              "0.22", "0.28", "0.33","0.39",
                              "0.44", "0.5")) +
    scale_x_discrete(TeX("Coral Recruitment parameter"),
                     limits = c("1", "2.56", "4.11", "5.67", 
                                "7.22", "8.78", "10.33", "11.89", 
                                "13.44", "15")) +
    scale_fill_gradientn(TeX(""),
                         colours= c(low="white",
                                    middle = med.col[c],
                                    high= high.col[c]),
                         limits = c(min(V2datsL$compDevVar),max(V2datsL$compDevVar)))+
    coord_fixed()+
    guides(fill = guide_colourbar(barwidth = 1,
                                  barheight = 8)) +
    ggtitle(paste0(CompFun[c]))
  
  ggsave(paste0(BaseD, "PNGs\\Comp_MA\\", "LowIntComp.compDevVar.", CompFun[c], ".tiled.png"), width = 6, height = 6, units = "in", dpi = 300)
}


for (c in 1:length(CompFun)){
  data <- V2datsI[V2datsI$CompType== CompFun[c] ,]
  
  plot <- ggplot(data, aes(x=RecVal, y=FishingEffrt, fill = compDevVar))+ 
    geom_tile() + #Tiled
    #geom_raster(interpolate = TRUE) + #Smooth
    scale_y_discrete(TeX("Fishing Pressure"),
                     limits=c("0","0.06", "0.11", "0.17",
                              "0.22", "0.28", "0.33","0.39",
                              "0.44", "0.5")) +
    scale_x_discrete(TeX("Coral Recruitment parameter"),
                     limits = c("1", "2.56", "4.11", "5.67", 
                                "7.22", "8.78", "10.33", "11.89", 
                                "13.44", "15")) +
    scale_fill_gradientn(TeX(""),
                         colours= c(low="white",
                                    middle = med.col[c],
                                    high= high.col[c]),
                         limits = c(min(V2datsI$compDevVar),max(V2datsI$compDevVar)))+
    coord_fixed()+
    guides(fill = guide_colourbar(barwidth = 1,
                                  barheight = 8)) +
    ggtitle(paste0(CompFun[c]))
  
  ggsave(paste0(BaseD, "PNGs\\Comp_MA\\", "IntIntComp.compDevVar.", CompFun[c], ".tiled.png"), width = 6, height = 6, units = "in", dpi = 300)
}


########### final Complexity

for (c in 1:length(CompFun)){
  data <- V2datsL[V2datsL$CompType== CompFun[c] ,]
  
  plot <- ggplot(data, aes(x=RecVal, y=FishingEffrt, fill = complxFin))+ 
    #geom_tile() + #Tiled
    geom_raster(interpolate = TRUE) + #Smooth
    scale_y_discrete(TeX("Fishing Pressure"),
                     limits=c("0","0.06", "0.11", "0.17",
                              "0.22", "0.28", "0.33","0.39",
                              "0.44", "0.5")) +
    scale_x_discrete(TeX("Coral Recruitment parameter"),
                     limits = c("1", "2.56", "4.11", "5.67", 
                                "7.22", "8.78", "10.33", "11.89", 
                                "13.44", "15")) +
    scale_fill_gradientn(TeX(""),
                         colours= c(low="white",
                                    middle = med.col[c],
                                    high= high.col[c]),
                         limits = c(min(V2datsL$complxFin),max(V2datsL$complxFin)))+
    coord_fixed()+
    guides(fill = guide_colourbar(barwidth = 1,
                                  barheight = 8)) +
    ggtitle(paste0(CompFun[c]))
  
  ggsave(paste0(BaseD, "PNGs\\Comp_MA\\", "LowIntComp.complxFin.", CompFun[c], ".smooth.png"), width = 6, height = 6, units = "in", dpi = 300)
}


for (c in 1:length(CompFun)){
  data <- V2datsI[V2datsI$CompType== CompFun[c] ,]
  
  plot <- ggplot(data, aes(x=RecVal, y=FishingEffrt, fill = complxFin))+ 
    #geom_tile() + #Tiled
    geom_raster(interpolate = TRUE) + #Smooth
    scale_y_discrete(TeX("Fishing Pressure"),
                     limits=c("0","0.06", "0.11", "0.17",
                              "0.22", "0.28", "0.33","0.39",
                              "0.44", "0.5")) +
    scale_x_discrete(TeX("Coral Recruitment parameter"),
                     limits = c("1", "2.56", "4.11", "5.67", 
                                "7.22", "8.78", "10.33", "11.89", 
                                "13.44", "15")) +
    scale_fill_gradientn(TeX(""),
                         colours= c(low="white",
                                    middle = med.col[c],
                                    high= high.col[c]),
                         limits = c(min(V2datsI$complxFin),max(V2datsI$complxFin)))+
    coord_fixed()+
    guides(fill = guide_colourbar(barwidth = 1,
                                  barheight = 8)) +
    ggtitle(paste0(CompFun[c]))
  
  ggsave(paste0(BaseD, "PNGs\\Comp_MA\\", "IntIntComp.complxFin.", CompFun[c], ".smooth.png"), width = 6, height = 6, units = "in", dpi = 300)
}


