library(ggplot2)
library(latex2exp)
library(stringr)

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

BaseD <- "~/Documents/GitHub/MegaModelCubed/"
ns    <- "output/data/RecvsFish/Simulations_results_041122.rdata"
load(file=paste0(BaseD,ns)) 

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


