################################################################################
# Heat Map plots
library(ggplot2)
library(latex2exp)

BaseD <- "~/Documents/GitHub/MBIO740_AMD/output/"
ns    <- "ComplexitySims_meatmapData_fixed.rdata"
load(file=paste0(BaseD,ns))

####
# plot 3 rec values, or each of the 10 rec values separately
# then bar plots of the lam slope for the fishing pressures

# should maybe also make sure to try one that is separated/binned with high middle, low
dataf2 <- dataf[dataf$CompType == "Logit",]

FP <- unique(dataf2$FishingEffrt)[1:10]
RV <- unique(dataf2$RecVal)

for(r in 1:10){
  a <- dataf2[dataf2$RecVal== RV[r],]
  b <- a[a$FishingEffrt == FP,]
  
  RV_allFP <- ggplot(b, aes(y=lambdaSlp,x=FishingEffrt,fill=lambdaSlp))+
              geom_col()+
              scale_x_discrete(TeX("Fishing Pressure"),
                               limits=c("0","0.06", "0.11", "0.17",
                                        "0.22", "0.28", "0.33","0.39",
                                        "0.44", "0.5")) +
              scale_fill_gradientn("",colours= c(low="#9909F7", #"#8B32E6"
                                                 middle = "#E6F30C",
                                                 high="#F70A0A"),
                                   limit=c(-0.027, 0.05))+
              ylim(c(-0.027,0.05))+
              ylab(TeX("Slope of Population Growth Rate $\\Delta\\lambda / \\Delta t$"))+
    # scale_x_discrete(TeX("Coral Recruitment parameter"),
    #                  limits = c("1", "2.56", "4.11", "5.67", 
    #                             "7.22", "8.78", "10.33", "11.89", 
    #                             "13.44", "15")) +
              theme_classic()
  
  ggsave(filename = paste0("SepByRV_rec", RV[r], ".png"),
         path = paste0(BaseD, "Figures/"),
         plot = RV_allFP,
         width = 5,
         height = 5)
  
  
  
}

for(f in 1:10){
  a <- dataf2[dataf2$FishingEffrt == FP[f],]
  
  FP_allRV <- ggplot(a, aes(y=(lambdaFin),x=RecVal,fill=(lambdaFin)))+
    geom_col()+
    scale_x_discrete(TeX("Coral Recruitment parameter"),
                     limits = c("1", "2.56", "4.11", "5.67",
                                "7.22", "8.78", "10.33", "11.89",
                                "13.44", "15")) +
    scale_fill_gradientn("",colours= c(low="#9909F7", #"#8B32E6"
                                       middle = "#E6F30C",
                                       high="#F70A0A"),
                         limits = c(10^-10,1.75))+
    
                         #limit=c(-0.027, 0.05))+
   #ylim(c(-0.5,2.00))+
    ylab(TeX("Final Population Growth Rate $\\lambda_{f}"))+

    theme_classic()
  
  ggsave(filename = paste0("SepByFP_FP", FP[f], "_finLam.png"),
         path = paste0(BaseD, "Figures/"),
         plot = FP_allRV,
         width = 5,
         height = 5)
  

}










# Version 1 vrs Version 2

#V2
V2dats  <- dataf[dataf$CompVers == "V2",]
V2datsL <- V2dats[V2dats$IntComp == "LowIntComp",]
V2datsL <- V2datsL[V2datsL$CompType=="Log",]
Rvals  <- unique(V2datsL$RecVal)
Effvs  <- unique(V2dats$FishingEffrt)
CompT  <- unique(V2dats$CompType)


fishPopSLP = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=fishPopSlp))+
  geom_raster(interpolate = TRUE)+ # color = "black"
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn("",#TeX("$\\frac{dP}{dt}$ over first 20 years"),
                       colours= c(low="#2128D0", #"#8B32E6"
                                  middle = "#CAC6C9",
                                  high="#FF7503"),
                       limits = c(-5,10))+

  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10))+
  # geom_contour()+
  theme_classic()


fishPopFIN = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=fishPopFin))+
  geom_raster(interpolate = TRUE)+ 
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX("Fish Pop at $t=100$"),
                       colours= c(low="#2128D0", #"#8B32E6"
                                  middle = "#CAC6C9",
                                  high="#FF7503"), #
                       limits = c(0,400))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10))+
  #facet_wrap(~CompType)
  theme_classic()


harvestSlp = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=harvestSlp))+
  geom_raster(interpolate = TRUE)+ 
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours=c(low="#2128D0", #"#8B32E6"
                                 middle = "#CAC6C9",
                                 high="#FF7503"),
                       limits = c(-2,1))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10))+
  #facet_wrap(~CompType)
  theme_classic()

HarvestFIN = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=harvestFin))+
  geom_raster(interpolate = TRUE)+ #geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours= c(low="#2128D0", #"#8B32E6"
                                  middle = "#CAC6C9",
                                  high="#FF7503"),
                       limits = c(10^-10,10^2))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10))+
  #facet_wrap(~CompType)
  theme_classic()


lambdaSlp = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=lambdaSlp))+
  geom_raster(interpolate = TRUE)+ #geom_tile(color = "black")+
  #geom_contour(aes(x=RecVal, y=FishingEffrt, z=lambdaSlp), color="white")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn( "", #TeX("$\\frac{d\\lambda}{dt}$"),
                       colours= c(low="#2128D0", #"#8B32E6"
                                  middle = "#CAC6C9",
                                  high="#FF7503"),
                       limits = c(-0.03,0.05))+
  #labs(caption =)+
  #coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10))+
  #facet_wrap(~CompType)
  theme_classic()

lambdaFin = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=lambdaFin))+
  geom_raster(interpolate = TRUE)+ #geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours=c(low="#2128D0", #"#8B32E6"
                                 middle = "#CAC6C9",
                                 high="#FF7503"),
                       limits = c(10^-10,4))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10))+
  #facet_wrap(~CompType)
  theme_classic()

complxSlp = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=complxSlp))+
  geom_raster(interpolate = TRUE)+#geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours=c(low="#2128D0", #"#8B32E6"
                                 middle = "#CAC6C9",
                                 high="#FF7503"),
                       limits = c(-0.007,0.025))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10))+
  #facet_wrap(~CompType)
  theme_classic()

complxFin = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=complxFin))+
  geom_raster(interpolate = TRUE)+ #geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours=c(low="#2128D0", #"#8B32E6"
                                 middle = "#CAC6C9",
                                 high="#FF7503"),
                       limits = c(-10^-4,0.5))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10))+
  #facet_wrap(~CompType)
  theme_classic()

compDevVar = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=compDevVar))+
  geom_raster(interpolate = TRUE)+#geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours=c(low="#2128D0", #"#8B32E6"
                                 middle = "#CAC6C9",
                                 high="#FF7503"),
                       limits = c(0,80))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10))+
  #facet_wrap(~CompType)
  theme_classic()


compDevFin = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=compDevFin))+
  geom_raster(interpolate = TRUE)+ #geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours=c(low="#2128D0", #"#8B32E6"
                                 middle = "#CAC6C9",
                                 high="#FF7503"),
                       limits = c(0.8,1.05))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10))+
  #facet_wrap(~CompType)
  theme_classic()


Plots <- list(fishPopSLP,
              fishPopFIN,
              harvestSlp,
              HarvestFIN,
              lambdaSlp,
              lambdaFin,
              complxSlp,
              complxFin,
              compDevVar,
              compDevFin)

#print(Plots)

NamesP <- c("fishPopSLP",
            "fishPopFIN",
            "harvestSlp",
            "HarvestFIN",
            "lambdaSlp",
            "lambdaFin",
            "complxSlp",
            "complxFin",
            "compDevVar",
            "compDevFin")

for (i in 1:length(NamesP)){
  ggsave(filename = paste0(NamesP[i],"_log", ".png"),
         path = paste0(BaseD, "Figures\\"),
         plot = Plots[[i]],
         width = 5,
         height = 5)
}
