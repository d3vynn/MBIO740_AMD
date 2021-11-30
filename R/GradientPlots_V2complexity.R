################################################################################
# Heat Map plots
library(ggplot2)
library(latex2exp)

BaseD <- "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\"
ns    <- "ComplexitySims_meatmapData_fixed.rdata"
load(file=paste0(BaseD,ns))

# Version 1 vrs Version 2

#V2
V2dats  <- dataf[dataf$CompVers == "V2",]
V2datsL <- V2dats[V2dats$IntComp == "LowIntComp",]
Rvals  <- unique(V2datsL$RecVal)
Effvs  <- unique(V2dats$FishingEffrt)
CompT  <- unique(V2dats$CompType)


fishPopSLP = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=fishPopSlp))+
  geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX("$\\frac{dP}{dt}$ over first 20 years"),
                       colours= c(low="blue",
                                  middle = "orange",
                                  high="red"),
                       limits = c(-5,25))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 25))+
  facet_wrap(~CompType)

fishPopFIN = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=fishPopFin))+
  geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX("Fish Pop at $t=100$"),
                       colours= c(low="blue",
                                  middle = "orange",
                                  high="red"),
                       limits = c(0,400))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 25))+
  facet_wrap(~CompType)


harvestSlp = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=harvestSlp))+
  geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours= c(low="blue",
                                  middle = "orange",
                                  high="red"),
                       limits = c(-2,1))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 25))+
  facet_wrap(~CompType)

HarvestFIN = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=harvestFin))+
  geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours= c(low="blue",
                                  middle = "orange",
                                  high="red"),
                       limits = c(10^-10,10^2))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 25))+
  facet_wrap(~CompType)


lambdaSlp = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=lambdaSlp))+
  geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours= c(low="blue",
                                  middle = "orange",
                                  high="red"),
                       limits = c(-0.03,0.05))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 25))+
  facet_wrap(~CompType)

lambdaFin = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=lambdaFin))+
  geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours= c(low="blue",
                                  middle = "orange",
                                  high="red"),
                       limits = c(10^-10,4))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 25))+
  facet_wrap(~CompType)

complxSlp = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=complxSlp))+
  geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours= c(low="blue",
                                  middle = "orange",
                                  high="red"),
                       limits = c(-0.007,0.025))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 25))+
  facet_wrap(~CompType)

complxFin = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=complxFin))+
  geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours= c(low="blue",
                                  middle = "orange",
                                  high="red"),
                       limits = c(-10^-4,0.5))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 25))+
  facet_wrap(~CompType)

compDevVar = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=compDevVar))+
  geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours= c(low="blue",
                                  middle = "orange",
                                  high="red"),
                       limits = c(0,80))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 25))+
  facet_wrap(~CompType)


compDevFin = ggplot(V2datsL, aes(x=RecVal, y=FishingEffrt, fill=compDevFin))+
  geom_tile(color = "black")+
  scale_y_discrete(TeX("Fishing Pressure"),
                   limits=c("0","0.06", "0.11", "0.17",
                            "0.22", "0.28", "0.33","0.39",
                            "0.44", "0.5")) +
  scale_x_discrete(TeX("Coral Recruitment parameter"),
                   limits = c("1", "2.56", "4.11", "5.67", 
                              "7.22", "8.78", "10.33", "11.89", 
                              "13.44", "15")) +
  scale_fill_gradientn(TeX(""),
                       colours= c(low="blue",
                                  middle = "orange",
                                  high="red"),
                       limits = c(0.8,1.05))+
  #labs(caption =)+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 25))+
  facet_wrap(~CompType)
