
Dir <- "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\Another\\"
ls <- list.files(Dir)
#ls <- ls[2:length(ls)]
library(stringr)
library(ggplot2)

# Getting the data into a format useful for ggplot2
# Burn <- data.frame()
# for(l in 1:length(ls)){
#   load(paste0(Dir,ls[l]))
#   string <- str_split(ls[l], pattern="_")
#   
#   df <- data.frame(Population=Burnin$FishPopulation[,1],
#                    Harvest=Burnin$Harvest[,1],
#                    Lambdas=Burnin$Lambdas,
#                    Complexity=Burnin$rawComplex,
#                    ChgComplex=Burnin$dComplex,
#                    FishEffect=Burnin$FishMort,
#                    HavestLevel=rep(string[[1]][1],length=50),
#                    StartingPop=rep(string[[1]][2],length=50),
#                    Time=seq(1,50,1))
#   
#   Burn <- rbind(Burn, df)
# }
# 
# save(Burn, file = paste0(Dir,"Burnin.rdata"))
# Getting the data into a format useful for ggplot2
Data <- data.frame()
for(l in 1:length(ls)){
  load(paste0(Dir,ls[l]))
  string <- str_split(ls[l], pattern="_")

  df <- data.frame(Population=FullModel$FishPopulation[,1],
                   Harvest=FullModel$Harvest[,1],
                   Lambdas=FullModel$Lambdas,
                   Complexity=FullModel$rawComplex,
                   ChgComplex=FullModel$dComplex,
                   FishEffect=FullModel$FishMort,
                   HavestLevel=rep(string[[1]][1],length=50),
                   StartingPop=rep(string[[1]][3],length=50),
                   Time=seq(1,50,1))

  Data <- rbind(Data, df)
}
# save(Data,file="")
PNGD <- "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\PNGs\\" # PNG DIRC
Data <- Data
# For each fishing effort scenario:
# Would have 3 lines on each plot (high, stable, low initial population)
base = ggplot(Data)+
  theme_bw()+
  theme(axis.line.x.top = element_line(color="black"),
        panel.grid = element_blank(),
        strip.background = element_rect(colour="white",
                                        fill="lavenderblush2"),
        plot.title.position = "plot",
        aspect.ratio = 1.2)

 # BurninPltF = base +
 #   geom_line(data=Data, aes(x=Time, y=Population, 
 #                            color=StartingPop),
 #             size=1.2)+
 #   labs(colour = "Lambda",
 #        y = "Fish Population (biomass)",
 #        x = "")
 # #print(BurninPltF)
 # 
 # BurninPltL = base +
 #   geom_line(data=Data, aes(x=Time, y=Lambdas, 
 #                            color=StartingPop),
 #             size=1.2)+
 #   labs(colour = "Lambda",
 #        y = "Coral Population Growth Rate (lambda)",
 #        x = "Time (years)")
 # 
 # #print(BurninPltL)
 # 
 # BurninPltC = base +
 #   geom_line(data=Data, aes(x=Time, y=Complexity, 
 #                            color=StartingPop),
 #             size=1.2)+
 #   labs(colour = "Lambda",
 #        y = "Coral Compexity (unitless)",
 #        x = "")
 # #print(BurninPltC)
 # 
 # BurninPltdC = base +
 #   geom_line(data=Data, aes(x=Time, y=ChgComplex, 
 #                            color=StartingPop),
 #             size=1.2)+
 #   labs(colour = "Lambda",
 #        y = "delta Compexity (unitless)",
 #        x = "")
 # #print(BurninPltdC)
 # 
 # BurnPs <- list(BurninPltF,
 #                BurninPltL,
 #                BurninPltC,
 #                BurninPltdC)
 # 
 # NamesB <- c("FishBurn",
 #             "LamBurn",
 #             "CompBurn",
 #             "dCompBurn")
 # 
 # for (i in 1:length(NamesB)){
 #   ggsave(filename = paste0(NamesB[i], ".png"),
 #          plot = BurnPs[[i]],
 #          width = 10,
 #          height = 5)
 # }
 
 
fc <- c("Increasing", "Stable", "Declining")  # c("Low", "Medium", "High")
# 1. Fish population over time
fishP = base +
  geom_line(data=Data, aes(x=Time, y=Population, 
                           color=HavestLevel,
                           group=HavestLevel),
            size= 1.2)+
  facet_wrap(facets = ~factor(StartingPop, 
                              levels = fc))+
  xlim(c(1,50))+
  labs(colour = "Harvest Level",
       y = "Fish Population (biomass)",
       x = "Time (years)")

print(fishP)  


# 2. Fish harvest over time
fishH = base +
  geom_line(data=Data, aes(x=Time, y=Harvest, 
                           color=HavestLevel, 
                           group=HavestLevel),
            size= 1.2)+
  facet_wrap(facets = ~factor(StartingPop, 
                              levels = fc))+
  labs(colour = "Harvest Level",
       y = "Fish Harvest (biomass)",
       x = "Time (years)")
print(fishH) 

# 3. Coral lambdas over time
coralL = base +
  geom_line(data=Data, aes(x=Time, y=Lambdas, 
                           color=HavestLevel, 
                           group=HavestLevel),
            size= 1.2)+
  facet_wrap(facets = ~factor(StartingPop, 
                              levels = fc))+
  geom_abline(slope = 0, intercept = 1)+
  labs(colour = "Harvest Level",
       y = "Coral Population Growth Rate (lambda)",
       x = "Time (years)")
#print(coralL) 
# 4. Coral complexity over time

coralC = base +
  geom_line(data=Data, aes(x=Time, y=Complexity, 
                           color=HavestLevel, 
                           group=HavestLevel),
            size = 1.2)+
  facet_wrap(facets = ~factor(StartingPop, 
                              levels = fc))+
  labs(colour = "Harvest Level",
       y = "Benthic Complexity (unitless)",
       x = "Time (years)")
#print(coralC) 

coraldC = base +
  geom_line(data=Data, aes(x=Time, y=ChgComplex, 
                           color=HavestLevel, 
                           group=HavestLevel),
            size= 1.2)+
  facet_wrap(facets = ~factor(StartingPop,
                              levels = fc))+
  labs(colour = "Harvest Level",
       y = "delta Benthic Complexity (unitless)",
       x = "Time (years)")
#print(coraldC) 

# Interaction between models: plotting

coraldC_lam = base +
  geom_line(data=Data, aes(x=Lambdas, y=Complexity, 
                           color=HavestLevel, 
                           group=HavestLevel),
            size= 1.2)+
  facet_wrap(facets = ~factor(StartingPop, 
                              levels = fc))+
  labs(colour = "Harvest Level",
       y = "Benthic Complexity (unitless)",
       x = "Coral Population Growth Rate (lambda)")
#print(coraldC_lam) 


coraldC_fish = base +
  geom_line(data=Data, aes(y=Population, x=ChgComplex,
                            color=HavestLevel,
                            group=HavestLevel),
             size= 1.2)+
  facet_wrap(facets = ~factor(StartingPop, 
                              levels = fc))+
  labs(colour = "Harvest Level",
       x = "delta Benthic Complexity (unitless)",
       y = "Fish Population (biomass)")
#print(coraldC_fish) 

coralL_fish = base +
  geom_line(data=Data, aes(x=Population, y=Lambdas, 
                            color=HavestLevel, 
                           group=HavestLevel),
            size= 1.2)+
  facet_wrap(facets = ~factor(StartingPop, 
                              levels = fc))+
  labs(colour = "Harvest Level",
       y = "Coral Population Growth Rate (lambda)",
       x = "Fish Population (biomass)")
#print(coralL_fish) 
coralL_har = base +
  geom_line(data=Data, aes(x=Harvest, y=Lambdas, 
                           color=HavestLevel, 
                           group=HavestLevel),
            size= 1.2)+
  facet_wrap(facets = ~factor(StartingPop, 
                              levels = fc))+
  labs(colour = "Lambda",
       y = "Coral Population Growth Rate (lambda)",
       x = "Fish Harvest (biomass)")
#print(coralL_har) 

Plots <- list(coralL_har,
              coralL_fish,
              coraldC_fish,
              coraldC_lam,
              coraldC,
              coralC,
              coralL,
              fishH,
              fishP)

NamesP <- c("LamvsHar_2",
            "LamvsPop_2",
            "dComvsPop_2",
            "dComvsLam_2",
            "dComvsT_2",
            "ComvsT_2",
            "LamvsT_2",
            "HavvsT_2",
            "PopvsT_2")

for (i in 1:length(NamesP)){
  ggsave(filename = paste0(NamesP[i], ".png"),
         plot = Plots[[i]],
         width = 10,
         height = 5)
}

