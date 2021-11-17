
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
# BurninPlt = base +
#   geom_line(data=Data, aes(x=Time, y=Population, color=StartingPop))
# print(BurninPlt)
# 1. Fish population over time
fishP = base +
  geom_line(data=Data, aes(x=Time, y=Population, 
                           color=StartingPop,
                           group=StartingPop),
            size= 1.5)+
  facet_wrap(facets = ~factor(HavestLevel, levels = c("Low", "Medium", "High")))+
  xlim(c(1,10))+
  labs(colour = "Lambda",
       y = "Population (biomass)",
       x = "Time (years)")

print(fishP)  
# 2. Fish harvest over time
fishH = base +
  geom_line(data=Data, aes(x=Time, y=Harvest, color=StartingPop, group=StartingPop))+
  facet_wrap(facets = ~factor(HavestLevel, levels = c("Low", "Medium", "High")))+
  labs(colour = "Lambda",
       y = "Harvest (biomass)",
       x = "Time (years)")
print(fishH) 
# 3. Coral lambdas over time
coralL = base +
  geom_line(data=Data, aes(x=Time, y=Lambdas, color=StartingPop, group=StartingPop))+
  facet_wrap(facets = ~factor(HavestLevel, levels = c("Low", "Medium", "High")))+
  geom_abline(slope = 0, intercept = 1)+
  labs(colour = "Lambda",
       y = "Coral Intrinsic Growth Rate (lambda)",
       x = "Time (years)")
print(coralL) 
# 4. Coral complexity over time

coralC = base +
  geom_line(data=Data, aes(x=Time, y=Complexity, color=StartingPop, group=StartingPop))+
  facet_wrap(facets = ~factor(HavestLevel, levels = c("Low", "Medium", "High")))+
  labs(colour = "Lambda",
       y = "Benthic Complexity (unitless)",
       x = "Time (years)")
print(coralC) 

coraldC = base +
  geom_line(data=Data, aes(x=Time, y=ChgComplex, color=StartingPop, group=StartingPop))+
  facet_wrap(facets = ~factor(HavestLevel, levels = c("Low", "Medium", "High")))+
  labs(colour = "Lambda",
       y = "delta Benthic Complexity (unitless)",
       x = "Time (years)")
print(coraldC) 

# Interaction between models: plotting

coraldC_lam = base +
  geom_line(data=Data, aes(y=Lambdas, x=Complexity, color=StartingPop, group=StartingPop))+
  facet_wrap(facets = ~factor(HavestLevel, levels = c("Low", "Medium", "High")))+
  labs(colour = "Lambda",
       x = "Benthic Complexity (unitless)",
       y = "Intrinsic PopulationGrowth Rate")
print(coraldC_lam) 


coraldC_fish = base +
  geom_point(data=Data, aes(y=Population, x=ChgComplex, color=StartingPop, group=StartingPop))+
  facet_wrap(facets = ~factor(HavestLevel, levels = c("Low", "Medium", "High")))+
  labs(colour = "Lambda",
       x = "delta Benthic Complexity (unitless)",
       y = "Fish Population")
print(coraldC_fish) 

coralL_fish = base +
  geom_point(data=Data, aes(x=Population, y=Lambdas, color=StartingPop, group=StartingPop))+
  facet_wrap(facets = ~factor(HavestLevel, levels = c("Low", "Medium", "High")))+
  labs(colour = "Lambda",
       y = "Lambdas",
       x = "Fish Population")
print(coralL_fish) 

coralL_har = base +
  geom_point(data=Data, aes(x=Harvest, y=Lambdas, color=StartingPop, group=StartingPop))+
  facet_wrap(facets = ~factor(HavestLevel, levels = c("Low", "Medium", "High")))+
  labs(colour = "Lambda",
       y = "Lambdas",
       x = "Fish Harvest")
print(coralL_har) 


