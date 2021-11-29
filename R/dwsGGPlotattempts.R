# Heat Map plots
library(ggplot2)
library(latex2exp)
library(stringr)

BaseD <- "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\"
IntCp <- c("LowIntComp","IntIntComp")
c <- 2
Dir   <-  paste0( BaseD, "ComplexitySims\\", IntCp[c], "\\")
ls    <- list.files(Dir)

### Combine data into a loooooong dataframe
# to then use for ggplots
dataf <- data.frame()
for (l in 1:length(ls)){
  load(paste0(Dir,ls[l]))
  name <- str_split(ls[l], pattern = "_")
  
  FishPopulation  <- FullModel$FishPopulation[,1]
  FishHarvest     <- FullModel$Harvest[,1]
  Lambdas         <- FullModel$Lambdas
  Complexity      <- FullModel$rawComplex
  Compdeviance    <- FullModel$dComplex
  CompType        <- rep(name[[1]][5], length.out=length(FishPopulation))
  IntRecVal       <- rep(name[[1]][3], length.out=length(FishPopulation))
  FishingPressure <- rep(name[[1]][1], length.out=length(FishPopulation))
  
  ds <- data.frame(FishPopulation=FishPopulation,
                   FishHarvest=FishHarvest,
                   Lambdas=Lambdas,
                   Complexity=Complexity,
                   Compdeviance=Compdeviance,
                   CompType=CompType,
                   IntRecVal=IntRecVal,
                   FishingPressure=FishingPressure)
  
  dataf <- rbind(dataf, ds)
}
ns <- paste0(IntCp[c], "_longData_CompSims.rdata")
save(dataf, file=paste0(BaseD,ns))


################################################################################





PlotName = ggplot(DATAName, aes(x=, y=, fill=))+
  geom_tile(color = "black")+
  scale_x_discrete(TeX(""),
                   limits=c()) +
  scale_y_discrete(TeX("")) +
  scale_fill_gradientn(TeX(""),
                       colours= c(low="blue",
                                  middle = "orange",
                                  high="red"),
                       limits = c(-10,10))+
  labs(caption = paste0(Temps[t], " ", Specs[s]))+
  coord_fixed()+
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 25))
