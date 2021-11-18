library(tidyverse)
#data from Fig3a Fukunaga et al. 2020
data <- read.csv("wpd_datasets.csv")

#convert their y values of abundance to a grams value to be relevant to my gm2 units
#source: https://www.researchgate.net/publication/275273995_Estimating_Catch_Weight_of_Reef_Fish_Species_Using_Estimation_and_Intercept_Data_from_the_Hawaii_Marine_Recreational_Fishing_Survey
#three families" acanthuridae, kyphosidae, scaridae - surgeonfish, chubs, parrotfish
#took annual avg weight (table 1) and divided it by annual avg # harvested (table 4)
#surgeons: 0.5652
#chubs: 1.506
#parrots: 4.7057
#overall average: 2.259
#multiply y values by this: 1024.66516


#abundance / 176.714586764m2 = fish per m2 * average weight of fish = g/m2

fish_grams <- 1024.66516
area <- 176.714586764

data$Line_Y_Weight <- (data$Line_Y * fish_grams) / area

fit<-lm(log(Line_Y_Weight)~Line_X, data=data)
summary(fit)


f1 <- function (X, slope, int) {
  exp(int) * exp(slope*X)
}

Pred_Y <- f1(data$Line_X, slope=coef(fit)[2], coef(fit)[1])
data <- cbind(data, Pred_Y)

ggplot() + 
  geom_point(data=data, aes(x=Line_X, y=Line_Y_Weight)) +
  geom_line(data = data, aes(x=Line_X, y=Pred_Y))
