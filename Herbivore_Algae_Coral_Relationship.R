##source:
#https://www.researchgate.net/profile/Su-Sponaugle/publication/225845782_Grazing_pressure_of_herbivorous_coral_reef_fishes_on_low_coral-cover_reefs/links/55c247f308aeb975673e3ff9/Grazing-pressure-of-herbivorous-coral-reef-fishes-on-low-coral-cover-reefs.pdf
#Figure 2b - herbivore biomass against fig 2f - algae percent cover
library(tidyverse)


#no relationship so dont think this will work
herbivore <- read.csv("Fish_Biomass.csv")
algae <- read.csv("Algae_Biomass.csv")

herbivore$algae <- algae$algae

fit <- lm(algae~fish, data = herbivore)
summary(fit)
ggplot(data=herbivore, aes(x=fish, y=algae)) + geom_point()
