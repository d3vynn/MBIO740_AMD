source('Herbivore_Model.R')
source('CoralIPMs.R')
Dir <- "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\Burnin\\"
load(paste0(Dir,"Burnin.rdata"))

d <- Burn[Burn$StartingPop == "Declining",]
s <- Burn[Burn$StartingPop == "Stable",]
I <- Burn[Burn$StartingPop == "Increasing",]

# Fish Parameters
population[1]       <- median(Burn$Population)
starting_population <- median(Burn$Population)
fishing_effort      <- 0

# Coral Demographic Parameters
fishBiomort <- c(median(d$FishEffect),
                 median(s$FishEffect),
                 median(I$FishEffect))

lambdaInt <- c(d$Lambdas[50],
               s$Lambdas[50],
               I$Lambdas[50])

# Coral Complexity Parameters
ComplexityInt <- c(d$Complexity[50],
                   s$Complexity[50],
                   I$Complexity[50])

ChgComplexInt <- c(d$ChgComplex[50],
                   s$ChgComplex[50],
                   I$ChgComplex[50])

Ps <- list(g.int = -0.295459528,
           g.slp = 0.897642821,
           g.var = 0.148459262,
           s.int = 6.619306118,
           s.slp = 4.356206835,
           s.slp2 = 0.60252172,	
           rec.size =	-2.868638,
           min.size = -3,
           max.size = 2,
           n = 100,
           rec_val = 5,
           fishbiomort = 0.5)