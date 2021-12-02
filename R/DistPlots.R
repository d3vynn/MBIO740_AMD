# Disturbance Plots
library(ggplot2)
library(stringr)
library(dplyr)
library(reshape2)

BaseD <- "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\"
ls    <- list.files(BaseD)

load(paste0(BaseD,ls[9]))
load(paste0(BaseD,ls[13]))

recovf[]


Recovery = ggplot(recovf, aes(y=RecoveryTime, 
                 x=DisturbanceType,
                 fill=DisturbanceType), 
     colour = "black")+
  xlab("")+
  ylab("Recovery Time (years)")+
geom_boxplot()+
#geom_point(aes(y=,x=DisturbanceType,colour=DisturbanceType,fill=DisturbanceType))+
facet_wrap(~ResponseType, nrow = 2, ncol = 2,
           labeller=labeller(ResponseType = 
                               c("ComRecv" = "Benthic Complexity",
                                 "FishRecv" = "Fish Biomass",
                                 "HarvRecv" = "Harvest Biomass",
                                 "LmRecv" = "Coral Population Growth Rate")))+
scale_fill_discrete(name="")+
theme_classic()

ggsave(filename = paste0("DisturbanceRecovery_V2.png"),
       path = paste0(BaseD, "Figures\\"),
       plot = Recovery,
       width = 10,
       height = 7)

load(file=paste0(BaseD,"Disturbance_LongMeanErrs.rdata"))
r <- 3
Fish = ggplot(FullData %>% filter(Response == Resp[r]),
              aes(y=MEAN, x=Time,
                  colour=Disturbance,
                  group=Disturbance))+
  geom_ribbon(aes(ymin=LOWER,ymax=UPPER, 
                  fill=Disturbance),  
              linetype=0,  alpha=0.2)+
  ylab(Resp[r])+
  xlab("Time (years)")+
  xlim(c(5,100))+
  ylim(c(0,0.75))+
  geom_line(size=1.06)+
  theme_classic()

ggsave(filename = paste0(Resp[r],".png"),
       path = paste0(BaseD, "Figures\\"),
       plot = Fish,
       width = 5,
       height = 4)
         


FullData <- data.frame()
DTps <- c("Hurricane", "Disease", "Bleaching")
Resp <- c("Fish Biomass", "Harvest Biomass",
          "Coral Population Growth Rate",
          "Benthic Complexity")
r <- 1
d <- 3
d1 <- dataf[dataf$DisturbanceType == DTps[d],]

if(r == 1){FP <- d1$FishPop}
if(r == 2){FP <- d1$FishHav}
if(r == 3){FP <- d1$Lambs}
if(r == 4){FP <- d1$Complex}


TP <- d1$Time
MG <- d1$Magnitude

df <- data.frame(FP=FP,TP=TP,MG=MG)
dfW <- reshape(df,
               idvar = "TP",
               timevar = "MG",
               direction = "wide",
               sep = "_")

MEAN <- apply(dfW[,2:ncol(dfW)],1,mean)
MEDIAN <- apply(dfW[,2:ncol(dfW)],1,median)
LOWER <- apply(dfW[,2:ncol(dfW)],1,min)
UPPER <- apply(dfW[,2:ncol(dfW)],1,max)
STDE <- apply(dfW[,2:ncol(dfW)],1,sd)/sqrt(100)

dF <- data.frame(MEAN=MEAN,
                 MEDIAN=MEDIAN,
                 LOWER=LOWER,
                 UPPER=UPPER,
                 STDE=STDE,
                 Disturbance=DTps[d],
                 Response=Resp[r])

FullData <- rbind(FullData, dF)

    
Times <- rep(seq(100),length.out=nrow(FullData))
FullData <- cbind(FullData, data.frame(Time=Times))
save(FullData, file=paste0(BaseD,"Disturbance_LongMeanErrs.rdata"))
