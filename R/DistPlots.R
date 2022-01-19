# Disturbance Plots
library(ggplot2)
library(stringr)
library(dplyr)
library(reshape2)



BaseD <- "C:\\Users\\Devynn\\Documents\\GitHub\\MBIO740_AMD\\output\\"
ls    <- list.files(BaseD)

load(paste0(BaseD,ls[11]))
load(paste0(BaseD,ls[18]))

# ###### Color hack!
# COLORS <- c("#e84935",
#             "#850f92",
#             "#5a85e8")
# 
# DTs <- recovf$DisturbanceType
# CCs <- rep(0, length.out=length(DTs))
# 
# CCs[which(DTs == "Bleaching")] <- COLORS[1]
# CCs[which(DTs == "Disease")] <- COLORS[2]
# CCs[which(DTs == "Hurricane")] <- COLORS[3]
# 
# recovf <- cbind(recovf, data.frame(Colors=CCs))

Resp <- c("Fish Biomass", "Harvest Biomass",
          "Coral Population Growth Rate",
          "Benthic Complexity")
CX_type <- c("Logit", "Exp", "Log")

for (cx in 1:3){
  DataUse <- recovf[recovf$ComplexType == CX_type[cx],]
Recovery = ggplot(DataUse, aes(y=RecoveryTime, 
                 x=DisturbanceType,
                 fill=DisturbanceType), 
                 colour = "black")+
  xlab("")+
  ylab("Recovery Time (years)")+
  geom_boxplot()+
  scale_fill_manual(values= c("Bleaching"= "#e84935",
                              "Disease"= "#850f92",
                              "Hurricane"="#5a85e8"))+
#geom_point(aes(y=,x=DisturbanceType,colour=DisturbanceType,fill=DisturbanceType))+
facet_wrap(~ResponseType, nrow = 1, ncol = 4,
           labeller=labeller(ResponseType = 
                               c("ComRecv" = "Benthic Complexity",
                                 "FishRecv" = "Fish Biomass",
                                 "HarvRecv" = "Harvest Biomass",
                                 "LmRecv" = "Coral Population Growth Rate")))+
#scale_fill_discrete(name="")+
theme_classic()

ggsave(filename = paste0("DisturbanceRecovery_",CX_type[cx],".png"),
       path = paste0(BaseD, "Figures\\"),
       plot = Recovery,
       width = 15,
       height = 4.5)

}

load(file=paste0(BaseD,"Disturbance_LongMeanErrs_AllCX.rdata"))
load(file=paste0(BaseD,"YMaxValues.rdata"))

for(r in 1:4){
  for(cx in 1:3){
    daData <- FullData[FullData$CXtype == CX_type[cx],]

ResponsePlt = ggplot(daData %>% filter(Response == Resp[r]),
              aes(y=MEAN, x=Time,
                  colour=Disturbance,
                  group=Disturbance))+
  scale_color_manual(values = c("Bleaching"= "#e84935",
                               "Disease"= "#850f92",
                               "Hurricane"="#5a85e8"))+
  geom_ribbon(aes(ymin=LOWER,ymax=UPPER, 
                  fill=Disturbance),  
              linetype=0,  alpha=0.2)+
  scale_fill_manual(values = c("Bleaching"= "#e84935",
                               "Disease"= "#850f92",
                               "Hurricane"="#5a85e8"))+
  ylab(Resp[r])+
  xlab("Time (years)")+
  xlim(c(5,100))+
  ylim(c(0,yMaxVals[r,cx]))+
  geom_line(size=1.06)+
  theme_classic()

ggsave(filename = paste0(Resp[r],"_",CX_type[cx],".png"),
       path = paste0(BaseD, "Figures\\"),
       plot = ResponsePlt,
       width = 5,
       height = 4)
         
  }
}

FullData <- data.frame()
DTps <- c("Hurricane", "Disease", "Bleaching")
Resp <- c("Fish Biomass", "Harvest Biomass",
          "Coral Population Growth Rate",
          "Benthic Complexity")
CX_type <- c("Logit", "Exp", "Log")
r  <- 2
d  <- 2
cx <- 1

for(r in 1:length(Resp)){
  for(d in 1:length(DTps)){
    for(cx in 1:length(CX_type)){
      dd <- dataf[dataf$ComplexType == CX_type[cx],]
      d1 <- dd[dd$DisturbanceType == DTps[d],]
      
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
                       Response=Resp[r],
                       CXtype=CX_type[cx])
      
      FullData <- rbind(FullData, dF)
    }
  }
}


    
Times <- rep(seq(100),length.out=nrow(FullData))
FullData <- cbind(FullData, data.frame(Time=Times))
save(FullData, file=paste0(BaseD,"Disturbance_LongMeanErrs_AllCX.rdata"))
