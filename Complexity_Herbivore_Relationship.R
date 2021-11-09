#data from Fig3a Fukunaga et al. 2020
data <- read.csv("wpd_datasets.csv")

fit<-lm(log(Line_Y)~Line_X, data=data)
summary(fit)

f1 <- function (X) {
  exp(3.784931) * exp(0.316063*X)
}

Pred_Y <- f1(data$Line_X)
data <- cbind(data, Pred_Y)

ggplot() + 
  geom_point(data=data, aes(x=Line_X, y=Line_Y)) +
  geom_line(data = data, aes(x=Line_X, y=Pred_Y))