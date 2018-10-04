library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)
library(MASS)
library(car)
library(vars)
library(glmnet)
library(caTools)
library(corrplot)
library(forecast)
library(vars)

file.names <- dir("./", pattern =".csv")

out <- sapply(file.names, function(x) read.csv(x, stringsAsFactors = F))

names(out)

str(out$`Canada 10-Year Bond Yield Historical Data.csv`)
names(out$`Canada 10-Year Bond Yield Historical Data.csv`) <- 
  c("Date", "C_10_BY_Price", "C_10_BY_Open","C_10_BY_High", 
    "C_10_BY_Low", "C_10_BY_Change")

str(out$`Canada 30-Year Bond Yield Historical Data.csv`)
names(out$`Canada 30-Year Bond Yield Historical Data.csv`) <- 
  c("Date", "C_30_BY_Price", "C_30_BY_Open",
    "C_30_BY_High", "C_30_BY_Low", "C_30_BY_Change")

str(out$`Nasdaq Futures Historical Data.csv`)
names(out$`Nasdaq Futures Historical Data.csv`) <- 
  c("Date", "NF_Price", "NF_Open",
    "NF_High", "NF_Low", "NF_Vol","NF_Change")

str(out$`Crude Oil WTI Futures Historical Data.csv`)
names(out$`Crude Oil WTI Futures Historical Data.csv`) <- 
  c("Date", "CO_WTI_F_Price", "CO_WTI_F_Open",
    "CO_WTI_F_High", "CO_WTI_F_Low","CO_WTI_F_Vol", "CO_WTI_F_Change")

str(out$`Gold Futures Historical Data.csv`)
names(out$`Gold Futures Historical Data.csv`) <- 
  c("Date", "GF_Price", "GF_Open",
    "GF_High", "GF_Low","GF_Vol", "GF_Change")

str(out$`Silver Futures Historical Data.csv`)
names(out$`Silver Futures Historical Data.csv`) <- 
  c("Date", "SF_Price", "SF_Open",
    "SF_High", "SF_Low","SF_Vol", "SF_Change")

str(out$`SnP TSX Composite Historical Data.csv`)
names(out$`SnP TSX Composite Historical Data.csv`) <- 
  c("Date", "Snp_tsx_Comp_Price", "Snp_tsx_Comp_Open",
    "Snp_tsx_Comp_High", "Snp_tsx_Comp_Low","Snp_tsx_Comp_Vol", "Snp_tsx_Comp_Change")

str(out$`SnP 500 VIX Futures Historical Data.csv`)
names(out$`SnP 500 VIX Futures Historical Data.csv`) <- 
  c("Date", "Snp_500_VIX_Price", "Snp_500_VIX_Open",
    "Snp_500_VIX_High", "Snp_500_VIX_Low","Snp_500_VIX_Vol", "Snp_500_VIX_Change")

str(out$`TRI Historical Data.csv`)
names(out$`TRI Historical Data.csv`) <- 
  c("Date", "TRI_Price", "TRI_Open", 
    "TRI_High", "TRI_Low", "TRI_Vol", "TRI_Change")

str(out$`US Dollar Index Futures Historical Data.csv`)
names(out$`US Dollar Index Futures Historical Data.csv`) <- 
  c("Date", "USD_IF_Price", "USD_IF_Open", "USD_IF_High",
    "USD_IF_Low", "USD_IF_Vol", "USD_IF_Change")

str(out$`USD CAD Historical Data.csv`)
names(out$`USD CAD Historical Data.csv`) <- 
  c("Date", "USD_CAD_Price", "USD_CAD_Open", "USD_CAD_High", 
    "USD_CAD_Low", "USD_CAD_Change")

str(out$`EUR CAD Historical Data.csv`)
names(out$`EUR CAD Historical Data.csv`) <- 
  c("Date", "EUR_CAD_Price", "EUR_CAD_Open", "EUR_CAD_High", 
    "EUR_CAD_Low", "EUR_CAD_Change")

str(out$`CM Historical Data.csv`)
names(out$`CM Historical Data.csv`) <- 
  c("Date", "CM_Price", "CM_Open", "CM_High", 
    "CM_Low", "CM_Vol", "CM_Change")

str(out$`MFC Historical Data.csv`)
names(out$`MFC Historical Data.csv`) <- 
  c("Date", "MFC_Price", "MFC_Open", "MFC_High", 
    "MFC_Low", "MFC_Vol", "MFC_Change")
 
test_df <- as.data.frame(out$`SnP 500 VIX Futures Historical Data.csv`$Date)
test_df <- as.data.frame(test_df[-which(is.na(test_df)|(test_df=="")),])
names(test_df) <- c("Date")

summary(test_df)

class(test_df)

for(df_name in names(out))
{
  df <- out[[df_name]]
  df <- df[-which(is.na(df[,ncol(df)])|(df[,1]=="")),]
  test_df <- left_join(test_df, df, by="Date")
  #print(colnames(test))
}

sum(is.na(test_df))

df <- test_df
for (i in 2:ncol(test_df))
{
  z <- which(is.na(test_df[,i]))
  test_df[z,i] <- test_df[z-1,i]
}
sum(is.na(df))
sum(is.na(test_df))

for (i in 2:ncol(test_df))
{
  z <- which(is.na(test_df[,i]))
  test_df[z,i] <- test_df[z-1,i]
}
sum(is.na(df))
sum(is.na(test_df))

merged.data <- test_df

merged.data$Date <- as.Date(as.POSIXlt(merged.data[,1], format='%b %d, %Y'))

merged.data.arranged <- arrange(merged.data, Date)

price.data <- merged.data.arranged[ , c(1, grep("price", colnames(merged.data.arranged), ignore.case = T ))]

change.data <- merged.data.arranged[ , c(1, grep("change", colnames(merged.data.arranged), ignore.case = T ))]


price.data.num <- cbind.data.frame(Date = price.data[,1], as.data.frame(sapply(price.data[,-1], function(coln) { 
  as.vector(sapply(coln, function(x) {
  x=gsub(",", "", x)
  if(grepl("b", x, ignore.case = T)) {as.numeric(gsub("B", "", x)) * (10^9)}
  else if(grepl("m", x, ignore.case=T)) {as.numeric(gsub("M", "", x))* (10^6)}
  else if(grepl("k", x, ignore.case=T)) {as.numeric(gsub("K", "", x)) * (10^3)}
  else as.numeric(x)}))})))

sum(is.na(price.data.num))

str(price.data.num)



price.data.num$quarter <-  paste(format(price.data.num$Date, "%Y"), "-", quarters(price.data.num$Date), sep="")

price.data.transformed <- cbind.data.frame(Date=price.data.num[,1], sapply(price.data.num[,-c(1,16)], function(x) as.double(x/max(x))))

bar_x_axis = theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


price.data.num.aggregated <- cbind.data.frame(price.data.transformed, quarter=price.data.num$quarter) %>% group_by(quarter) %>% 
  summarise( a.CM_Price = mean(CM_Price),
    a.c_10_By_Price = mean(C_10_BY_Price),
    a.c_30_By_Price = mean(C_30_BY_Price),
    a.CO_WTI_F_Price = mean(CO_WTI_F_Price),
    a.EUR_CAD_Price = mean(EUR_CAD_Price),
    a.GF_Price = mean(GF_Price),
    a.MFC_Price = mean(MFC_Price),
    a.NF_Price = mean(NF_Price),
    a.sF_Price = mean(SF_Price),
    a.Snp_500_VIX_Price = mean(Snp_500_VIX_Price),
    a.Snp_tsx_Comp_Price = mean(Snp_tsx_Comp_Price),
    a.TRI_Price = mean(TRI_Price),
    a.USD_IF_Price = mean(USD_IF_Price),
    a.USD_CAD_Price = mean(USD_CAD_Price))

bar_x_axis = theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

attach(price.data.num.aggregated)
ggplot(price.data.num.aggregated) +
  geom_line(aes(quarter, a.c_10_By_Price, group=1, colour="c_10_By_Price")) + 
  geom_line(aes(quarter, a.c_30_By_Price, group=1, colour="c_30_By_Price")) +
  geom_line(aes(quarter, a.CO_WTI_F_Price, group=1, colour="CO_WTI_F_Price")) +
  geom_line(aes(quarter, a.CM_Price, group=1, colour="CM_Price")) +
  scale_colour_manual(name="Prices", 
                      breaks = c("c_10_By_Price", "c_30_By_Price", "CO_WTI_F_Price", "CM_Price"),
                      values = c("red", "green", "purple", "black")) + 
  bar_x_axis + labs(y="Normalized Price", x="Year-Quarter") + theme(legend.position = "bottom")


ggplot(price.data.num.aggregated) +
  geom_line(aes(quarter, a.EUR_CAD_Price, group=1, colour="EUR_CAD_Price")) + 
  geom_line(aes(quarter, a.GF_Price, group=1, colour="GF_Price")) +
  geom_line(aes(quarter, a.NF_Price, group=1, colour="NF_Price")) +
  geom_line(aes(quarter, a.CM_Price, group=1, colour="CM_Price")) +
  scale_colour_manual(name="Prices", 
                      breaks = c("EUR_CAD_Price", "GF_Price", "NF_Price", "CM_Price"),
                      values = c("black", "red", "green", "purple")) + 
  bar_x_axis + labs(y="Normalized Price", x="Year-Quarter") + theme(legend.position = "bottom")

ggplot(price.data.num.aggregated) +
  geom_line(aes(quarter, a.Snp_500_VIX_Price, group=1, colour="Snp_500_VIX_Price")) + 
  geom_line(aes(quarter, a.Snp_tsx_Comp_Price, group=1, colour="Snp_tsx_Comp_Price")) +
  geom_line(aes(quarter, a.TRI_Price, group=1, colour="TRI_Price")) +
  geom_line(aes(quarter, a.CM_Price, group=1, colour="CM_Price")) +
  scale_colour_manual(name="Prices", 
                      breaks = c("Snp_500_VIX_Price", "Snp_tsx_Comp_Price", "TRI_Price","CM_Price" ),
                      values = c("black", "red", "green", "purple")) + 
  bar_x_axis + labs(y="Normalized Price", x="Year-Quarter") + theme(legend.position = "bottom")

ggplot(price.data.num.aggregated) +
  geom_line(aes(quarter, a.USD_IF_Price, group=1, colour="USD_IF_Price")) + 
  geom_line(aes(quarter, a.MFC_Price, group=1, colour="MFC_Price")) +
  geom_line(aes(quarter, a.USD_CAD_Price, group=1, colour="USD_CAD_Price")) +
  geom_line(aes(quarter, a.CM_Price, group=1, colour="CM_Price")) +
  scale_colour_manual(name="Prices", 
                      breaks = c("USD_IF_Price", "MFC_Price", "USD_CAD_Price", "CM_Price"),
                      values = c("black", "red", "green", "purple")) + 
  bar_x_axis + labs(y="Normalized Price", x="Year-Quarter") + theme(legend.position = "bottom")



attach(price.data.transformed)

#Quarter level plots

ggplot(price.data.transformed, aes(CM_Price)) + geom_histogram(bins=50) # more or less normaly distributed

train <- price.data.transformed[1:(nrow(price.data.transformed)*0.7),]
test_df <-price.data.transformed[-(1:nrow(train)),]
#########################################Linear Regression#######################


M <- cor(price.data.transformed[,-1])
corrplot(M, method = "number", type = "upper")

model_1 <-lm(CM_Price~.,data=train[,-1])

step <- stepAIC(model_1, direction="both")

step

model_2 <- lm(formula = CM_Price ~ C_10_BY_Price + C_30_BY_Price + CO_WTI_F_Price + 
                EUR_CAD_Price + MFC_Price + NF_Price + SF_Price + Snp_500_VIX_Price + 
                Snp_tsx_Comp_Price + TRI_Price + USD_IF_Price + USD_CAD_Price, 
              data = train[, -1])

summary(model_2)
sort(vif(model_2))


model_3 <- lm(formula = CM_Price ~ C_10_BY_Price + CO_WTI_F_Price + 
                EUR_CAD_Price + MFC_Price + NF_Price + SF_Price + Snp_500_VIX_Price + 
                Snp_tsx_Comp_Price + TRI_Price + USD_IF_Price + USD_CAD_Price, 
              data = train[, -1])

summary(model_3)
sort(vif(model_3))

model_4 <- lm(formula = CM_Price ~ C_10_BY_Price + CO_WTI_F_Price + 
                EUR_CAD_Price + MFC_Price + NF_Price + SF_Price + Snp_500_VIX_Price + 
                Snp_tsx_Comp_Price + TRI_Price + USD_CAD_Price, 
              data = train[, -1])

summary(model_4)
sort(vif(model_4))


model_5 <- lm(formula = CM_Price ~ C_10_BY_Price + CO_WTI_F_Price + 
                EUR_CAD_Price + MFC_Price + SF_Price + Snp_500_VIX_Price + 
                Snp_tsx_Comp_Price + TRI_Price + USD_CAD_Price, 
              data = train[, -1])

summary(model_5)
sort(vif(model_5))


model_6 <- lm(formula = CM_Price ~ C_10_BY_Price + 
                EUR_CAD_Price + MFC_Price + SF_Price + Snp_500_VIX_Price + 
                Snp_tsx_Comp_Price + TRI_Price + USD_CAD_Price, 
              data = train[, -1])

summary(model_6)
sort(vif(model_6))

model_7 <- lm(formula = CM_Price ~ C_10_BY_Price + 
                EUR_CAD_Price + MFC_Price + SF_Price + Snp_500_VIX_Price + 
                Snp_tsx_Comp_Price + TRI_Price, 
              data = train[, -1])

summary(model_7)
sort(vif(model_7))

model_8 <- lm(formula = CM_Price ~ C_10_BY_Price + 
                EUR_CAD_Price + SF_Price + Snp_500_VIX_Price + 
                Snp_tsx_Comp_Price + TRI_Price, 
              data = train[, -1])

summary(model_8)
sort(vif(model_8))

model_9 <- lm(formula = CM_Price ~ C_10_BY_Price + 
                SF_Price + Snp_500_VIX_Price + 
                Snp_tsx_Comp_Price + TRI_Price, 
              data = train[, -1])

summary(model_9)
sort(vif(model_9))

model_10 <- lm(formula = CM_Price ~ C_10_BY_Price + 
                SF_Price + Snp_500_VIX_Price + 
                Snp_tsx_Comp_Price, 
              data = train[, -1])

summary(model_10)
sort(vif(model_10))

result <- as.data.frame(test_df[,c(1,4)])

test_df <- test_df[,-c(1,4)]

result$linear_pred <- predict(model_10, test_df)

par(mfcol=c(1,1))
#linear prediction output
ggplot(result) +
  geom_line(aes(Date, CM_Price,colour="CM_Price")) +
  geom_line(aes(Date, linear_pred, colour = "CM_pred_price")) +
  scale_color_manual(name="Prices", 
breaks = c("CM_Price", "CM_pred_price"),
values = c("red", "green"))

ggplot(result, aes(linear_pred - CM_Price)) + geom_histogram(bins = 30)
#almost normally distributed

###################################LASSO Model##############################################
#grid = 10^seq(10, -2, length =100)

y <- train$CM_Price
x <- as.matrix(train[,-c(1,4)])

lasso.model <- glmnet(x, y, alpha = 1)
ridge.model <- glmnet(x,y, alpha = 0)
elastic.net <- glmnet(x,y, alpha = 0.5)

plot(lasso.model, label = T)

plot(ridge.model, label = T)

plot(elastic.net, label = T)

dim(coef(lasso.model))

#CV has been used to find the lambda value
set.seed(100)
cv.lasso.model <- cv.glmnet(x,y, alpha=1)

cv.ridge.model <- cv.glmnet(x,y, alpha=0)

cv.elastic.model <- cv.glmnet(x,y, alpha=0.5)

plot(cv.lasso.model)

plot(cv.ridge.model)

plot(cv.elastic.model)

names(cv.lasso.model)

cv.lasso.model$lambda.min      
# 4.331556e-05

cv.lasso.model$lambda.1se #1 sd
#0.0001919149

result$lasso.pred.min.gamma <- predict(cv.lasso.model, newx = as.matrix(test_df), s=cv.lasso.model$lambda.min)

result$lasso.pred.1se.gamma <- predict(cv.lasso.model, newx = as.matrix(test_df), s=cv.lasso.model$lambda.1se)

result$ridge.pred.min.gamma <- predict(cv.ridge.model, newx = as.matrix(test_df), s=cv.ridge.model$lambda.min)

result$ridge.pred.1se.gamma <- predict(cv.ridge.model, newx = as.matrix(test_df), s=cv.ridge.model$lambda.1se)

result$elastic.pred.min.gamma <- predict(cv.elastic.model, newx = as.matrix(test_df), s=cv.elastic.model$lambda.min)

result$elastic.pred.1se.gamma <- predict(cv.elastic.model, newx = as.matrix(test_df), s=cv.elastic.model$lambda.1se)

lasso.model.fit <- glmnet(x,y, alpha=1, lambda = cv.lasso.model$lambda.1se)

coef(lasso.model.fit)

ridge.model.fit <- glmnet(x,y, alpha=0, lambda = cv.ridge.model$lambda.1se)

coef(ridge.model.fit)

elastic.model.fit <- glmnet(x,y, alpha=0.5, lambda = cv.elastic.model$lambda.1se)

coef(elastic.model.fit)

ggplot(result) + geom_line(aes(Date, CM_Price, colour="CM_Price")) +
#lines(result$Date, result$lasso.pred.1se.gamma, col="green")
geom_line(aes(Date, lasso.pred.min.gamma, colour="lasso.pred"))+
  geom_line(aes(Date, ridge.pred.min.gamma, colour="ridge.pred"))+ #Seems like these are closer to the actual data
#lines(result$Date, result$ridge.pred.1se.gamma, col="orange") # But may overfit
#lines(result$Date, result$elastic.pred.min.gamma, col="violet")
  geom_line(aes(Date, elastic.pred.1se.gamma, colour="elastic.pred")) +
  scale_colour_manual(name="Prices", 
                      breaks = c("CM_Price", "lasso.pred", "ridge.pred","elastic.pred" ),
                      values = c("black", "red", "blue", "purple"))
  

##############################Vector AutoRegression############################

head(price.data.transformed)

CM <- cor(price.data.transformed[,-1])
names(sort(CM[3,]))[c(1,2,12,13)]
corrplot(CM, method = "number")

##par(mfrow=c(1,2))
##with(price.data.transformed, pacf(CM_Price, lag.max = 50))
##with(price.data.transformed, pacf(USD_CAD_Price, lag.max = 50))
##
##with(price.data.transformed, ccf(CM_Price,USD_CAD_Price, lag.max = 50))
##with(price.data.transformed, pacf(USD_CAD_Price, lag.max = 50))
##par(mfrow=c(1,1))
#put data into a time series
price.data.ts = ts(price.data.transformed[,-1], frequency=1)
ncol(price.data.ts)

colnames(price.data.ts)

plot(price.data.ts[,c(1:6,8:9)])
plot(price.data.ts[,c(10:11,13:14)])

#Highly Correlated Data with CM
var.data <- price.data.transformed[,colnames(price.data.transformed) %in% c(names(sort(CM[3,]))[c(1,2,12,13)],"CM_Price")]

str(var.data)

var.data.ts = ts(var.data, frequency=1)


#determine stationarity and number of lags to achieve stationarity
dummy <- var.data.ts[,5]
ndiffs(dummy, alpha=0.05, max.d = 10)
for(i in 1:ncol(var.data.ts)) {
  print(ndiffs(var.data.ts[,i], alpha = 0.05, test = c("adf"), max.d = 10))
}

colnames(var.data.ts)

station.C_30_BY_Price <- diff(var.data.ts[,1], differences = 1)

station.CM_Price <- diff(var.data.ts[,2], differences = 1)

station.NF_Price <- diff(var.data.ts[,3], differences = 1)

station.snp_500_VIX_Price <- diff(var.data.ts[,4], differences = 1)

station.snp_tsx_comp_Price <- diff(var.data.ts[,5], differences = 1)

stationary_data <- cbind(station.C_30_BY_Price, station.CM_Price, station.NF_Price, station.snp_500_VIX_Price , station.snp_tsx_comp_Price )


for(i in 1:ncol(stationary_data)) {
  print(ndiffs(ts(stationary_data[,i]), alpha = 0.05))
} #0 Indicates all are stationary now no need of further differencing

VARselect(stationary_data, lag.max=10, type="both")$selection #BIC = 1, AIC=3

data.var.1.const <- VAR(stationary_data, p=1, type="const")

serial.test(data.var.1.const, lags.pt=10, type="PT.asymptotic")

data.var.1.both <- VAR(stationary_data, p=1, type="both")

serial.test(data.var.1.both, lags.pt=10, type="PT.asymptotic")


data.var.2.const <- VAR(stationary_data, p=2, type="const")

serial.test(data.var.2.const, lags.pt=10, type="PT.asymptotic")

data.var.2.both <- VAR(stationary_data, p=2, type="both")

serial.test(data.var.2.both, lags.pt=10, type="PT.asymptotic")

arch.test(data.var.2.both, lags.multi = 10)


data.var.3.const <- VAR(stationary_data, p=3, type="const")

serial.test(data.var.3.const, lags.pt=10, type="PT.asymptotic")

data.var.3.both <- VAR(stationary_data, p=3, type="both")

serial.test(data.var.3.both, lags.pt=10, type="PT.asymptotic")

#As the model doesn't produce stationary residues we are trying 
#few more lags and see if that works for us

data.var.9.const <- VAR(stationary_data, p=9, type="const")

serial.test(data.var.9.const, lags.pt=10, type="PT.asymptotic")

data.var.9.both <- VAR(stationary_data, p=9, type="both")

serial.test(data.var.9.both, lags.pt=10, type="PT.asymptotic")

#ARCH test (Autoregressive conditional heteroscedasdicity)
arch.test(data.var.9.both, lags.multi = 10)

summary(data.var.1.both)

#Granger Causality test
#Does x1 granger cause x2?
grangertest(station.C_30_BY_Price  ~ station.CM_Price, order = 1)

#Does x2 granger cause x1?
grangertest(station.CM_Price ~ station.C_30_BY_Price, order = 2)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      1      1      2

fcst <- forecast(data.var.1.both)

plot(fcst, xlab="Days")

plot(stationary_data, xlab="Year")

#Prediction
prd <- predict(data.var.1.both, n.ahead = 60, ci = 0.95, dumvar = NULL)
print(prd)
plot(prd, "single")

