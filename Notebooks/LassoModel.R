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

file.names <- dir("./", pattern =".csv")

out <- sapply(file.names, function(x) read.csv(x, stringsAsFactors = F))

names(out)

str(out$`Canada 10-Year Bond Yield Historical Data.csv`)
names(out$`Canada 10-Year Bond Yield Historical Data.csv`) <- 
  c("Date", "C-10-BY_Price", "C-10-BY_Open","C-10-BY_High", 
    "C-10-BY_Low", "C-10-BY_Change")

str(out$`Canada 30-Year Bond Yield Historical Data.csv`)
names(out$`Canada 30-Year Bond Yield Historical Data.csv`) <- 
  c("Date", "C-30-BY_Price", "C-30-BY_Open",
    "C-30-BY_High", "C-30-BY_Low", "C-30-BY_Change")

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

str(out$`SnP TSX Equity Historical Data.csv`) 
names(out$`SnP TSX Equity Historical Data.csv`) <- 
  c("Date", "Snp_tsx_Eq_Price", "Snp_tsx_Eq_Open",
    "Snp_tsx_Eq_High", "Snp_tsx_Eq_Low","Snp_tsx_Eq_Vol", "Snp_tsx_Eq_Change")

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
 
test <- as.data.frame(out$`SnP 500 VIX Futures Historical Data.csv`$Date)
test <- test[-which(is.na(test)|(test==""))]
names(test) <- c("Date")

summary(test)

for(df_name in names(out))
{
  df <- out[[df_name]]
  df <- df[-which(is.na(df[,ncol(df)])|(df[,1]=="")),]
  test <- merge(test, df, by="Date", all.y = T)
}

sum(is.na(test))

merged.data <- na.omit(test)

merged.data$Date <- as.Date(as.POSIXlt(merged.data[,1], format='%b %d, %Y'))

clean_merged_data <- arrange(merged.data, Date)

price_data <- final_Data[ , c(1, grep("price", colnames(final_Data), ignore.case = T ))]

change_data <- final_Data[ , c(1, grep("change", colnames(final_Data), ignore.case = T ))]


#final_Data <- cbind.data.frame(Date = clean_merged_data[,1], as.data.frame(sapply(clean_merged_data[,-1], function(coln) { 
#  sapply(coln, function(x) {
#  x=gsub(",", "", x)
#  if(grepl("b", x, ignore.case = T)) {as.numeric(gsub("B", "", x)) * (10^9)}
#  else if(grepl("m", x, ignore.case=T)) {as.numeric(gsub("M", "", x))* (10^6)}
#  else if(grepl("k", x, ignore.case=T)) {as.numeric(gsub("K", "", x)) * (10^3)}
#  else as.numeric(x)})})))
#
#sum(is.na(final_Data))

final_Data <- arrange(price_data, desc(Date))

final_data_trandformed <- cbind.data.frame(Date=final_Data[,1], sapply(final_Data[,-1], function(x) as.double(x/max(x))))

bar_x_axis = theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

attach(final_data_trandformed)

#plot(Date,`C-10-BY_Price`,type='n',main='Comparison of Different Prices',
#     xlab='Dates',ylab='Normalized Price')
#
#c <- colours(distinct = T)
#
#for(i in 2:ncol(final_data_trandformed)) {
#  lines(Date,final_data_trandformed[,i],col=c[i+20])
#  }

ggplot(final_data_trandformed, aes(CM_Price)) + geom_histogram() # more or less normaly distributed

ggplot(final_data_trandformed, aes(x=final_data_trandformed$Date)) +
  geom_line(aes(y=`C-10-BY_Price`, group=1), colour="red") +
  geom_line(aes(y=CO_WTI_F_Price, group=1), colour="green") +
  geom_line(aes(y=EUR_CAD_Price, group=1), colour="blue") +
  geom_line(aes(y=GF_Price, group=1), colour="gold") +
  geom_line(aes(y=NF_Price, group=1), colour="purple") +
  geom_line(aes(y=Snp_tsx_Comp_Price, group=1), colour="yellow") +
  geom_line(aes(y=TRI_Price, group=1), colour="pink") +
  geom_line(aes(y=USD_CAD_Price, group=1), colour="maroon") +
  bar_x_axis


#########################################Linear Regression#######################


M <- cor(train[,-c(1,2)])
corrplot(M, method = "circle")
#3,4,5,9 are highly correlated 2 and each other as well with each other hence removing it
#26 and 27 also seems to me highly correlated with others
train <- final_data_trandformed[final_data_trandformed$Date<= as.Date(as.POSIXlt("2017-06-30", format="%Y-%m-%d")),]
test <- final_data_trandformed[final_data_trandformed$Date> as.Date(as.POSIXlt("2017-06-30", format="%Y-%m-%d")),]

model_1 <-lm(CM_Price~.,data=train[,-1])

step <- stepAIC(model_1, direction="both")

step

model_2 <- lm(formula = CM_Price ~ `C-10-BY_Price` + `C-30-BY_Price` + CO_WTI_F_Price + 
                EUR_CAD_Price + GF_Price + NF_Price + Snp_tsx_Comp_Price + 
                TRI_Price + USD_CAD_Price, data = train[, -1])

summary(model_2)
sort(vif(model_2))


model_3 <- lm(formula = CM_Price ~ `C-30-BY_Price` + 
                CO_WTI_F_Price + EUR_CAD_Price + GF_Price + MFC_Price + NF_Price + 
                Snp_tsx_Comp_Price + TRI_Price + USD_CAD_Price, data = train[,-1])

summary(model_3)
sort(vif(model_3))

model_4 <- lm(formula = CM_Price ~ `C-30-BY_Price` + 
                CO_WTI_F_Price + EUR_CAD_Price + GF_Price + MFC_Price + NF_Price + 
                TRI_Price + USD_CAD_Price, data = train[,-1])

summary(model_4)
sort(vif(model_4))


model_5 <- lm(formula = CM_Price ~ `C-30-BY_Price` + 
                CO_WTI_F_Price + EUR_CAD_Price + MFC_Price + NF_Price + 
                TRI_Price + USD_CAD_Price, data = train[,-1])

summary(model_5)
sort(vif(model_5))


model_6 <- lm(formula = CM_Price ~ 
                CO_WTI_F_Price + EUR_CAD_Price + MFC_Price + NF_Price + 
                TRI_Price + USD_CAD_Price, data = train[,-1])

summary(model_6)
sort(vif(model_6))

model_7 <- lm(formula = CM_Price ~ 
                CO_WTI_F_Price + EUR_CAD_Price + MFC_Price + NF_Price + 
                USD_CAD_Price, data = train[,-1])

summary(model_7)
sort(vif(model_7))

result <- as.data.frame(test$Date)

result$CM_Price <- test[,4]

result$prediction <- predict(model_7, test[,-c(1,4)])

plot(test$Date, test$CM_Price)

line

mean((result$prediction - test$CM_Price)**2)

###################################LASSO Model##############################################
#grid = 10^seq(10, -2, length =100)

y <- train[,4]
x <- as.matrix(train[,-c(1,4)])

lasso_model <- glmnet(x, y, alpha = 1)

plot(lasso_model, label = TRUE)

dim(coef(lasso_model))

#CV has been used to find the lambda value
set.seed(100)
cv.lasso.model <- cv.glmnet(x,y, alpha=1)
plot(cv.lasso.model)

names(cv.lasso.model)

cv.lasso.model$glmnet.fit

cv.lasso.model$lambda.min      
# 4.331556e-05

cv.lasso.model$lambda.1se #1 sd
#0.0001919149

lasso_model.final <- glmnet(x,y, alpha = 1)


result$lasso_pred_out_min <- predict(cv.lasso.model, newx = as.matrix(test[,-c(1,4)]), s=cv.lasso.model$lambda.min)

test$lasso_pred_out_1se <- predict(lasso_model.final, newx = as.matrix(test[,-c(1,4)]), s=cv.lasso.model$lambda.1se)

intercepts <- names(coefficients)[which(coefficients!=0.0)]
intercepts <- cbind(intercepts,coefficients[which(coefficients!=0.0)])

intercepts

predict.glmnet(lasso_model.final, newx = model.matrix(test[,-c(1,4)]))

set.seed(123)
cv_lasso = cv.glmnet(as.matrix(train[, -c(1,4)]), train[, 4])

plot(cv_lasso)

cv_lasso$lambda.min

cv_lasso$lambda.1se

## Predictions
preds <- predict(cv_lasso, newx = as.matrix(test[, -c(1,4)]), s = cv_lasso$lambda.1se)
sum((test$CM_Price-preds)**2)
