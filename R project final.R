#######################################################################################################
######################## REGRESSION PROJECT - USED CARS EBAY PROJECT - R code #####################################
#######################################################################################################

rm(list=ls())

#install.packages("rebus")
#install.packages("stringr")
#install.packages("corrplot")
#install.packages("VIF")
#install.packages("usdm")
library(usdm)
library(ggplot2)
library(rebus)
library(stringr)
library(corrplot)
library(VIF)
library(MASS)
library(glmnet)

#setwd("C:/Users/MINKU/Dropbox/[Georgia Tech]MS in Analytics/ISye 6414/Project/used-cars-database")

### Reading data and deleting NA values
#Cars<-read.csv("autos.csv",header=TRUE)


#################################################### DATA CLEANING ##################################################
#fill the missing blank with NA
Cars[Cars==''] <-NA

#Summary of each column. #It seems that we need to modify some of columns such as price, seller, offerType...
summary(Cars)  

#nrOfpictures has no value so we can drop nrOfpictures column
Cars <- Cars[,-18]

#Since there are only 10 rows with gewerblich(seller) or Gesuch(request), remove them. 
#This allows us to focus on private offer.  
Cars<-Cars[!(Cars$seller =='gewerblich' | Cars$offerType=='Gesuch'),]

#There are advertisements with extremely low price and also highly inflated price.
#We need to remove those outliers.
Cars<-Cars[!(Cars$price <10 | Cars$price> 500000),]

#Also there are a lot of cars registered before 1900. We assume that no one would be interested in that old cars. 
#Some of cars are registered after 2017 which is not possible.(since this original_data was crawled in 2016)
Cars<-Cars[!(Cars$yearOfRegistration < 1900 | Cars$yearOfRegistration > 2016),]

#Now, let's look into the powerPS which is equivalent to horsepower.
#Higher PS than 800 is not likely to happen. So is 0 PS
Cars<-Cars[!(Cars$powerPS ==0 | Cars$powerPS > 800),]

#Change the two columns containg date information into date format.
Cars$dateCreated <-as.Date(Cars$dateCreated, format = "%m/%d/%Y")
Cars$lastSeen <-as.Date(Cars$lastSeen, format = "%m/%d/%Y")

#calculate the duration of the advertisement
Cars['duration'] = Cars$lastSeen - Cars$dateCreated



#drop the columns that might be unuseful in further investigation.
Cars <- Cars[,-c(1,6,17,18,19,20)]

#check the number of missing value in each column
na_count <-sapply(Cars, function(y) sum(length(which(is.na(y)))))
na_count

#notRepariedDamage has the most number of NAs.Maybe people do not want to report whether their car is repaired or not.
#So there might be some bias in the notRepariedDamage column
summary(Cars)

### deleting NA values
Cars <-Cars[complete.cases(Cars),]

#write.csv(Cars, file = "cleaned_data.csv",row.names=FALSE)

#######################################################################################################



### deleting NA values
Cars = na.omit(Cars)
#### convert german characters to english
Cars$notRepairedDamage = factor(Cars$notRepairedDamage,labels=c("Yes","No"))


### replacing year of registration with age of car to be used for analysis
Cars$year_current=2016
Cars$age_of_car=Cars$year_current-Cars$yearOfRegistration
Cars$yearOfRegistration=NULL
Cars$year_current=NULL
#hist(Cars$price,breaks=200)

pattern <- ANY_CHAR %R% "!" %R% ANY_CHAR
names = Cars$name
names_with_q <- str_subset(names, pattern)
count_of_q <- str_count(names, pattern)
#table(count_of_q)
with_q <- str_detect(names, pattern)
Cars["exclamation"] <- with_q
Cars$exclamation = as.numeric(Cars$exclamation)
Cars["exclamation_count"] <- count_of_q
pattern2 <- ANY_CHAR %R% "_" %R% ANY_CHAR
count_of_words <- str_count(names, pattern2)
#table(count_of_words)
Cars["word_count"] <- count_of_words+1
#colnames(Cars)
Cars$exclamation=as.factor(Cars$exclamation)




################################################ Question 1 Correlation #################################################



#### correlation with numerical variables
data_sub=Cars[,c(4,7,9,15,18)]
#cor_cars=cor(data_sub)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

###### scatter plot for variables (FIGURE 1)
pairs(price~., data=data_sub,lower.panel=panel.smooth, upper.panel=panel.cor, pch=20, main="Scatter Plot for Price of Cars")

####(FIGURE 1)
M=cor(data_sub)
corrplot(M,type="upper")

#### boxplots to see correlation with categorical variables

theme_update(plot.title = element_text(hjust = 0.5))

##### FIGURE 2
#boxplot(Cars$price~as.factor(Cars$brand),main=" Boxplot - Price quote on Ebay with Brand",ylab="Price quote on ebay",las=2,outline=FALSE)
box_brand<-ggplot(Cars, aes(x=as.factor(Cars$brand), y=Cars$price, color=as.factor(Cars$brand))) + geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(Cars$price, c(0.1, 0.9)))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab(" Car Brand") +
  ylab("Price on Ebay") + ggtitle("Box Plot - Quoted Price on Ebay with Car Brand")+theme(legend.title = element_blank())
box_brand

##### FIGURE 3
#boxplot(Cars$price ~as.factor(Cars$notRepairedDamage),main=" Boxplot - Price on ebay with whether on not the car is damaged",xlab="Damaged or not",ylab="Price quote on ebay",outline=FALSE)
box_damage<-ggplot(Cars, aes(x=as.factor(Cars$notRepairedDamage), y=Cars$price, color=as.factor(Cars$notRepairedDamage))) + geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(Cars$price, c(0.1, 0.9)))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("Damaged or Not") +
  ylab("Price on Ebay") + ggtitle("Box Plot - Quoted Price on Ebay with whether Car is Damaged or Not")+theme(legend.title = element_blank())
box_damage

##### FIGURE 4
#boxplot(Cars$price ~as.factor(Cars$exclamation),main=" Boxplot - Price on ebay with Exclamation",xlab="Presence of Exclamation",ylab="Price quote on ebay",outline=FALSE)
box_excl<-ggplot(Cars, aes(x=as.factor(Cars$exclamation), y=Cars$price, color=as.factor(Cars$exclamation))) + geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(Cars$price, c(0.1, 0.9)))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("Exclamation Mark Present or Not") +
  ylab("Price on Ebay") + ggtitle("Box Plot - Quoted Price on Ebay with whether the Car Advertisement Title on Ebay has an exclamation mark or not!")+theme(legend.title = element_blank())
box_excl

#### ANOVA Test on Brand
br=Cars$brand
AOV_brand=aov(log(Cars$price)~br)
summary(AOV_brand)
pairwise_brand=TukeyHSD(AOV_brand)
pairwise_brand$br[pairwise_brand$br[,4]>0.05,]

#### VIF (first subset the data for the variables we're using)
colnames(Cars)
data_sub_1=Cars[,c(12,4,6,7,9,13,15,16,18)]
vif(data_sub_1)




################################################### Question 2 ######################################################
### given same brand and age of car how much does whether the car is damaged or not change the model


##reduced model regression
regression.line.reduced = lm(log(price) ~ age_of_car + brand, data=data_sub_1)
summary(regression.line.reduced)
##full model regression
regression.line=lm(log(price) ~ age_of_car + brand+notRepairedDamage, data=data_sub_1)
summary(regression.line)
### partial F-test of full model against reduced model to understand the significance of 
anova(regression.line.reduced, regression.line)





##################################################### Question  3 #####################################################


### To check if the presence of an exclamation mark affects the price or not

box_plot_exclam<-ggplot(Cars, aes(x=as.factor(Cars$exclamation), y=Cars$price, color=as.factor(Cars$exclamation))) + geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(Cars$price, c(0.1, 0.9)))+xlab("Exclamation mark present on Title or Not") +
  ylab("Price on Ebay") + ggtitle("Box Plot - Price vs whether the title contained an Exclamation mark or not")+theme(legend.title = element_blank())
box_plot_exclam


#### To confirm if the presence of exclamation mark affects the price
AOV_exclam = aov(log(Cars$price)~ Cars$exclamation)
summary(AOV_exclam)




#### histogram of word count - (figure 5)
cars_sub_1=Cars[Cars$word_count<100,]
qplot(cars_sub_1$word_count,
      geom="histogram",
      main = "Histogram for Word Count", 
      xlab = "Word Count",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))



### scatter plot pf car prices with word count  (figure 6)
ggplot(Cars, aes(x=Cars$word_count, y=log(Cars$price))) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) +xlab("Word Count") + ylab("Car Price on Ebay") + xlim(1,25)+ ggtitle("Scatter Plot of Car Price on Ebay with Word Count")


title_price_mod=lm(log(price)~exclamation+word_count,data=Cars)
summary(title_price_mod)

dur=Cars$duration
title_dur_mod=lm(log(dur+1)~exclamation+word_count,data=Cars)
summary(title_dur_mod)




##################################################### Question  4 #####################################################
## for testing if required
#dt = sort(sample(nrow(data), nrow(data)*.7))
#train<-data[dt,]
#test<-data[-dt,]


############### MODEL 1 ############### 
data_sub_1=Cars[,c(12,4,6,7,9,13,15,16,18)]
model<-lm((price)~.,data=data_sub_1)
summary(model)

### Evaluating Model Assumptions
#### Residual Plot
resid=model$residuals
fit=model$fitted.values
par(mfrow =c(2,2))
plot(fit,resid,xlab="fits",ylab="Residals")
abline(0,0,col="red")

#### QQ norm plot
qqnorm(resid)
qqline(resid,col="blue")
#hist(resid, xlab="Residuals", main= "Histogram of Residuals",breaks=100)

###  figure 7  and figure 8 before and after log of histogram plot 
qplot(Cars$price,
      geom="histogram",
      main = "Histogram for Car Prices on Ebay", 
      xlab = "Car Price",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))

qplot(log(Cars$price),
      geom="histogram",
      main = "Histogram for log(Car_Price) on Ebay", 
      xlab = "Car Price",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))



############### MODEL 2 - LOG ############### 


#colnames(data_sub_1)
model_2<-lm(log(price)~.,data=data_sub_1)
summary(model_2)

#### use cooks distance to identify outliers
cook = cooks.distance(model)
par(mfrow=c(2,2))
## Check outliers
#influencePlot(model_1)
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")
#(cook[cook>1])

#### Evaluate model assumptions
#### Residual Plot
resid=model_2$residuals
fit=model_2$fitted.values
par(mfrow =c(2,2))
plot(fit,resid,xlab="fits",ylab="Residals")
abline(0,0,col="red")

### qq norm plot

#### QQ norm plot
qqnorm(resid)
qqline(resid,col="blue")
hist(resid, xlab="Residuals", main= "Histogram of Residuals",breaks=100)

#### using lasso variable selection to see which model enters first

colnames(data_sub_1)
attach(data_sub_1)
x <- model.matrix(price ~  gearbox + powerPS + kilometer+notRepairedDamage+age_of_car+exclamation+word_count)[, -1]
#x        <- as.matrix(data.frame(age, bmi_p, xfactors))

# Note alpha=1 for lasso only and can blend with ridge penalty down to
# alpha=0 ridge only.
glmmod <- glmnet(x, y=price)

# Plot variable coefficients vs. shrinkage parameter lambda.
plot(glmmod, xvar="lambda",label=TRUE)



############### MODEL 3 - Box Cox Fit ############### 


# run the box-cox transformation
bc <- boxcox(model)

(lambda <- bc$x[which.max(bc$y)])
lambda
#[1] 0.1818182


# re-run with transformation

m_new = lm((price^lambda-1)/lambda ~ .,data=data_sub_1)
summary(m_new)

# resid plot
resid_3=m_new$residuals
fit_3=m_new$fitted.values
par(mfrow =c(2,2))
plot(fit_3,resid_3,xlab="fits",ylab="Residals")
abline(0,0,col="red")

### qqnorm
qqnorm(resid_3)
qqline(resid_3,col="blue")
hist(resid_3, breaks=100, xlab="Residuals", main= "Histogram of Residuals")


############### MODEL 4 - scaling the variables  ############### 

scale_kilometer=scale(data_sub_1$kilometer)
scale_powerPS=scale(data_sub_1$powerPS)
scale_age=scale(data_sub_1$age)


log_lm_model_scaled1 <-lm(log(price)~gearbox+scale_powerPS+scale_kilometer+brand+notRepairedDamage+word_count+scale_age+exclamation, data=data_sub_1)
summary(log_lm_model_scaled1)
plot(fitted(log_lm_model_scaled1),residuals(log_lm_model_scaled1))




############################################################

### prediction test of Rachel's car

newdata = data.frame(brand="toyota",powerPS=134,kilometer=104607,notRepairedDamage="No",age_of_car=2,exclamation=0,word_count=5)
predictions = predict.lm(model_7, newdata)
predict(model_7, newdata, interval="predict",level=0.95) 





##################################################### Question  5 #####################################################



##### Cars that sell on same day  - are they significantly different from the cars that sell later on

### defining a new variable that tells us whether or not car ad stays up for less than 24hours on the ebay
Cars$flag=0
Cars[Cars$duration==0,]$flag=1
Cars[Cars$duration>0,]$flag=0



#### fig 12 and 13  - box plots for Duration vs age and price of the 2 groups


box_price_dur<-ggplot(Cars, aes(x=as.factor(Cars$flag), y=Cars$price, color=as.factor(Cars$flag))) + geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(Cars$price, c(0.1, 0.9)))+xlab("Car sold within 24 hours(0) or not(1)") +
  ylab("Price on Ebay") + ggtitle("Box Plot - Price vs whether the Car was sold within 24 hours or not")+theme(legend.title = element_blank())
box_price_dur




box_age_dur<-ggplot(Cars, aes(x=as.factor(Cars$flag), y=Cars$age_of_car, color=as.factor(Cars$flag))) + geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(Cars$age_of_car, c(0.1, 0.9)))+xlab("Car sold within 24 hours(0) or not(1)") +
  ylab("Age of car on Ebay Ad") + ggtitle("Box Plot - Age of Car vs whether the Car was sold within 24 hours or not")+theme(legend.title = element_blank())
box_age_dur




##### anova of duration (24 hours or more) with Price
AOV_dur_price = aov(log(Cars$price)~ as.factor(Cars$flag))
summary(AOV_dur_price)


##### anova of duration (24 hours or more) with Age of Car
AOV_dur_age = aov(Cars$age_of_car~ as.factor(Cars$flag))
summary(AOV_dur_age)



#######################################################################################################
########################################## END ########################################################################
#######################################################################################################




