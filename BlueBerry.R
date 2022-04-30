getwd()
setwd("/Users/shanmukhchintakula/Desktop/MET_CS_555_A3/555 Term Project")
data <- read_csv("https://raw.githubusercontent.com/jaime-wang/CS544-project-Haomin-Shanmukh/main/Blueberry%20yield%20data.csv")
install.packages("corrplot")
library(corrplot)
library(RColorBrewer)
library(sampling)
library(rgl)

corrplot(method="circle", cor(data), col=brewer.pal(n=8, name="RdBu"))

data<- data[ , -c(1, 8:13, 15:17)] # Removing highly correlated columns
corrplot(method="circle", cor(data), col=brewer.pal(n=8, name="RdBu"))
corrplot(method="number", cor(data), col=brewer.pal(n=8, name="RdBu"))
glimpse(data)

n<-500
set.seed(2116)
data<- data[which(srswor(n, nrow(data))==1), ]
nrow(data)

boxplot(data$yield, main="yield boxplot")

model <- lm(yield ~ clonesize + honeybee + bumbles + andrena + osmia + MaxOfUpperTRange + AverageRainingDays , data = data)
summary(model) #Adj R2=81.6%

confint(model, conf.level=0.95)
#81.6% of the variation in the output variable is explained by the input variables.
#Reject Null hypothesis

set.seed(2116)
train.size <- 0.8
train.index<- sample.int(n, n*train.size)
length(train.index)
train.sample <- data[train.index, ]
valid.sample <- data[-train.index, ]

nrow(train.sample)
nrow(valid.sample)

#Step wise selection of variables by backward elimination
model <- lm(yield ~  honeybee + bumbles + andrena + osmia + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model) #Adj R2= 58.5%

model <- lm(yield ~ clonesize  + bumbles + andrena + osmia + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model) #Adj R2= 80.9%

model <- lm(yield ~ clonesize + honeybee  + andrena + osmia + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model) #Adj R2= 74.25%

model <- lm(yield ~ clonesize + honeybee + bumbles  + osmia + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model) #Adj R2= 81.8% Remove Andrena

model <- lm(yield ~ clonesize + honeybee + bumbles + andrena  + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model) #Adj R2= 76.4%

model <- lm(yield ~ clonesize + honeybee + bumbles + andrena + osmia  + AverageRainingDays , data = train.sample)
summary(model) #Adj R2= 79.0%

model <- lm(yield ~ clonesize + honeybee + bumbles + andrena + osmia + MaxOfUpperTRange  , data = train.sample)
summary(model) #Adj R2= 45.11%

# Remove Andrena at Adj R2= 81.23%

model2 <- lm(yield ~  honeybee + bumbles  + osmia + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model2) #Adj R2= 55.8.2% 

model2 <- lm(yield ~ clonesize  + bumbles  + osmia + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model2) #Adj R2= 80.1 %  remove honeybee

model2 <- lm(yield ~ clonesize + honeybee   + osmia + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model2) #Adj R2= % 

model2 <- lm(yield ~ clonesize + honeybee + bumbles   + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model2) #Adj R2= % 

model2 <- lm(yield ~ clonesize + honeybee + bumbles  + osmia  + AverageRainingDays , data = train.sample)
summary(model2) #Adj R2= % 

model2 <- lm(yield ~ clonesize + honeybee + bumbles  + osmia + MaxOfUpperTRange  , data = train.sample)
summary(model2) #Adj R2= % 

#  remove honeybee at Adj R2= 80.67 %

model3 <- lm(yield ~   bumbles  + osmia + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model3) #Adj R2= %

model3 <- lm(yield ~ clonesize    + osmia + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model3) #Adj R2= %

model3 <- lm(yield ~ clonesize  + bumbles   + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model3) #Adj R2= %

model3 <- lm(yield ~ clonesize  + bumbles  + osmia  + AverageRainingDays , data = train.sample)
summary(model3) #Adj R2= 77.85%

model3 <- lm(yield ~ clonesize  + bumbles  + osmia + MaxOfUpperTRange  , data = train.sample)
summary(model3) #Adj R2= %

#Nothing removed

#final model
model2 <- lm(yield ~ clonesize  + bumbles  + osmia + MaxOfUpperTRange + AverageRainingDays , data = train.sample)
summary(model2) #Adj R2= 80.1 % 

model2.valid <- lm(yield ~ clonesize  + bumbles  + osmia + MaxOfUpperTRange + AverageRainingDays , data = valid.sample)
summary(model5)  #Adj R2=96.4

plot(model2)
plot(model2.valid)



train.sample$Pred.Yield<- predict(model2, newdata = subset(train.sample, 
select = c(clonesize , bumbles  , osmia , MaxOfUpperTRange , AverageRainingDays)))

valid.sample$Pred.Yield<- predict(model2.valid, newdata = subset(valid.sample, 
select = c(clonesize  , bumbles  , osmia , MaxOfUpperTRange , AverageRainingDays)))


train.corr<- cor(train.sample$Pred.Yield, train.sample$yield)
train.RMSE<- sqrt(mean((train.sample$Pred.Yield-train.sample$yield)^2))
c(train.corr^2, train.RMSE)
#0.8035189 594.8605725

valid.corr<- cor(valid.sample$Pred.Yield, valid.sample$yield)
valid.RMSE<- sqrt(mean((valid.sample$Pred.Yield-valid.sample$yield)^2))
c(valid.corr^2, valid.RMSE)
#0.8372958 597.9372955





