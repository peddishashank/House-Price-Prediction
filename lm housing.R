data = read.csv("housing.csv",stringsAsFactors = T)
head(data)
data = data[,-1]

str(data)
summary(data)

x = boxplot(data$price)
out = x$out
min(out)

data$price = ifelse(data$price >= 125000,mean(data$price),data$price)
data$bedrooms = as.factor(data$bedrooms)
data$bathrms = as.factor(data$bathrms)
data$stories = as.factor(data$stories)
summary(data)
data$garagepl = as.factor(data$garagepl)

### Train test split

sampling =sort(sample(nrow(data),nrow(data)*0.8))
train = data[sampling,]
nrow(train)
test = data[-sampling,]
nrow(test)

#### Build model



model1 = lm(price ~ . , data = train)
summary(model1)

test$pred =  predict(model1,newdata=test)
test$resid = test$price - test$pred
plot(test$resid)

test$MAPE = (test$resid/test$price) * 100
test$MAPE = abs(test$MAPE)
mean(test$MAPE)

train$resid = model1$residuals
train$MAPE = abs((train$resid/train$price) * 100)
mean(train$MAPE)
vif(model1)
summary(model1)
model2 = step(lm(price ~ .,data = data))
summary(model2)



