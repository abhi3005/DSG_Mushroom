# importing the dataset
mushroom = read.csv("C:/Users/Abhishek/Downloads/mushroom_train.csv")
 mushroom[mushroom == "?"]= NA

# finding the missing values in our dataset 
 summary(mushroom)
# only the stalk.root have missing values
head(mushroom)
 

table(mushroom$class,mushroom$gill.spacing)
table(mushroom$class,mushroom$odor)
table(mushroom$class,mushroom$habitat)
table(mushroom$class,mushroom$cap.shape)
# seems like odor is the most iportant variable for our model but let's explore the data before fitting the model
library(ggplot2)
# let's use plots to see whether the mushroom is edible or poisonous





  
  
p = ggplot(mushroom,aes(cap.shape,  
                    y=cap.surface, 
                    color=class))

p + geom_jitter(alpha=0.3) +  
  scale_color_manual(breaks = c('e','p'),
                     values=c('green','red'))
#  CapShape Bell is more likely to be edible and it alone is not sufficient and CapSurface Fibrous + CapShape Bell, Knobbed, or Sunken are also likely to be edible
p= ggplot(mushroom,aes(x=stalk.color.below.ring,  
                  y=stalk.color.above.ring,
                  color=class))

p + geom_jitter(alpha=0.3) +  
  scale_color_manual(breaks = c('e','p'),
                     values=c('green','red'))
# Gray is edible while while buff is poisonous
ggplot(mushroom, aes(x = odor, fill = class)) + geom_bar(position = "fill")
ggplot(mushroom, aes(x = habitat, fill = class)) + geom_bar(position = "fill")
ggplot(mushroom, aes(x = population, fill = class)) + geom_bar(position = "fill")
# as odor can mostly differentiate between edible and poisonous while mushrooms in wood are also edible


library(caTools)
set.seed(3005)
spl = sample.split(mushroom$class, SplitRatio = .7)
Train = subset(mushroom,spl==T)
Test = subset(mushroom,spl==F)
table(mushroom$class)/nrow(mushroom)
table(Train$class)/nrow(Train) 
table(Test$class)/nrow(Test)
# so let's fit a random forest model
library(randomForest)
# selected this model using forward selection
mod1 = randomForest(class ~ odor + habitat + population + spore.print.color + ring.type + 
                                 stalk.color.below.ring + stalk.surface.below.ring + gill.color + 
                                 gill.size , data = Train)
test1 = predict(mod1,newdata = Test)
table(Test$class,test1)
# clearly  the accuracy is 1
mod1$importance
# even though we have a hundred % accuracy but there are some variable which do not contribute at all
library(caret)
control = trainControl(method="repeatedcv", number=4, repeats=3)
Final_model = train(class ~ odor + spore.print.color + ring.type + stalk.surface.below.ring + gill.color + 
                      +                         gill.size , data=Train, method="rf", preProcess="scale", trControl=control)
# creating final modek using cross validation
Final_model
pred_test = predict(Final_model, newdata = Test)
 table(test$class, pred_test)
 mushroom_test = read.csv("C:/Users/Abhishek/Downloads/mushroom_test.csv")
 Final_pred = predict(Final_model, newdata = mushroom_test)
 mushroom_test$Predictions = Final_pred
 dim(mushroom_test)
 head(mushroom_test$Predictions, n = 15)
 mushroom_predictions = data.frame(Final_pred)
 str(mushroom_predictions)
 write.csv(mushroom_predictions, 'mushroom_predictions.csv')
 