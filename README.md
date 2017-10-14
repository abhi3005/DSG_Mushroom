# Predicting mushroom samples as edible or poisonous.

The aim of this project was to classify mushroom samples into two classes: edible and poisonous, using characteristics like odor, gill size, color etc. A dataset taken from Audubon Society Field Guide to North American Mushrooms(1981) is used for the purpose.

R (version 3.4.0) was used to create a model to classify the mushroom samples as edible or poisonous.

## Libraries used

  1. caTools: caTools package is used to divide the data into training and testing sets in such a way that both have same ratio of different levels in dependent variable.
  2. rpart and rpart.plot: These packages are used to create tree based models and make their visualisations.
  3. randomForest: This package is used to train the model in this problem.
  4. caret: caret package is used to perform cross validation.
  5. ggplot2: For making visualisations
