# Load the "randomForest" library into the R context.
library(randomForest)

# We can now go ahead and create the model. For this example, we will be using the built-in iris dataset. Feel free to try using other datasets!
# To create the model, we will use the randomForest function. It has a wide array of parameters for customization, but the simplest approach is just to provide it with a formula and the dataset to infer the probabilities from. This can be seen in the following code:

mylittleForest = randomForest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                              data = iris)
print(mylittleForest)

# As you can see, calling the print function on the model we created prints a summary of our model. This summary is quite informative -- it tells us how many trees were created, how many variables were tried at each split, the estimate of the error rate for the Out-of-Bag data (remember, it works as validation for our model!), and its confusion matrix.
#
# Another statistic that can be quite informative is the importance of each predictor for the prediction of our data points. This can be done by using the importance function, which can be seen in the following code:
print(importance(mylittleForest, type = 2))
par(mar=c(5,5,5,5))
plot(mylittleForest, uniform=TRUE)
randomForest::getTree(mylittleForest, labelVar = TRUE)



