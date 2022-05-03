
# Test script for final GAM model -----------------------------------------

library(stats)
library(ROCR)
library(caret)

# Select correct model for testing
mod <- mod_bin

# to assess model performance in the form of the true positive rate and the false positive rate
pr <- as.numeric(predict(mod, mod_df, type="response"))            

# the final model is used to predict the data on the response scale (i.e. a value between 0 and 1)
pred <- prediction(pr, mod_df$dive_bool)

# to specify the vector of predictions (pr) and the vector of labels (i.e. the observed values "Pres")
perf <- performance(pred, measure="tpr", x.measure="fpr")         

# to plot the ROC curve
plot(perf, colorize = TRUE, print.cutoffs.at = c(0.1,0.2,0.3,0.4,0.5))

# Choice of the best cut-off probability

y <- as.data.frame(perf@y.values)
x <- as.data.frame(perf@x.values)

# to calculate the distance between the 45? line and the ROC curve
fi <- atan(y/x) - pi/4                                             

# to calculate the distance to the ROC curve
L <- sqrt(x^2+y^2)                                                 

# to calculate the length of the line joining the origin to the point (x;y) on the ROC curve
d <- L*sin(fi)                                                     

# pull out the threshold with the greatest distance to the 45? line
ddf <- as.data.frame(d)
ind <- which(ddf == max(na.omit(ddf)))

# the alpha values represent the corresponding cut-offs
alpha <- as.data.frame(perf@alpha.values)                            

# to identify the alpha value (i.e. the cut-off) that corresponds to the maximum AUC
threshold <- alpha[ind,]                                           

# threshold = Best cutoff
# This value can now be used to build the confusion matrix:

# to build a matrix with 3 columns and n rows, where n is the dimension of the data set
DATA <- matrix(0,nrow(mod_df),3)                                       
DATA <- as.data.frame(DATA)
names(DATA) <- c("plotID","Observed","Predicted")

# the first column is filled with an ID value that is unique for each row
DATA$plotID <- 1:nrow(mod_df)                                          

# the second column reports the observed response (0s and 1s)
DATA$Observed <- mod_df$dive_bool                                       

# the third column reports the predictions
DATA$Predicted <- pr    

# split out pre into binary predictive values
pre <- DATA$Predicted
pre <- (ifelse(pre > threshold, 1, 0))
pre <- as.factor(as.numeric(pre))

# print working threshold
print(threshold)

# prints output of confusion matrix and related stats
print(caret::confusionMatrix((pre), as.factor(DATA$Observed)))

#print AUC
auc <- performance(pred, measure="auc")
print(paste("auc: ", auc@y.values))

# blows away mess of associated objects
rm(mod, pr, pred, perf, y, x, fi, L, d, ddf, ind, alpha, threshold, DATA, pre, auc)
