# I'd like the final product to be colour coded
# in ggpubr or similar. I want it to work first.
# I don't think I know enough about how resamples() works.
# HRLR looks like a list of the models.
resamples <- resamples(HRLR)
# An error said "argument 1 is not a vector".
unlist(resamples)
# Base R does not look good enough.
bwplot(resamples, metric = "ROC")

# I'm not familiar with the resamples() function but you can do this below 
# you should just be able to do this to produce a colour-code AUC plot
# make sure that the list of algorithms in gnames is in alphabetical order
library(MLeval)
x <- evalm(HRLR, gnames = c("glmnet","ranger","svm","xgbTree"))

# this line makes the plot
x$roc