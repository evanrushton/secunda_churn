# Churn Analysis for Edtech Company

Subscriber data for a small startup was analyzed to look at the retention rate, predict who will churn, and highlight features that might indicate churn or help to retain users.

A random forest model was used to produve a variable importance plot.

A logistic regressoin model was used to find coefficients for the variables and lasso regularization was used to shrink some of the parameters for the logistic regression.

An NBD/Pareto model was used to get a probability that a user will churn based on their AppOpen history. The model fit wasn't great and seasonality needs to be addressed in the time series data.
