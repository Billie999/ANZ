# title: "Predicitve Analytics at ANZ"
# author: "Biljana Simonovikj"
# date: "19/12/2020"

#------------------------------------------------------
# Install and load the libraries used in this project:
#------------------------------------------------------
library(AppliedPredictiveModeling)
library(ggplot2)
library(tidyverse)
library(scales)
library(dplyr)
library(funModeling)
library(ggpubr)
library(gridExtra)
library(caret)
library(corrplot)
library(lattice)
library(dplyr)
library(tidyr)
library(e1071)
library(graphics)
library(hrbrthemes)
library(glmnet)
library(kernlab)
library(gbm)
library(Cubist)
devtools::install_github('rstudio/rmarkdown')
rm(list = ls())
#### 1. Import the Dataset
# Import the dataset
anz_data <- read.csv("/Users/Biljana/R files/PA/ANZ synthesised transaction dataset.csv",
                     header = TRUE, na.strings = c("","NA"))

# Change the format of date column
anz_data$date <- as.Date(anz_data$date, format = "%d/%m/%Y")
knitr::kable(head(anz_data[1:6, 1:7]),
             caption = "Table 1: Glimse at the first seven columns of the
             ANZ synthesised transaction dataset",
             align = 'c', format = "markdown")
#### 1.1. Correlations between annual salary and various customer attributes
##### Annual salary by customer
# Create the initial dataframe to store the results
df_customer = data.frame(customer_id = unique(anz_data$customer_id))

# Create a mode function to find out what is the salary payment frequency
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Create a loop to process through all salary payments for each customer
for (i in seq(nrow(df_customer))) {
  transaction_data <- anz_data[anz_data$customer_id == df_customer$customer_id[i]
                               & anz_data$txn_description == "PAY/SALARY",c("amount","date")] %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(amount = sum(amount))
  total_sum <- sum(transaction_data$amount)
  count = dim(transaction_data)[1]
  if (count == 0) {
    df_customer$frequency[i] = NA
    df_customer$level[i] = NA
  } else {
    s = c()
    lvl = c()
    for (j in seq(count - 1)) {
      s = c(s,(transaction_data$date[j + 1] - transaction_data$date[j]))
      lvl = c(lvl,transaction_data$amount[j])}
    lvl = c(lvl,tail(transaction_data$amount,n = 1))
    df_customer$frequency[i] = mode(s)
    df_customer$level[i] = mode(lvl)
  }
}

df_customer$annual_salary = df_customer$level / df_customer$frequency * 365.25
df_customer_table <- as.data.frame(df_customer)
knitr::kable(head(df_customer_table, n = 10),
             caption = "Table 2: Calculated annual salary by customer",
             align = 'c', format = "markdown")

# Distribution of customers' annual salary
hist(df_customer$annual_salary[!is.na(df_customer$annual_salary)],breaks = c(seq(28000,140000,by = 10000)),
     col = rgb(0,0,0,0.5), main = "Histogram of Annual Salary", xlab = 'Income, ($)')

##### Annual salary by various customer attributes
# Create a dataframe to summarize customers' consumption behavior
df_attributes <- anz_data %>%
  # use anz_data to summarize customers' consumption behavior
  dplyr::select(customer_id, gender, age, amount, date, balance) %>%
  group_by(customer_id) %>%
  dplyr::mutate(num_trans = n(),
                avg_weekly_trans = round(7*n()/length(unique(date)),0),
                max_amount = max(amount),
                num_large_trans = sum(amount > 100),
                # an arbitrary $100 benchmark is selected
                use_num_day = length(unique(date)),
                avg_trans_amount = mean(amount, na.rm = TRUE),
                med_balance = median(balance,na.rm = TRUE)) %>%
  dplyr::select(-c("amount","date","balance")) %>%
  unique()

# Assign gender as binary numeric variable
df_attributes$gender_num <- ifelse(df_attributes$gender == "M",1,0)

# Assign age by groups as binary numeric variables
df_attributes$age_below20 <- ifelse(df_attributes$age < 20,1,0)
df_attributes$age_btw20n40 <- ifelse(df_attributes$age >= 20 & df_attributes$age < 40,1,0)
df_attributes$age_btw40n60 <- ifelse(df_attributes$age >= 40 & df_attributes$age < 60,1,0)

# Merge all the attributes into single dataframe and select relevant attributes
customer_data <- merge(df_customer, df_attributes)

# Remove columns: customer_id, frequency and level
customer_data <- customer_data[ , c(4:17)] # 14 columns left


# 1. Scatter plot of annual salary versus age
ggplot(customer_data, aes(x = age, y = annual_salary,
                          color = gender)) +  geom_point(size = 2) +
  stat_smooth(aes(group = 1), method = "lm", se = TRUE, position = "identity", col = "grey56") +
  theme_ipsum() +
  labs(title = "Annual Salary vs Age",
       subtitle = "ANZ Customer Database", y = "Annual salary, ($)", x = "Age") +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(vjust = 2, face = 'bold'),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(),
    axis.title.x = element_text(size = 12, margin = margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, margin = margin(0,10,0,0)))

# 2. Scatter plot of annual salary versus avg_trans_amount
ggplot(customer_data, aes(x = avg_trans_amount, y = annual_salary,
                          color = gender)) +  geom_point(size = 2) +
  stat_smooth(aes(group = 1), method = "lm", se = TRUE, position = "identity", col = "grey56") +
  theme_ipsum() +
  labs(title = "Annual Salary vs Average Transactions Amount",
       subtitle = "ANZ Customer Database", y = "Annual salary, ($)", x = "Avgerage transactions amount, ($)") +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(vjust = 2, face = 'bold'),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(),
    axis.title.x = element_text(size = 12, margin = margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, margin = margin(0,10,0,0)))

# 3. Scatter plot of annual salary versus maximum transactions amount
ggplot(customer_data, aes(x = max_amount, y = annual_salary,
                          color = gender)) +  geom_point(size = 2) +
  stat_smooth(aes(group = 1), method = "lm", se = TRUE, position = "identity", col = "grey56") +
  theme_ipsum() +
  labs(title = "Annual Salary vs Maximum Transactions Amount",
       subtitle = "ANZ Customer Database", y = "Annual salary, ($)", x = "Maximum transactions amount, ($)") +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(vjust = 2, face = 'bold'),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(),
    axis.title.x = element_text(size = 12, margin = margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, margin = margin(0,10,0,0)))

# 4. Scatter plot of annual salary versus number of transactions by customer
ggplot(customer_data, aes(x = num_trans, y = annual_salary,
                          color = gender)) +  geom_point(size = 2) +
  stat_smooth(aes(group = 1), method = "lm", se = TRUE, position = "identity", col = "grey56") +
  theme_ipsum() +
  labs(title = "Annual Salary vs Transactions Count",
       subtitle = "ANZ Customer Database", y = "Annual salary, ($)", x = "Number of transactions by customer") +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(vjust = 2, face = 'bold'),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(),
    axis.title.x = element_text(size = 12, margin = margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, margin = margin(0,10,0,0)))

#### 1.2 Validation Dataset
# First to eliminate categorical gender variable that was kept for plotting scatter plots above
customer_data <- customer_data[ ,-2] # 13 columns left

# Create train and test datasets
set.seed(7)
validationIndex <- createDataPartition(customer_data$annual_salary, p = 0.80, list = FALSE)
# select 20% of the data for validation
validation <- customer_data[-validationIndex,]
# use the remaining 80% of data to training and testing the models
dataset <- customer_data[validationIndex,]

#### 2. Analyze Data - Descriptive Statistics
##### Dimensions of the dataset
dim(dataset)
##### List types of each attribute
sapply(dataset, class)
##### Summarize attribute distributions
sum_m <- sapply(dataset, summary)
df_sum <- as.data.frame(sum_m)
t(df_sum)
##### Calculate skewness for each variable
skew <- apply(dataset, 2, skewness)
print(skew)

#### Unimodal and Multimodal Data Visualisations
##### Histograms
# Plot histograms for each input (explanatory) variable
par(mfrow = c(4,3), oma = c(4,3,4,3), mar = c(1,4,3,2))
for (i in 2:13) {
  graphics::hist(dataset[,i], main = names(dataset[i]), xlab = "", col = "grey50")
}

##### Density Plots
# Density plots for each variable
mycolors = c('red','darkturquoise','blue',"orange", "magenta1","green", "red",
             "darkturquoise", "blue", "orange", "magenta1", "green", "red")
par(mfrow = c(4,3), oma = c(0,1,0,1), mar = c(1,6,3,2))
for (i in 2:13) {
  graphics::plot(density(dataset[,i]), main = names(dataset)[i], col = mycolors[i])
}
##### Box Plots
# Box plots for each variable
par(mfrow = c(4,3), oma = c(0,1,0,1), mar = c(1,6,2,2))
for (i in 2:13) {
  graphics::boxplot(dataset[,i], main = names(dataset)[i], col = mycolors[i])
}
##### Scatter Plots
# Scactterplot matrices for all attributes
dataset_scatter <- sapply(dataset[ , 1:9], jitter)
pairs(dataset_scatter[ , 1:9], col = mycolors)
##### Correlation Plot
# Correlation between all of the numeric attributes except annual_salary
annual_salary <- dataset$annual_salary # assign annual_salary variable as vector

# Create a dataframe for correlation plot
dataset_cor <- dataset[ ,-1]
# Plot the correlation matrix
correlation_matrix <- cor(dataset_cor)
corrplot.mixed(correlation_matrix, order = "hclust", tl.pos = "lt",
               upper = "ellipse")

#### Summary of Ideas
#### 3. Evaluate Algorithms
##### Baseline
# Run algorithms using 10-fold cross validation
trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
metric <- "RMSE"

# LM
set.seed(7)
fit.lm <- train(annual_salary~., data = dataset, method = "lm",
                metric = metric, preProc = c("center","scale"), trControl = trainControl)
# GLM
set.seed(7)
fit.glm <- train(annual_salary~., data = dataset, method = "glm",
                 metric = metric, preProc = c("center","scale"), trControl = trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(annual_salary~., data = dataset, method = "glmnet", metric = metric,
                    preProc = c("center", "scale"), trControl = trainControl)
# SVM
set.seed(7)
fit.svm <- train(annual_salary~., data = dataset, method = "svmRadial", metric = metric,
                 preProc = c("center", "scale"), trControl = trainControl)
# CART
set.seed(7)
grid <- expand.grid(.cp = c(0, 0.05, 0.1))
fit.cart <- train(annual_salary~., data = dataset, method = "rpart", metric = metric, tuneGrid = grid,
                  preProc = c("center", "scale"), trControl = trainControl)

# KNN
set.seed(7)
fit.knn <- train(annual_salary~., data = dataset, method = "knn", metric = metric, preProc = c("center","scale"), trControl = trainControl)


# Compare algorithms
results <- resamples(list(LM = fit.lm, GLM = fit.glm, GLMNET = fit.glmnet, SVM = fit.svm,
                          CART = fit.cart, KNN = fit.knn))
summary(results)
dotplot(results)
##### Feature Selection
# Remove correlated attributes
set.seed(7)
cutoff <- 0.70
highly_correlated <- findCorrelation(correlation_matrix, cutoff = cutoff)
for (value in highly_correlated) {
  print(names(dataset_cor)[value])
}
# Create a new dataset without highly corrected features
dataset_features <- dataset_cor[,-highly_correlated]
# # Attach annual_salary to the df
dataset_features <- cbind(dataset_features, annual_salary)

# Summary of estimated model accuracy
# Run algorithms using 10-fold cross validation
trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
metric <- "RMSE"
# lm
set.seed(7)
fit.lm <- train(annual_salary~., data = dataset_features, method = "lm", metric = metric,
                preProc = c("center", "scale"), trControl = trainControl)
# GLM
set.seed(7)
fit.glm <- train(annual_salary~., data = dataset_features, method = "glm", metric = metric,
                 preProc = c("center", "scale"), trControl = trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(annual_salary~., data = dataset_features, method = "glmnet", metric = metric,
                    preProc = c("center", "scale"), trControl = trainControl)
# SVM
set.seed(7)
fit.svm <- train(annual_salary~., data = dataset_features, method = "svmRadial", metric = metric,
                 preProc = c("center", "scale"), trControl = trainControl)
# CART
set.seed(7)
grid <- expand.grid(.cp = c(0, 0.05, 0.1))
fit.cart <- train(annual_salary~., data = dataset_features, method = "rpart", metric = metric,
                  tuneGrid = grid, preProc = c("center", "scale"), trControl = trainControl)
# KNN
set.seed(7)
fit.knn <- train(annual_salary~., data = dataset_features, method = "knn", metric = metric,
                 preProc = c("center", "scale"), trControl = trainControl)

# Compare algorithms
feature_results <- resamples(list(LM = fit.lm, GLM = fit.glm, GLMNET = fit.glmnet, SVM = fit.svm,
                                  CART = fit.cart, KNN = fit.knn))

summary(feature_results)
dotplot(feature_results)
##### Box Cox
# Run algorithms using 10-fold cross validation
trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
metric <- "RMSE"
# lm
set.seed(7)
fit.lm <- train(annual_salary~., data = dataset, method = "lm", metric = metric,
                preProc = c("center", "scale", "BoxCox"), trControl = trainControl)
# GLM
set.seed(7)
fit.glm <- train(annual_salary~., data = dataset, method = "glm", metric = metric,
                 preProc = c("center", "scale", "BoxCox"), trControl = trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(annual_salary~., data = dataset, method = "glmnet", metric = metric,
                    preProc = c("center", "scale", "BoxCox"), trControl = trainControl)
# SVM
set.seed(7)
fit.svm <- train(annual_salary~., data = dataset, method = "svmRadial", metric = metric,
                 preProc = c("center", "scale", "BoxCox"), trControl = trainControl)
# CART
set.seed(7)
grid <- expand.grid(.cp = c(0, 0.05, 0.1))
fit.cart <- train(annual_salary~., data = dataset, method = "rpart",
                  metric = metric, tuneGrid = grid, preProc = c("center", "scale", "BoxCox"),
                  trControl = trainControl)

# KNN
set.seed(7)
fit.knn <- train(annual_salary~., data = dataset, method = "knn", metric = metric,
                 preProc = c("center", "scale", "BoxCox"), trControl = trainControl)

# Compare algorithms
transformResults <- resamples(list(LM = fit.lm, GLM = fit.glm, GLMNET = fit.glmnet, SVM = fit.svm,
                                   CART = fit.cart, KNN = fit.knn))
summary(transformResults)
dotplot(transformResults)
##### Improve Results with Tuning
##### Estimated accuracy of GLMNET
print(fit.glmnet)

# We first setup our cross-validation strategy
trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
metric <- "RMSE"
set.seed(7)
tuneGridb <- expand.grid(.alpha = seq(0, 1, 0.05),
                         .lambda = c(1:10/10, length = 250))
anz_elnet_int = train(annual_salary ~., data = dataset, method = "glmnet", metric = metric,
                      preProc = c("BoxCox"), trControl = trainControl, tuneLength = 10,
                      tuneGrid = tuneGridb)


plot(anz_elnet_int, main = "Algorithm Tuning Results for GLMNET on the ANZ Dataset")

##### Estimated accuracy after tuning the parameters
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

get_best_result(anz_elnet_int)

#### Ensmble Methods

# try ensembles
trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
metric <- "RMSE"
# Random Forest
set.seed(7)
fit.rf <- train(annual_salary~., data = dataset, method = "rf", metric = metric,
                preProc = c("BoxCox"), trControl = trainControl)
# Stochastic Gradient Boosting
set.seed(7)
fit.gbm <- train(annual_salary~., data = dataset, method = "gbm", metric = metric,
                 preProc = c("BoxCox"), trControl = trainControl, verbose = FALSE)
# Cubist
set.seed(7)
fit.cubist <- train(annual_salary~., data = dataset, method = "cubist", metric = metric,
                    preProc = c("BoxCox"), trControl = trainControl)
# Compare algorithms
ensemble_results <- resamples(list(RF = fit.rf, GBM = fit.gbm, CUBIST = fit.cubist))
summary(ensemble_results)
dotplot(ensemble_results)

##### Look for parameters used for Cubist
# Cubist's parameters
print(fit.cubist)
# Tune the Cubist algorithm
trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
metric <- "RMSE"
set.seed(7)
grid <- expand.grid(.committees = seq(5, 25, by = 1), .neighbors = c(3, 5, 7))
tune.cubist <- train(annual_salary~., data = dataset, method = "cubist", metric = metric,
                     preProc = c("BoxCox"), tuneGrid = grid, trControl = trainControl)
print(tune.cubist)
plot(tune.cubist, main = "Tuning the Parametres of the Cubist Algorithm on the ANZ Dataset")

# Prepare the data transform using training data
set.seed(7)
x <- dataset[,2:13]
y <- dataset[,1]
preprocessParams <- preProcess(x, method = c("BoxCox"))
transX <- predict(preprocessParams, x)
# train the final model
final_model <- cubist(x = transX, y = y, committees = 24)
print(final_model)
summary(final_model)

# Transform the validation dataset
set.seed(7)
valX <- validation[,2:13]
trans_valX <- predict(preprocessParams, valX)
valY <- validation[,1]
# Use final model to make predictions on the validation dataset
predictions <- predict(final_model, newdata = trans_valX, neighbors = 7)
predictions

# Calculate RMSE
rmse <- RMSE(predictions, valY)
r2 <- R2(predictions, valY)
##### Estimated accuracy on validation dataset
rmse <- round(rmse, 2)
r2 <- round(r2, 4)
values <- c(rmse, r2)
names <- c("RMSE", "Rsquared")
setNames(values, names)
plot(final_model)
