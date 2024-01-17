install.packages("glmnet")
install.packages("caret")
install.packages("randomForest")

rm(list=ls())
library(MASS)
library(glmnet)
library(caret)
library(randomForest)

# load dataset
house_data = read.csv("data.csv", header= T)

# Number of records in the dataset
n = nrow(house_data)
n

## Pre processing steps

sum(house_data$price == 0) # 49 - count the number of zero values in the 'price' column
house_data$price[house_data$price == 0] = NA # replace all zero values with NA
sum(is.na(house_data$price)) # count the number of NA values in the 'price' column
house_data$price[is.na(house_data$price)] = mean(house_data$price, na.rm = TRUE) # replace all NA values with the mean of the 'price' column (excluding NAs)


# variation of prices over the years
library(dplyr)
house_data %>%
  group_by(yr_built) %>%
  summarize(avg_price = mean(price)) %>%
  ggplot(aes(x = yr_built, y = avg_price)) +
  geom_line() +
  labs(x = "Year Built", y = "Price", title = "Prices of houses over the years" ) +
  theme_bw()

# variation of prices in different cities 
library(ggplot2)
ggplot(house_data, aes(x = city, y = price, color = city)) +
  geom_point(size = 1, alpha = 0.5) +
  coord_flip() +
  labs(x = "City", y = "Price", color = "City") +
  ggtitle("House prices by City") +
  theme_bw()

#pie chart
nlevels(house_data$view)
table(house_data$view)
view_counts <- table(house_data$view)
par(mfrow = c(2,4))
par(mar = c(2,2,2,2))
pie(view_counts, labels = paste0(names(view_counts), ": ", view_counts), main = "Distribution of View")

#condition pie chart
nlevels(house_data$condition)
table(house_data$condition)
condition_counts <- table(house_data$condition)
par(mfrow = c(2,4))
par(mar = c(2,2,2,2))
pie(condition_counts, labels = paste0(names(condition_counts), ": ", condition_counts), main = "Distribution of Condition")

#waterfront pie chart
nlevels(house_data$waterfront)
table(house_data$waterfront)
waterfront_counts <- table(house_data$waterfront)
par(mfrow = c(2,4))
par(mar = c(2,2,2,2))
pie(waterfront_counts, labels = paste0(names(waterfront_counts), ": ", waterfront_counts), main = "Distribution of Waterfront")

#histogram
quant_house_data = house_data[,c("price","bedrooms","sqft_living","floors","sqft_lot", "sqft_above", "sqft_basement", "yr_renovated", "yr_built")]
par(mfrow=c(2,4)) # set up a 2x4 grid of plots
for (i in 2:ncol(quant_house_data)) {
  hist(quant_house_data[,i], main = colnames(quant_house_data)[i], xlab = colnames(quant_house_data)[i], ylab = "Frequency")
}
# Check for missing values in the price variable
sum(is.na(quant_house_data$price))

# Create a new dataset without missing values in the price variable
quant_house_data_clean = na.omit(quant_house_data)

# Create a histogram of the price variable without missing values
ggplot(quant_house_data_clean, aes(x=price)) +
  geom_histogram(binwidth=100000, fill="grey", color="white") +
  labs(x="Price", y="Frequency", title="Histogram of Price") +
  scale_x_continuous(labels = scales::comma, limits = c(0, 2000000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#correlation matrix
install.packages("corrplot")
library(corrplot)
corrplot(cor(house_data[, c("price", "bedrooms", "bathrooms", "sqft_living", "sqft_lot", "floors", "waterfront", "view", "condition", "sqft_above", "sqft_basement", "yr_built", "yr_renovated"
)], use='pairwise.complete.obs'), method = 'color', order = 'alphabet',addCoef.col = 'white', col = COL2('RdBu'), type ='lower')


#transform sqft basement - 1 if basement is there, 0 if basement is not there
house_data$sqft_basement = ifelse(house_data$sqft_basement>0,1,0)

# no missing data
sum(is.na(house_data))

# displaying details of the columns
str(house_data)

## Dropping date,street,city,statezip,country columns
house_data$street = NULL 
house_data$city = NULL 
house_data$statezip = NULL 
house_data$country = NULL 
house_data$date = NULL 

#linear model
lm = lm(price ~ ., data = house_data)
summary(lm)

set.seed(1) 
train.index=sample(n, round(0.7*n))
train= house_data[train.index,]
test= house_data[-train.index,]


#scaling the dataset, since the price value is large 

# initializing standard scaler
std_scaler = preProcess(train, method="scale")

# applying standard scaler to all numeric columns
train_df = predict(std_scaler, train)
test_df = predict(std_scaler, test)

head(train_df)

# Separate target variable and input features for training dataset
x_train = train_df[, c("bedrooms", "bathrooms", "sqft_living", "sqft_lot", "waterfront", "view", "yr_built", 
                       "condition", "sqft_above", "sqft_basement")]
y_train = train_df$price

# Separate target variable and input features for testing dataset
x_test = test_df[, c("bedrooms", "bathrooms", "sqft_living", "sqft_lot", "waterfront", "view", "yr_built", 
                     "condition", "sqft_above", "sqft_basement")]
y_test = test_df$price




### linear reg model 
lm= lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
         waterfront + view + yr_built + floors + condition + sqft_above + sqft_basement +
         yr_built, data=train_df)
summary(lm)
lm.pred= predict(lm, newdata=test_df)
y.test= test_df$price
mse_linear_reg = mean((lm.pred - y.test)^2) #test error
cat("MSE of Linear Regression: ", mse_linear_reg)



## Ridge
X = as.matrix(x_train)
Y = as.matrix(y_train)

fit = cv.glmnet(X, Y, alpha = 0)

# fit the ridge regression model
set.seed(1) 
ridge = glmnet(X, Y, alpha = 0, lambda = fit$lambda.min)

X_test = as.matrix(x_test)
predictions = predict(ridge, newx = X_test)

mse_ridge = mean((y_test - predictions)^2)
cat("MSE of Ridge Regression: ", mse_ridge)


## Lasso 

#converting them as matrix
lfit <- cv.glmnet(X, Y, alpha = 1)

# fit the ridge regression model
set.seed(1) 
lasso = glmnet(X, Y, alpha = 1, lambda = lfit$lambda.min)

X_test = as.matrix(x_test)

predictions = predict(lasso, newx = X_test)

mse_lasso = mean((y_test - predictions)^2)
cat("MSE of Lasso regression: ", mse_lasso)




##Random Forest
set.seed(1) 
rf = randomForest(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
                     waterfront + view + yr_built + floors + condition + sqft_above + sqft_basement +
                    yr_built, data = train_df, mtry = 11, importance = TRUE)

rf
yhat.rf = predict(rf, newdata = test_df)
mse_rf=mean((y.test- yhat.rf)^2)
mse_rf
cat("MSE of Random Forest: ", mse_rf)

importance(rf)

varImpPlot(rf)

#BIC

library(leaps)
###
regfit.full = regsubsets(price ~ ., data = house_data, nvmax = 12)  #nvmax is the maximum size of subsets to examine
reg.summary = summary(regfit.full)

###
reg.summary
###
names(reg.summary)
reg.summary$which
###
reg.summary$rsq  # r2 increases as number of variables increases.
reg.summary$adjr2   #adjusted R squared
reg.summary$bic   #BIC values
###
par(mfrow = c(1, 3))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
###
which.max(reg.summary$adjr2)
points(10, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(7, reg.summary$bic[7], col = "red", cex = 2, pch = 20)


(max(house_data$price))
house_data[which.max(house_data$price), "city"]

max_price <- max(house_data$price)
max_city <- house_data[which.max(house_data$price), "city"]

while (is.null(max_city)) {
  house_data <- house_data[house_data$price != max_price, ]
  max_price <- max(house_data$price)
  max_city <- house_data[which.max(house_data$price), "city"]
}

cat("City with maximum price is", max_city)
