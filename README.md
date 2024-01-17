# -House-Price-Prediction-in-Washington-State-using-R
Applied advanced analytical techniques in R to develop comprehensive regression models, encompassing Linear Regression, Decision Tree, Ridge Regression, and Lasso Regression, for the Washington housing market. 

# Data Source:
Kaggle: https://www.kaggle.com/datasets/shree1992/housedata

Dataset has 4600 observations with 18 columns (17 predictor variables & 1 response variable). 

# Data pre-processing:
1. We observed that some records of the response variable(price) in the dataset consisted of
value 0 . So , we replaced those values with the mean of price column in the dataset
2. Predictors like street,city, statezip, country,date are dropped as we are interested in price
prediction of Washington state in general.
3. The categorical variables - waterfront,view,condition are transformed into dummy
variables with unique integers assigned to each category . So, no dummy coding is done
for categorical variables

# Model Planning & BUilding:
For this project, Linear Regression, Ridge Regression, Lasso regression and Random Forest modeling are done. 

# Results & Observations:
<img width="626" alt="image" src="https://github.com/Nishu1996/-House-Price-Prediction-in-Washington-State-using-R/assets/26360936/df421d4a-320b-41f2-b51e-2d5b9ab085a0">

We have taken Root Mean Squared Error (RMSE) instead of the Mean Squared Error (MSE) when predicting prices since the values of costs tend to be higher, and the RMSE is expressed in the same units as the variable being predicted.
Upon comparison, it was noted that Ridge Regression outperformed all the other models.

# Setting up & Running the Project:
Download the R code (FinalProject.R) from this repository and dataset from the Kaggle source. Open the R code file in RStudio and set the working directory in Rstudio to the location of the downloaded dataset. Press Command + Return key to execute each command.

