library(readr)
Family_Income_and_Expenditure <- read_csv("F:/R/Family Income and Expenditure.csv")
View(Family_Income_and_Expenditure)

attach(Family_Income_and_Expenditure)

#creating subdata set 
df=data.frame(`Bread and Cereals Expenditure`,`Total Rice Expenditure`,
              `Meat Expenditure`,`Total Fish and  marine products Expenditure`,
              `Fruit Expenditure`,`Vegetables Expenditure`,`Total Household Income`,
              Region,`Agricultural Household indicator`,`Medical Care Expenditure`,
              `Total Number of Family members`,`Members with age less than 5 year old`,
              `Members with age 5 - 17 years old`)

View(df)
colnames(df) <- c('brd_cer','rice','meat','fish','fruit','veg','income','region',
                   'agri_ind','med','t_mem','babies','kids')
View(df)
detach(Family_Income_and_Expenditure)


#splitting dataset into test and train
set.seed(111)
sample_size = round(nrow(df)*.80) # setting what is 80%
index = sample(seq_len(nrow(df)), size = sample_size)

train =df[index, ]
test = df[-index, ]



#excluding some outliers 

df2<-subset(train, income<=1.9*10^6)
View(df2)


attach(df2)

df7=(subset(df3, (food_exp==0)))
View(df7)   
food_exp=brd_cer+rice
df3=data.frame(food_exp,income,region,agri_ind,med,t_mem,babies,kids)
detach(df2)
attach(df3)
View(df3)
detach(df3)
df4=(subset(df3, !(income<food_exp+med)))
View(df4)
adults=df4$t_mem-(df4$babies+df4$kids)
food_exp=df4$food_exp
food_exp[food_exp==0]=quantile(food_exp,0.75)
income=df4$income

region=df4$region
agri_ind=as.factor(df4$agri_ind)
med=df4$med
kids=df4$kids
babies=df4$babies


df5=data.frame(food_exp,income,region,agri_ind,med,
               kids,babies,adults)



View(df5)
# Create training  feature matrices
# we use model.matrix(...)[, -1] to discard the intercept
X <- model.matrix(food_exp ~ ., df5)[, -1]


# transform y with log transformation
Y <- log(df5$food_exp)



library(recipes)  # for feature engineering

# Modeling packages
library(glmnet)   # for implementing regularized regression
library(caret)    # for automating the tuning process

# Model interpretability packages
library(vip)      # for variable importance

# Apply ridge regression to df5 data
ridge <- glmnet(
  x = X,
  y = Y,
  alpha = 0
)

plot(ridge, xvar = "lambda")

# Apply CV ridge regression to Ames data
ridge <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 0
)

# Apply CV lasso regression to Ames data
lasso <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 1
)

# plot results
par(mfrow = c(1, 2))
plot(ridge, main = "Ridge penalty\n\n")
plot(lasso, main = "Lasso penalty\n\n")



# Ridge model
min(ridge$cvm)       # minimum MSE

ridge$lambda.min     # lambda for this min MSE

ridge$cvm[ridge$lambda == ridge$lambda.1se]  # 1-SE rule

ridge$lambda.1se  # lambda for this MSE

# Lasso model
min(lasso$cvm)       # minimum MSE

lasso$lambda.min     # lambda for this min MSE

lasso$nzero[lasso$lambda == lasso$lambda.min] # No. of coef | Min MSE

lasso$cvm[lasso$lambda == lasso$lambda.1se]  # 1-SE rule

lasso$lambda.1se  # lambda for this MSE

lasso$nzero[lasso$lambda == lasso$lambda.1se] # No. of coef | 1-SE MSE

# Ridge model
ridge_min <- glmnet(
  x = X,
  y = Y,
  alpha = 0
)

# Lasso model
lasso_min <- glmnet(
  x = X,
  y = Y,
  alpha = 1
)

par(mfrow = c(1, 2))
# plot ridge model
plot(ridge_min, xvar = "lambda", main = "Ridge penalty\n\n")
abline(v = log(ridge$lambda.min), col = "red", lty = "dashed")
abline(v = log(ridge$lambda.1se), col = "blue", lty = "dashed")

# plot lasso model
plot(lasso_min, xvar = "lambda", main = "Lasso penalty\n\n")
abline(v = log(lasso$lambda.min), col = "red", lty = "dashed")
abline(v = log(lasso$lambda.1se), col = "blue", lty = "dashed")

# for reproducibility
set.seed(123)

# grid search across 
cv_glmnet <- train(
  x = X,
  y = Y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

# model with lowest RMSE
cv_glmnet$bestTune

# results for model with lowest RMSE
cv_glmnet$results %>%
  filter(alpha == cv_glmnet$bestTune$alpha, lambda == cv_glmnet$bestTune$lambda)


# plot cross-validated RMSE
ggplot(cv_glmnet)

vip(cv_glmnet, num_features = 20, geom = "point")


# Fit the final ridge model on the training data
model <- glmnet(X, Y, alpha = 0, lambda = ridge$lambda.min)
# Display regression coefficients
coef(model)

# Fit the final lasso model on the training data
model <- glmnet(X, Y, alpha = 1, lambda = lasso$lambda.min)
# Display regression coefficients
coef(model)



########################excluding false data###################################

df6=subset(df5, !(income<= food_exp+med))


# Create training  feature matrices
# we use model.matrix(...)[, -1] to discard the intercept
X <- model.matrix(food_exp ~ ., df6)[, -1]

# transform y with log transformation
Y <- log(df6$food_exp)



































