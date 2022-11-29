library(readr)
library(recipes)  # for feature engineering

# Modeling packages
library(glmnet)   # for implementing regularized regression
library(caret)    # for automating the tuning process

# Model interpretability packages
library(vip)      # for variable importance
library(ggplot2)
#for partial dependancy plots
library(pdp)

Family_Income_and_Expenditure <- read_csv("F:/R/Family Income and Expenditure.csv")


attach(Family_Income_and_Expenditure)

#creating subdata set 
df=data.frame(`Bread and Cereals Expenditure`,`Total Rice Expenditure`,
              `Meat Expenditure`,`Total Fish and  marine products Expenditure`,
              `Fruit Expenditure`,`Vegetables Expenditure`,`Total Household Income`,
              Region,`Agricultural Household indicator`,`Medical Care Expenditure`,
              `Total Number of Family members`,`Members with age less than 5 year old`,
              `Members with age 5 - 17 years old`)

#rename coloumns
colnames(df) <- c('brd_cer','rice','meat','fish','fruit','veg','income','region',
                  'agri_ind','med','t_mem','babies','kids')

detach(Family_Income_and_Expenditure)


#splitting dataset into test and train
set.seed(111)
sample_size = round(nrow(df)*.80) # setting what is 80%
index = sample(seq_len(nrow(df)), size = sample_size)

train =df[index, ]
test = df[-index, ]

#excluding some outliers 

train<-subset(train, income<=1.5*10^6)
test<- subset(test,income<=1.5*10^6)


#combine brd_cer and rice


train<-data.frame(train$brd_cer+train$rice,train$income,train$region,
                  as.factor(train$agri_ind),train$med,
                  (train$t_mem-train$babies-train$kids),train$babies,train$kids
                  )


test<-data.frame(test$brd_cer+test$rice,test$income,test$region,
                  as.factor(test$agri_ind),test$med,
                  (test$t_mem-test$babies-test$kids),test$babies,test$kids
)



#rename coloumns

colnames(train) <- c('food_exp','income','region','agri_ind','med','adults',
                     'babies','kids')

colnames(test) <- c('food_exp','income','region','agri_ind','med','adults',
                     'babies','kids')

#replace food_exp = 0 with median(food_expnd)

train$food_exp[train$food_exp==0] <- median(train$food_exp)
test$food_exp[test$food_exp==0] <- median(train$food_exp)



# Create training  feature matrices
# we use model.matrix(...)[, -1] to discard the intercept
#creaTINg design matrix
x_train <- model.matrix(food_exp ~ ., train)[, -1]
x_test <- model.matrix(food_exp ~ ., test)[, -1]


# transform y with log transformation
y_train <- log(train$food_exp)
y_test <- log(test$food_exp)


# Apply CV ridge regression to train data
ridge <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 0
)

# Apply CV lasso regression to train data
lasso <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 1
)

# plot results
par(mfrow = c(1, 2))
plot(ridge, main = "Ridge penalty\n\n")
plot(lasso, main = "Lasso penalty\n\n")


# Compute R^2 for test set
eval_results <- function(true, predicted) {
  
  RMSE = RMSE(predicted,true)
  MSE = RMSE(predicted,true)^2
  R_square = as.vector(R2(predicted,true))
  
  # Model performance metrics
  data.frame(
    MSE = MSE,
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}



# Ridge model
min(ridge$cvm)       # minimum MSE

ridge$lambda.min     # lambda for this min MSE

ridge$cvm[ridge$lambda == ridge$lambda.1se]  # 1-SE rule

ridge$lambda.1se  # lambda for this MSE


# Prediction and evaluation on test data with ridge
predictions_test <- predict(ridge, s = ridge$lambda.min, newx = x_test)
eval_results(y_test, predictions_test)

#coefficient of ridge
coef(ridge)


# Lasso model
min(lasso$cvm)       # minimum MSE

lasso$lambda.min     # lambda for this min MSE

lasso$nzero[lasso$lambda == lasso$lambda.min] # No. of coef | Min MSE

lasso$cvm[lasso$lambda == lasso$lambda.1se]  # 1-SE rule

lasso$lambda.1se  # lambda for this MSE

lasso$nzero[lasso$lambda == lasso$lambda.1se] # No. of coef | 1-SE MSE

# Prediction and evaluation on test data with lasso
predictions_test <- predict(lasso, s = lasso$lambda.min, newx = x_test)
eval_results(y_test, predictions_test)

#coefficient of lasso
coef(lasso)



#apply Elastic net to train data

# for reproducibility
set.seed(123)

# grid search across 
cv_glmnet <- train(
  x = x_train,
  y = y_train,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

# model with lowest RMSE
cv_glmnet$bestTune

# results for model with lowest RMSE 
#MSE of elastic net
cv_glmnet$results %>%
  filter(alpha == cv_glmnet$bestTune$alpha, lambda == cv_glmnet$bestTune$lambda)

# Prediction and evaluation on test data with Elastic net
predictions_test <- predict(cv_glmnet,x_test)
eval_results(y_test, predictions_test)


# Coefficient Elastic net
coef(cv_glmnet$finalModel, cv_glmnet$bestTune$lambda)

#Saving the model
saveRDS(cv_glmnet, file = "./cv_glmnet.rda")


#variable importance plots

#ridge
vip(ridge, num_features = 20, geom = "point")
#lasso
vip(lasso, num_features = 20, geom = "point")
#Elastic net
vip(cv_glmnet, num_features = 20, geom = "point")



partial(cv_glmnet, "adults", grid.resolution = 20, plot = TRUE)
partial(cv_glmnet, "kids", grid.resolution = 20, plot = TRUE)
partial(cv_glmnet, "income", grid.resolution = 20, plot = TRUE)





#linear model
set.seed(123)
cv_model3 <- train(
  log(food_exp) ~ ., 
  data = train, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

summary(cv_model3)
cv_model3$results 

#principal component regression
set.seed(123)
cv_model_pcr <- train(
  log(food_exp) ~ ., 
  data = train, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 100
)


# model with lowest RMSE
cv_model_pcr$bestTune

cv_model_pcr$results %>%
  dplyr::filter(ncomp == pull(cv_model_pcr$bestTune))


# plot cross-validated RMSE
ggplot(cv_model_pcr)


# paritial least square method
set.seed(123)
cv_model_pls <- train(
  log(food_exp) ~ ., 
  data = train, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 30
)
cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))
