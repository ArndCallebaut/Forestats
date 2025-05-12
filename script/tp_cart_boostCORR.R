## Forestinecology - TP 1
### CART and boosting

## set seed
set.seed(20250505)

## load libraries
library(tidyverse)
library(rpart)
library(caret)
library(lightgbm)
library(DALEX)

# 1. CART tree - iris data

## load data
data("iris")
summary(iris)
## construct a simple CART tree
tree_basic <- rpart(Species ~., data = iris)

## plot and discuss result
rpart.plot::rpart.plot(tree_basic)

## get prediction and prediction error
pred_tree <- predict(tree_basic, type = "class")
caret::confusionMatrix(iris$Species, pred_tree)

## create 10 train-test split to evaluate model prediction accuracy
## fill-in the dots
err <- NULL
for(i in 1:10){
  train_id <- sample(1:nrow(iris),
                     size = ceiling(0.8 * nrow(iris)))
  train_dat <- iris[train_id,]
  test_dat <- iris[-train_id,]
  model <- rpart(Species~., train_dat)
  pred_species <- predict(model, newdata = test_dat, type = "class")
  acc <- sum(pred_species == test_dat$Species) / nrow(test_dat)
  err <- c(err, acc)
}
mean(err)

## tweaking tree architecture
## play with the available argument and explore how it affects
## the resulting tree, aim at constructing a perfect tree
## fill in the dots
control <- rpart.control(cp = 0.001, minsplit = 2)
tree_tweak <- rpart(Species ~., data = iris, control = control)
rpart.plot::rpart.plot(tree_tweak, extra = 1)


# 2. Boosted trees - ozone and St Laurent data

## 2.1 first Ozone

## load data
data("airquality")
## replace NA by median
airquality$Ozone[is.na(airquality$Ozone)] <- median(airquality$Ozone, na.rm = TRUE)
airquality$Solar.R[is.na(airquality$Solar.R)] <- median(airquality$Solar.R, na.rm = TRUE)

## create train/test split
train_id <- sample(1:nrow(airquality),
                   size = ceiling(0.8*nrow(airquality)))
dat_train <- lgb.Dataset(data = data.matrix(airquality[train_id,!names(airquality) == "Ozone"]),
                         label = airquality$Ozone[train_id])
dat_test <- lgb.Dataset(data = data.matrix(airquality[-train_id,!names(airquality) == "Ozone"]),
                        label = airquality$Ozone[-train_id])
## first a simple fit, explore and interprete output
gbm_basic <- lgb.train(data = dat_train,
                       valids = list(test = dat_test))
gbm_basic

## get test prediction
pred <- predict(gbm_basic,
                newdata = data.matrix(airquality[-train_id,!names(airquality) == "Ozone"]))

## compute model performance metrics
nrmse <- RMSE(pred, airquality$Ozone[-train_id]) / mean(airquality$Ozone)
abs_err <- MAE(pred, airquality$Ozone[-train_id])
r2 <- R2(pred, airquality$Ozone[-train_id])

## get variable importance
imp <- lgb.importance(gbm_basic)
### Gain: This measures how much a feature contributes to reducing the loss function. Higher gain = more predictive power.
### Cover: Measures how frequently and widely a feature influences decisions.
### Frequency: Higher frequency indicates that the feature was selected often by the algorithm, though not necessarily with the highest impact.
lgb.plot.importance(imp)

## get interpretation of the effect of the variable on given predictions
int <- lgb.interprete(model = gbm_basic,
                      data = data.matrix(airquality[-train_id,!names(airquality) == "Ozone"]),
                      1:4)
lgb.plot.interpretation(int[[1]])
## Temp. reduce the ozone concentration by 10 ppb compared to the average for this prediction

## explore response curve along gradient of temperature
library(DALEX)

explainer <- explain(
  model = gbm_basic,
  data = data.matrix(airquality[, !names(airquality) == "Ozone"]),
  y = airquality$Ozone,
  label = "LightGBM"
)

pdp_temp <- model_profile(explainer, variables = "Temp")

## plot this together with observed data
ggplot(airquality) +
  geom_point(aes(x=Temp, y=Ozone)) +
  geom_path(data=pdp_temp$agr_profiles, aes(x=`_x_`, y=`_yhat_`),
            color = "red", linewidth = 1.25, linetype = "dashed")


## then tweak gbm params, see https://lightgbm.readthedocs.io/en/latest/Parameters.html
## for the full list of options
params <- list(
  objective = "regression", # what type of model to fit
  metric = "rmse", # what metric to optimize
  learning_rate = 0.01, # how fast does the algorithm learn from new trees
  num_leaves = 35, # of many leaves per tree
  feature_fraction = 0.75, # equivalent to mtry in random forest
  bagging_fraction = 0.75 # proportion of observation to select by bootstrap
)

gbm_adv <- lgb.train(
  params = params,
  data = dat_train,
  valids = list(test = dat_test),
  nrounds = 1000,
  early_stopping_rounds = 25
)

## compare with earlier results
pred <- predict(gbm_adv,
                newdata = data.matrix(airquality[-train_id,!names(airquality) == "Ozone"]))

## compute model performance metrics
nrmse_adv <- RMSE(pred, airquality$Ozone[-train_id]) / mean(airquality$Ozone)
abs_err_adv <- MAE(pred, airquality$Ozone[-train_id])
r2_adv <- R2(pred, airquality$Ozone[-train_id])

## get variable importance
imp_adv <- lgb.importance(gbm_adv)
lgb.plot.importance(imp_adv)
lgb.plot.importance(imp)
## slight changes in the importance but the ordering of variable importance remains 



## 2.2 St Laurent data

### load data
st_lau <- read.csv2("LIF/Presentation/ecostat_2025/data/StLaurent.csv")
st_lau$temperature[is.na(st_lau$temperature)] <- median(st_lau$temperature,
                                                        na.rm = TRUE)
summary(st_lau)
ggplot(st_lau, aes(x=longitude, y=latitude, color=factor(starfish))) +
  geom_point()

## set data vars
train_id <- sample(1:nrow(st_lau), size = ceiling(0.8 * nrow(st_lau)))
covars <- which(!names(st_lau) %in% c("starfish", "urchin"))
dat_train <- lgb.Dataset(data = data.matrix(st_lau[train_id, covars]),
                         label = st_lau$starfish[train_id])
dat_test <- lgb.Dataset(data = data.matrix(st_lau[-train_id, covars]),
                        label = st_lau$starfish[-train_id])

## set params for the BRT
## fill in the dots
params <- list(
  objective = "binary",
  metric = "auc",
  learning_rate = 0.1,
  num_leaves = 35,
  feature_fraction = 0.75,
  bagging_fraction = 0.75
)

gbm_lau <- lgb.train(
  params = params,
  data = dat_train,
  valids = list(test = dat_test),
  nrounds = 1000,
  early_stopping_rounds = 50
)

## get model performance
pred <- predict(gbm_lau, newdata = data.matrix(st_lau[-train_id, covars]),
                type = "class")
caret::confusionMatrix(factor(pred), factor(st_lau$starfish[-train_id]))
## large number of false positive, absence are often confused as presence

## predict over space
pred_xy <- predict(gbm_lau, newdata = data.matrix(st_lau[,covars]))
st_lau$pred <- pred_xy
ggplot(st_lau, aes(x=longitude, y=latitude, color=pred)) +
  geom_point() +
  scale_color_continuous(type = "viridis")

## get variable importance
imp <- lgb.importance(gbm_lau)
lgb.plot.importance(imp, measure = "Cover")

## predict invertebrate effect on probs of presence of starfish
explainer <- explain(
  model = gbm_lau,
  data = data.matrix(st_lau[,covars]),
  y = st_lau$starfish,
  label = "LightGBM"
)

pdp_temp <- model_profile(explainer, variables = "invertebrate")

## plot this together with observed data
ggplot(st_lau) +
  geom_point(aes(x=invertebrate, y=starfish)) +
  geom_path(data=pdp_temp$agr_profiles, aes(x=`_x_`, y=`_yhat_`),
            color = "red", linewidth = 1.25, linetype = "dashed")

### do the same steps with the variable urchin as response

## build model object
dat_urchtr <- lgb.Dataset(data = data.matrix(st_lau[train_id, covars]),
                         label = st_lau$starfish[train_id])
dat_urchte <- lgb.Dataset(data = data.matrix(st_lau[-train_id, covars]),
                        label = st_lau$starfish[-train_id])

## set params 
params <- list(
  objective = "binary",
  metric = "binary_error",
  learning_rate = 0.1,
  num_leaves = 75,
  feature_fraction = 0.75,
  bagging_fraction = 0.75
)

## fit model
gbm_urch <- lgb.train(
  params = params,
  data = dat_urchtr,
  valids = list(test = dat_urchte),
  nrounds = 1000,
  early_stopping_rounds = 50
)

## get model performance
pred <- predict(gbm_urch, newdata = data.matrix(st_lau[-train_id, covars]),
                type = "class")
caret::confusionMatrix(factor(pred), factor(st_lau$urchin[-train_id]))

