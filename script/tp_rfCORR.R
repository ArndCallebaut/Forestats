## Forestinecology - TP 2
### Random Forests

## load libraries
library(caret)
library(randomForest)
library(lightgbm)

# 1. Random Forest - Classification - iris data

## load data
data("iris")
head(iris)
summary(iris)
str(iris)
set.seed(12) 

## construct a random forest (without train/test)
rf_model <- randomForest(Species ~ ., data = iris, ntree = 1000, mtry = 2, importance = TRUE)
print(rf_model)

# OOB error for the differents categories (and the sum, in black)
plot(rf_model)

# Mean Decrease in Gini → based on increased error after variable shuffle
importance(rf_model, type = 1) #type1 = Mean Decrease in accuracy
# Mean Decrease in Gini → based on sum of impurity reduction
importance(rf_model, type = 2) #type2 = Mean Decrease in Gini index
# And show it on a plot ..!
varImpPlot(rf_model)



# 2. Random Forest - Regression - Ozone

# 2.1 Building the model

## load data
data("airquality")
str(airquality)
## replace NA by median
airquality$Ozone[is.na(airquality$Ozone)] <- median(airquality$Ozone, na.rm = TRUE)
airquality$Solar.R[is.na(airquality$Solar.R)] <- median(airquality$Solar.R, na.rm = TRUE)

# Make the random forest
rf_air <- randomForest(Ozone ~ ., data = airquality, importance = TRUE, ntree = 500)
print(rf_air)

#Variable importance
varImpPlot(rf_air)

#OOB error with the number of trees
plot(rf_air)

# Prediction OOB VS real values
predicted <- rf_air$predicted
ggplot(airquality, aes(x = Ozone, y = predicted)) +
    geom_point(color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Prédictions RF vs Ozone réel",
         x = "Ozone réel", y = "Ozone prédit") +
    theme_minimal()

# 2.2 Comparaison with bootstrap & boosting trees
library(caret)
library(randomForest)
library(gbm)
library(dplyr)
library(ggplot2)

# Grille de nombres d'arbres
ntree_values <- c(10, 25, 50, 75, 100, 200, 300, 400, 500,750, 1000)
cv_control <- trainControl(method = "cv", number = 5)
results <- data.frame()

# we use the caret package to do a 5-fold cross validation, for different models.
# concept : 5 times, we use 4/5 of the dataset as train and then use 1/5 as test.
# we could use the OOB, but it's not exactly comparable with the error of GBM.

# BAGGING
for (nt in ntree_values) {
    set.seed(123)
    model <- train(
        Ozone ~ ., data = airquality,
        method = "rf",
        trControl = cv_control,
        tuneGrid = data.frame(mtry = ncol(airq_clean) - 1),  # mtry = nb variables -> bagging
        ntree = nt,
        metric = "RMSE"
    )
    results <- rbind(results, data.frame(Model = "Bagging", Trees = nt, RMSE = model$results$RMSE))
}

# RANDOM FOREST
for (nt in ntree_values) {
    set.seed(123)
    model <- train(
        Ozone ~ ., data = airquality,
        method = "rf",
        trControl = cv_control,
        tuneGrid = data.frame(mtry = 2),  
        ntree = nt,
        metric = "RMSE"
    )
    results <- rbind(results, data.frame(Model = "RandomForest", Trees = nt, RMSE = model$results$RMSE))
}

# BOOSTING (GBM)
for (nt in ntree_values) {
    set.seed(123)
    model <- train(
        Ozone ~ ., data = airquality,
        method = "gbm",
        trControl = cv_control,
        tuneGrid = data.frame(interaction.depth = 3,
                              n.trees = nt,
                              shrinkage = 0.05,
                              n.minobsinnode = 10),
        verbose = FALSE,
        metric = "RMSE"
    )
    results <- rbind(results, data.frame(Model = "Boosting", Trees = nt, RMSE = model$results$RMSE))
}
ggplot(results, aes(x = Trees, y = RMSE, color = Model)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(title = "RMSE vs Nombre d'Arbres par Modèle",
         x = "Nombre d'Arbres",
         y = "RMSE (Validation croisée)") +
    theme_minimal()

# 2.3 Tuning the model 
# what is the "good number" of variables used for each trees ?
# we use tuneRF to find the "best" number of variables considered for each trees.
set.seed(123)
model_tuned <- tuneRF(
    x=airquality[,-1], #define predictor variables
    y=airquality$Ozone, #define response variable
    ntreeTry=500,
    mtryStart=4, 
    stepFactor=2,
    improve=0.01,
    trace=TRUE #suivre l'évolution des tests
)

# we can directly get the "optimal" model
model_tuned_RF <- tuneRF(
    x=airquality[,-1], #define predictor variables
    y=airquality$Ozone, #define response variable
    ntreeTry=500,
    mtryStart=4, 
    stepFactor=1.5,
    improve=0.01,
    trace=TRUE, #suivre l'évolution des tests.. oui
    doBest = T # renvoyer un modèle de RF optimal avec les bon coefficients... oui
)



###############################################################################
###############################################################################
##### YOUR TURN !              ################################################
###############################################################################
###############################################################################

## St Laurent data
# We want to predict the presence/absence of starfishs, then of urchins.
set.seed(123)

### load data
st_lau <- read.csv2("StLaurent.csv")
st_lau$temperature[is.na(st_lau$temperature)] <- median(st_lau$temperature,
                                                        na.rm = TRUE)
summary(st_lau)

st_lau$starfish = as.factor(st_lau$starfish)

# Visualize
ggplot(st_lau, aes(x=longitude, y=latitude, color=factor(starfish))) +
    geom_point()

# Build the RandomForest model
rf_model <- randomForest(starfish ~ ., data = st_lau [,4:11], ntree = 1000, mtry = 2, importance = TRUE)


# Analyse the error of classification
print(rf_model)
# the precision is not great, absences are often classified as Presence and the overall OOB error is 33%
# let's continue..!

# Analyse the importance of the different variables
importance(rf_model, type = 1)
importance(rf_model, type = 2)
varImpPlot(rf_model)
# The best metric being type2, the variables depth and invertabrate are the most important.
# Comparing the to graphs, we can see the values are different.
# It's show how the fact that a variable that is not used a lot in the trees can still be important for the RF.

# What is the best model of the three ?
ntree_values <- c(10, 25, 50, 75, 100)
cv_control <- trainControl(method = "cv", number = 4)
results <- data.frame()
# BAGGING
for (nt in ntree_values) {
    set.seed(123)
    model <- train(
        starfish ~ ., data = st_lau[,4:11],
        method = "rf",
        trControl = cv_control,
        tuneGrid = data.frame(mtry = ncol(st_lau[,4:11]) - 1),  # mtry = nb variables -> bagging
        ntree = nt,
        metric = "Accuracy"
    )
    results <- rbind(results, data.frame(Model = "Bagging", Trees = nt, Accuracy = model$results$Accuracy))
}

# RANDOM FOREST
for (nt in ntree_values) {
    set.seed(123)
    model <- train(
        starfish ~ ., data = st_lau[,4:11],
        method = "rf",
        trControl = cv_control,
        tuneGrid = data.frame(mtry = 3),  
        ntree = nt,
        metric = "Accuracy"
    )
    results <- rbind(results, data.frame(Model = "RandomForest", Trees = nt, Accuracy = model$results$Accuracy))
}

# BOOSTING (GBM)
for (nt in ntree_values) {
    set.seed(123)
    model <- train(
        starfish ~ ., data = st_lau[,4:11],
        method = "gbm",
        trControl = cv_control,
        tuneGrid = data.frame(interaction.depth = 3,
                              n.trees = nt,
                              shrinkage = 0.05,
                              n.minobsinnode = 10),
        verbose = FALSE,
        metric = "Accuracy"
    )
    results <- rbind(results, data.frame(Model = "Boosting", Trees = nt, Accuracy = model$results$"Accuracy"))
}
ggplot(results, aes(x = Trees, y = Accuracy, color = Model)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(title = "Précision vs Nombre d'Arbres par Modèle",
         x = "Nombre d'Arbres",
         y = "Accuracy (Validation croisée)") +
    theme_minimal()

# Ici aucun des modèle n'est vraiment probant, on a au mieux une précision de 0.68







# Do it again for urchins.
set.seed(123)

### load data
st_lau <- read.csv2("StLaurent.csv")
st_lau$temperature[is.na(st_lau$temperature)] <- median(st_lau$temperature,
                                                        na.rm = TRUE)
summary(st_lau)

st_lau$urchin = as.factor(st_lau$urchin)

# Visualize
ggplot(st_lau, aes(x=longitude, y=latitude, color=factor(urchin))) +
    geom_point()

# Build the RandomForest model
rf_model <- randomForest(urchin ~ ., data = st_lau[,c(4:10,12)], ntree = 1000, mtry = 2, importance = TRUE)


# Analyse the error of classification
print(rf_model)
# The OOB error is only 14%.

# Analyse the importance of the different variables
importance(rf_model, type = 1)
importance(rf_model, type = 2)
varImpPlot(rf_model)
# The best metric being type2, the variables depth and invertabrate are the most important.
# Comparing the to graphs, we can see the values are different.
# It's show how the fact that a variable that is not used a lot in the trees can still be important for the RF.

# What is the best model of the three ?
ntree_values <- c(10, 25, 50, 75, 100,500,1000)
cv_control <- trainControl(method = "cv", number = 4)
results <- data.frame()
# BAGGING
for (nt in ntree_values) {
    set.seed(123)
    model <- train(
        urchin ~ ., data = st_lau[,c(4:10,12)],
        method = "rf",
        trControl = cv_control,
        tuneGrid = data.frame(mtry = ncol(st_lau[,c(4:10,12)]) - 1),  # mtry = nb variables -> bagging
        ntree = nt,
        metric = "Accuracy"
    )
    results <- rbind(results, data.frame(Model = "Bagging", Trees = nt, Accuracy = model$results$Accuracy))
}

# RANDOM FOREST
for (nt in ntree_values) {
    set.seed(123)
    model <- train(
        urchin ~ ., data = st_lau[,c(4:10,12)],
        method = "rf",
        trControl = cv_control,
        tuneGrid = data.frame(mtry = 3),  
        ntree = nt,
        metric = "Accuracy"
    )
    results <- rbind(results, data.frame(Model = "RandomForest", Trees = nt, Accuracy = model$results$Accuracy))
}

# BOOSTING (GBM)
for (nt in ntree_values) {
    set.seed(123)
    model <- train(
        urchin ~ ., data = st_lau[,c(4:10,12)],
        method = "gbm",
        trControl = cv_control,
        tuneGrid = data.frame(interaction.depth = 3,
                              n.trees = nt,
                              shrinkage = 0.05,
                              n.minobsinnode = 10),
        verbose = FALSE,
        metric = "Accuracy"
    )
    results <- rbind(results, data.frame(Model = "Boosting", Trees = nt, Accuracy = model$results$"Accuracy"))
}
ggplot(results, aes(x = Trees, y = Accuracy, color = Model)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(title = "Précision vs Nombre d'Arbres par Modèle",
         x = "Nombre d'Arbres",
         y = "Accuracy (Validation croisée)") +
    theme_minimal()

# Here, RF and Boosting give accuracies that are really close.
# D'autre métriques auraient pu être utilisées ici: Kappa, ROC, ect.





