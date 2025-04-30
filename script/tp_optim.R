
### Hyperparameters optimization

# 1. RF hyperparameters optimization example  

library(MASS)
library(randomForest)
library(GA)
library(caret)

# Data (we use criminology classical dataset from MASS)
data(Boston)
str(Boston)
set.seed(123)
train_index <- createDataPartition(Boston$medv, p = 0.8, list = FALSE)
train_data <- Boston[train_index, ]
test_data <- Boston[-train_index, ]

# Fonction objectif pour GA : retourne -RMSE (car GA maximise)
fitness_rf <- function(x) {
    mtry <- floor(x[1])
    ntree <- floor(x[2])
    nodesize <- floor(x[3])
    model <- randomForest(
        medv ~ ., data = train_data,
        mtry = mtry, ntree = ntree, nodesize = nodesize
    )
    preds <- predict(model, newdata = test_data)
    rmse <- sqrt(mean((preds - test_data$medv)^2))
    return(-rmse)  # on maximise, donc on retourne -RMSE
}

# Lancer l'algorithme génétique
ga_rf <- ga(
    type = "real-valued",
    fitness = fitness_rf,
    lower = c(1, 100, 1),         # bornes pour mtry, ntree, nodesize
    upper = c(13, 500, 10),
    popSize = 20, maxiter = 30, run = 10,
    seed = 42
)

# Résultats
summary(ga_rf)
best_params <- floor(ga_rf@solution)
names(best_params) <- c("mtry", "ntree", "nodesize")
print(best_params)



# 2. Gradient Boosting hyperparameters optimization example

# Fonction objectif : retourne -RMSE
fitness_gbm <- function(x) {
    n.trees <- floor(x[1])
    interaction.depth <- floor(x[2])
    shrinkage <- x[3]
    n.minobsinnode <- floor(x[4])
    
    model <- gbm(
        formula = medv ~ ., data = train_data,
        distribution = "gaussian",
        n.trees = n.trees,
        interaction.depth = interaction.depth,
        shrinkage = shrinkage,
        n.minobsinnode = n.minobsinnode,
        verbose = FALSE
    )
    
    preds <- predict(model, newdata = test_data, n.trees = n.trees)
    rmse <- sqrt(mean((preds - test_data$medv)^2))
    
    return(-rmse)
}

# Appel de l'algorithme génétique
ga_gbm <- ga(
    type = "real-valued",
    fitness = fitness_gbm,
    lower = c(50, 1, 0.01, 1),       # n.trees, depth, shrinkage, minobs
    upper = c(1000, 10, 0.2, 30),
    popSize = 20, maxiter = 30, run = 10,
    seed = 42
)

# Résumé des résultats
summary(ga_gbm)
best_params <- ga_gbm@solution
names(best_params) <- c("n.trees", "interaction.depth", "shrinkage", "n.minobsinnode")
print(round(best_params, 3))
