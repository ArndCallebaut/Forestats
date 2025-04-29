## Forestinecology - TP3
### Distributional Random Forest

### load libraries
library(tidyverse)
library(drf)
library(DiagrammeR)
library(DALEX)
library(pROC)

### 1. Airquality data

## load data
data("airquality")
## replace NA by median
airquality$Ozone[is.na(airquality$Ozone)] <- median(airquality$Ozone, na.rm = TRUE)
airquality$Solar.R[is.na(airquality$Solar.R)] <- median(airquality$Solar.R, na.rm = TRUE)
airquality$date <- paste(airquality$Month, airquality$Day, sep="_")

## prepare the data
X_all <- airquality[,c("Solar.R", "Wind", "Temp")]
y_all <- airquality[,"Ozone"]

## fit a basic drf
drf_basic <- drf(X_all, y_all, honesty = FALSE)

## plot some trees
tree1 <- get_tree(drf_basic,1)
class(tree1) <- "drf_tree"
plot(tree1)

## can you explain the total of obs in the terminal leaves?

## plot the 105th tree
## fill in the dots
tree105 <- get_tree(drf_basic,...)
class(tree105) <- "drf_tree"
plot(tree105)

## extract weights
w <- predict(drf_basic, newdata = X_all)$weights

## look at weights with obs from 01/05
plot(1:153, w[1,], type = "b")

## create density distribution from weights for this obs
### using MC estimation
dr_obs1 <- y_all[sample(1:length(y_all), size = length(y_all),
                      prob = w[1,], replace = TRUE)]
dens_obs1 <- density(dr_obs1)
plot(dens_obs1)

## estimate the mean and 95% CI via predict
m_obs1 <- predict(drf_basic, functional = "mean")$mean[1]
m_ci1 <- predict(drf_basic, functional = "quantile",
                 quantiles = c(0.025, 0.975))$quantile[1,,]
abline(v=m_obs1, col = "red")
abline(v=m_ci1, col="red", lty = 2)
abline(v=y_all[1], col="blue")

## get proba that the first observation has a value larger than the alert level of 120
sum(w[1,] * (y_all >= 120))

## repeat these steps (density distribution and p(Y) >= 120) 
## for the 62nd observation
dr_obs62 <- y_all[sample(1:length(y_all), size = length(y_all),
                        prob = w[...,], replace = TRUE)]

## get model performance
pred_ozone <- predict(drf_basic, functional = "mean")$mean[,1]
nrmse <- caret::RMSE(pred_ozone, y_test) / mean(y_all)
r2 <- caret::R2(pred_ozone, y_test)

## plot response curves to temperature
explainer <- explain(
  model = drf_basic,
  data = X_all,
  y = airquality$Ozone,
  predict_function = function(model, newdata) predict(model, newdata, functional="mean")$mean,
  label = "DRF"
)

pdp_temp <- model_profile(explainer, variables = "Temp")
## add CIs - a bit clunky ... 
pdp_temp$cp_profiles$lci <- predict(drf_basic,
                                    newdata = pdp_temp$cp_profiles[,c("Solar.R", "Wind", "Temp")],
                                    functional = "quantile", quantiles = 0.025)$quantile
pdp_temp$cp_profiles$uci <- predict(drf_basic,
                                    newdata = pdp_temp$cp_profiles[,c("Solar.R", "Wind", "Temp")],
                                    functional = "quantile", quantiles = 0.975)$quantile
pdp_temp$agr_profiles$lci <- aggregate(lci ~ Temp, data = pdp_temp$cp_profiles, FUN = mean)$lci
pdp_temp$agr_profiles$uci <- aggregate(uci ~ Temp, data = pdp_temp$cp_profiles, FUN = mean)$uci



## plot this together with observed data
ggplot(airquality) +
  geom_point(aes(x=Temp, y=Ozone)) +
  geom_path(data=pdp_temp$agr_profiles, aes(x=`_x_`, y=`_yhat_`),
            color = "red", linewidth = 1.75) +
  geom_path(data=pdp_temp$agr_profiles, aes(x=`_x_`, y=lci),
            color = "red", linewidth = 1.25, linetype = "dashed") +
  geom_path(data=pdp_temp$agr_profiles, aes(x=`_x_`, y=uci),
            color = "red", linewidth = 1.25, linetype = "dashed")
  

## tweak model fitting
## fill in the dots
drf_adv <- drf(X_train, y_train, num.trees = ...,num.features = ...,
               sample.fraction = ..., mtry = ...,
               honesty = ...,compute.variable.importance = ...)

## compare to the basic model

### part 2 - St Laurent data

## load data
st_lau <- read.csv2("LIF/Presentation/ecostat_2025/data/StLaurent.csv")
st_lau$temperature[is.na(st_lau$temperature)] <- median(st_lau$temperature,
                                                        na.rm = TRUE)

## create the data for drf
target_vars <- c("starfish", "urchin")
X <- st_lau[, !names(st_lau) %in% target_vars]
Y <- st_lau[,target_vars]

## fit a drf model
m_lau <- drf(X, Y, num.trees = 1000, mtry = 5, num.features = 50,
             honesty = FALSE, sample.fraction = 0.5,
             compute.variable.importance = TRUE)

## inner working for discrete predictions
w <- predict(m_lau)$weights
sum(w[1,] * Y[,1])


## get the predictions
pred_lau <- predict(m_lau, functional = "mean")$mean

## discretize the probs using the ROC curve
roc_star <- roc(Y[,"starfish"], pred_lau[,1])
thresh_star <- coords(roc_star, "best", ret = "threshold")
caret::confusionMatrix(factor(as.numeric(pred_lau[,1] >= thresh_star[1,1])),
                       factor(st_lau$starfish))

## do the same for the urchin

## plot variable importance
imp <- data.frame(name = m_lau$mat.col.names,
                  imp = m_lau$variable.importance)
ggplot(imp, aes(x=fct_reorder(name, imp), y=imp)) +
  geom_point() +
  coord_flip()

## explore predicted probability of presence across space conditional
## on the other variables and with CIs
pred_ci <- predict(m_lau, functional = "quantile",
                   quantiles = c(0.025, 0.975))$quantile

## put in one df
df_lau <- cbind(st_lau[,c("longitude", "latitude")],
                pred_lau,
                pred_ci[,,1],
                pred_ci[,,2])
names(df_lau)[3:8] <- c("star", "urch", "star_lci", "urch_lci",
                        "star_uci", "urch_uci")
## plot
df_lau %>%
  pivot_longer(3:8) %>%
  ggplot(aes(x=longitude,
             y=latitude,
             colour = value)) +
  geom_point() +
  facet_wrap(vars(name)) +
  scale_color_continuous(type="viridis")

## correlation between starfish and urchin probability of presence
cor_lau <- predict(m_lau, functional = "cor")$cor
## plot this across space
df_lau$cor <- cor_lau[,2,1]

ggplot(df_lau, aes(x=longitude,
                   y=latitude,
                   color=cor)) +
  geom_point() +
  scale_color_gradient2() +
  theme_void()

## plot the response curves of starfish and urchin probability of
## presence to temperature
## fill in the dots
explainer <- explain(
  model = ...,
  data = ...,
  y = ...,
  predict_function = ...,
  label = ...
)

pdp_temp <- model_profile(explainer, variables = ...)

ggplot(...)