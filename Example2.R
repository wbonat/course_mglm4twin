## Course · Analysis of Twin Data in Health Science · Session IV ---------------
## Example 2 · Anthropometric measures: Bivariate continuous data --------------
## Prof. Wagner Hugo Bonat · Ômega online School of Data Science ---------------
## Date: 05/05/2022 ------------------------------------------------------------

## Loading extra packages
library(Matrix) # Linear algebra used by the mglm4twin package
library(dplyr) # Data manipulation
library(tidyr) # data manipulation
library(ggplot2) # Data visualization

## Loading package
library(mglm4twin)
packageVersion("mglm4twin")

## Loading data set
data(anthro) ## The data set is already organized. Check Example 1 for details.

## Exploratory analysis --------------------------------------------------------

## Correlation between MZ and DZ twin

# Wide format
data_graph <- anthro %>% select(weight, height, Twin, Twin_pair, Group) %>%
  pivot_wider(names_from = 'Twin_pair',
              values_from = c('height', 'weight')) %>%
  pivot_longer(names_to = 'Trait',
               values_to = 'resposta',
               cols = -c(Group, Twin)) %>%
  separate(col = Trait, into = c("Trait", "Twin_pair"), sep = "_") %>%
  pivot_wider(names_from = 'Twin_pair',
              values_from = c('resposta'))

names(data_graph)[4:5] <- c("Twin_1", "Twin_2")
data_graph$Trait <- as.factor(data_graph$Trait)
levels(data_graph$Trait) <- c("Height (meters)", "Weight (kg)")

ggplot(data_graph) +
  geom_point(aes(y = Twin_1, x = Twin_2)) +
  geom_smooth(aes(y = Twin_1, x = Twin_2), method = 'lm') +
  facet_wrap(~ Trait + Group, scales = "free")


## Correlation between responses

data_graph2 <- anthro %>% select(weight, height, Twin, Twin_pair, Group) %>%
  pivot_wider(names_from = 'Twin_pair',
              values_from = c('height', 'weight'))

ggplot(data_graph2) +
  geom_point(aes(y = height_1, x = weight_1)) +
  geom_smooth(aes(y = height_1, x = weight_1), method = 'lm') +
  facet_wrap(~ Group)

ggplot(data_graph2) +
  geom_point(aes(y = height_2, x = weight_2)) +
  geom_smooth(aes(y = height_2, x = weight_2), method = 'lm') +
  facet_wrap(~ Group)


## Fitting univariate models ---------------------------------------------------

## Linear predictor
form_Wt <- weight ~ age + Group*Twin_pair
form_Ht <- height ~ age + Group*Twin_pair

## Matrix linear predictor
Z_E <- mt_twin(N_DZ = 327, N_MZ = 534, n_resp = 1, model = "E")
Z_AE <- mt_twin(N_DZ = 327, N_MZ = 534, n_resp = 1, model = "AE")
Z_CE <- mt_twin(N_DZ = 327, N_MZ = 534, n_resp = 1, model = "CE")
Z_ACE <- mt_twin(N_DZ = 327, N_MZ = 534, n_resp = 1, model = "ACE")

## Fitting · weight
Wt_E <- mglm4twin(linear_pred = c(form_Wt), 
                  matrix_pred = Z_E,
                  data = anthro)

Wt_AE <- mglm4twin(linear_pred = c(form_Wt), 
                  matrix_pred = Z_AE,
                  data = anthro)

Wt_CE <- mglm4twin(linear_pred = c(form_Wt), 
                   matrix_pred = Z_CE,
                   data = anthro)

Wt_ACE <- mglm4twin(linear_pred = c(form_Wt), 
                   matrix_pred = Z_ACE,
                   data = anthro)

## Fitting · height
Ht_E <- mglm4twin(linear_pred = c(form_Ht), 
                  matrix_pred = Z_E,
                  data = anthro)

Ht_AE <- mglm4twin(linear_pred = c(form_Ht), 
                   matrix_pred = Z_AE,
                   data = anthro)

Ht_CE <- mglm4twin(linear_pred = c(form_Ht), 
                   matrix_pred = Z_CE,
                   data = anthro)

Ht_ACE <- mglm4twin(linear_pred = c(form_Ht), 
                    matrix_pred = Z_ACE,
                    data = anthro)

## Fitting bivariate models ----------------------------------------------------

## Matrix linear predictor
Z_E <- mt_twin(N_DZ = 327, N_MZ = 534, n_resp = 2, model = "E")
Z_AE <- mt_twin(N_DZ = 327, N_MZ = 534, n_resp = 2, model = "AE")
Z_CE <- mt_twin(N_DZ = 327, N_MZ = 534, n_resp = 2, model = "CE")
Z_ACE <- mt_twin(N_DZ = 327, N_MZ = 534, n_resp = 2, model = "ACE")

biv_E <- mglm4twin(linear_pred = c(form_Wt, form_Ht), 
                   matrix_pred = Z_E,
                   data = anthro)

biv_AE <- mglm4twin(linear_pred = c(form_Wt, form_Ht), 
                    matrix_pred = Z_AE,
                    data = anthro)

biv_CE <- mglm4twin(linear_pred = c(form_Wt, form_Ht), 
                    matrix_pred = Z_CE,
                    data = anthro)

biv_ACE <- mglm4twin(linear_pred = c(form_Wt, form_Ht), 
                     matrix_pred = Z_ACE,
                     data = anthro)

## Regression models for the dispersion components -----------------------------


## Easy computation 
anthro$age <- (anthro$age - mean(anthro$age))/sd(anthro$age)
anthro$weight <- (anthro$weight - mean(anthro$weight))/sd(anthro$weight)
anthro$height <- (anthro$height - mean(anthro$height))/sd(anthro$height)

biv0 <- list("formE1" = ~ age, "formE2" = ~ age, "formE12" = ~ age,
             "formA1" = ~ age, "formA2" = ~ age, "formA12" = ~ age,
             "formC1" = ~ age, "formC2" = ~ age, "formC12" = ~ age)

Z_biv0 <- mt_twin(N_DZ = 327, N_MZ = 534, n_resp = 2, model = "ACE",
                  formula = biv0, data = anthro)

## Special case of ACE model
biv4 <- list("formE1" = ~ age, "formE2" = ~ age, "formE12" = ~ 1,
             "formA1" = ~ age, "formA2" = ~ 1, "formA12" = ~ 1)

Z_biv4 <- mt_twin(N_DZ = 327, N_MZ = 534, n_resp = 2, model = "AE",
                  formula = biv4, data = anthro)


## Initial values
control_initial <- list()
control_initial$regression <- list("R1" = c(0.13, 0.10, -0.20, -0.02, 0.037),
                                   "R2" = c(0.23, 0.01, -0.27, -0.11, 0.11))
control_initial$power <- list(c(0), c(0))
control_initial$tau <- c(0.15, 0, 0.12, rep(0,15))


# Model fitting
fit_0 <- mglm4twin(linear_pred = c(form_Wt, form_Ht), matrix_pred = Z_biv0,
                   control_initial = control_initial,
                   control_algorithm = list(tuning = 0.5),
                   power_fixed = c(TRUE, TRUE),
                   data = anthro)

control_initial$tau <- c(0.15, 0, 0.12, rep(0,6))
fit_4 <- mglm4twin(linear_pred = c(form_Wt, form_Ht), matrix_pred = Z_biv4,
                   control_initial = control_initial,
                   control_algorithm = list(tuning = 0.5),
                   power_fixed = c(TRUE, TRUE),
                   data = anthro)


## Computing measures of interest
Age = seq(-1.90, 1.70, l = 100)
point <- fit_4$Covariance

# Heritability
h_wt <- (point[6] + point[7]*Age)/(point[1] + point[2]*Age + point[6] + point[7]*Age)
h_ht <- (point[8])/(point[3] + point[4]*Age + point[8])
h_ht_wt <- point[9]/(point[5] + point[9])

# Genetic and phenotipic correlation
r_G <- point[9]/(sqrt(point[6] + point[7]*Age)*sqrt(point[8]))
std_1 <- sqrt(point[1] + point[2]*Age + point[6] + point[7]*Age)
std_2 <- sqrt(point[3] + point[4]*Age + point[8])
r_P <- (point[9] + point[5])/(std_1*std_2)


require(mvtnorm)
#> Loading required package: mvtnorm
Age = seq(-1.90, 1.70, l = 50)

# Point estimates
point1 <- fit_4$Covariance

# Covariance matrix (otimist - Gaussian case -> maximum likelihood estimator)
COV <- -2*fit_4$joint_inv_sensitivity[11:19, 11:19]

## Simulating from the assymptotic ditribution
point <- rmvnorm(n = 10000, mean = point1, sigma = as.matrix(COV))
res_h_wt <- matrix(NA, nrow = 50, ncol = 10000)
res_h_ht <- matrix(NA, nrow = 50, ncol = 10000)
res_ht_wt <- matrix(NA, nrow = 50, ncol = 10000)
res_r_G <- matrix(NA, nrow = 50, ncol = 10000)
res_r_P <- matrix(NA, nrow = 50, ncol = 10000)
res_r_E <- matrix(NA, nrow = 50, ncol = 10000)

for(i in 1:10000) {
  h_wt <- (point[i,6] + point[i,7]*Age)/(point[i,1] + point[i,2]*Age + point[i,6] + point[i,7]*Age)
  res_h_wt[,i] <- h_wt
  h_ht <- (point[i,8])/(point[i,3] + point[i,4]*Age + point[i,8])
  res_h_ht[,i] <- h_wt
  h_ht_wt <- point[i,9]/(point[i,5] + point[i,9])
  res_ht_wt[,i] <- h_ht_wt
  r_G <- point[i,9]/(sqrt(point[i,6] + point[i,7]*Age)*sqrt(point[i,8]))
  res_r_G[,i] <- r_G
  std_1 <- sqrt(point[i,1] + point[i,2]*Age + point[i,6] + point[i,7]*Age)
  std_2 <- sqrt(point[i,3] + point[i,4]*Age + point[i,8])
  r_P <- (point[9] + point[5])/(std_1*std_2)
  res_r_P[,i] <- r_P
  r_E <- point[i,5]/(sqrt(point[i,1] + point[i,2]*Age)*sqrt(point[i,3] + point[i,4]*Age))
  res_r_E[,i] <- r_E
}

Estimates <- c(rowMeans(res_h_wt), rowMeans(res_h_ht),
               rowMeans(res_ht_wt),
               rowMeans(res_r_G), rowMeans(res_r_E),
               rowMeans(res_r_P))
Ic_Min <- c(apply(res_h_wt, 1, quantile, 0.025),
            apply(res_h_ht, 1, quantile, 0.025),
            apply(res_ht_wt, 1, quantile, 0.025),
            apply(res_r_G, 1, quantile, 0.025),
            apply(res_r_E, 1, quantile, 0.025),
            apply(res_r_P, 1, quantile, 0.025))
Ic_Max <- c(apply(res_h_wt, 1, quantile, 0.975),
            apply(res_h_ht, 1, quantile, 0.975),
            apply(res_ht_wt, 1, quantile, 0.975),
            apply(res_r_G, 1, quantile, 0.975),
            apply(res_r_E, 1, quantile, 0.975),
            apply(res_r_P, 1, quantile, 0.975))
data_graph <- data.frame("Estimates" = Estimates, "Ic_Min" = Ic_Min,
                         "Ic_Max" = Ic_Max, "Age" = rep(Age, 6),
                         "Parameter" = rep(c("h1","h2","h12","G","E","P"), each = 50))

ggplot(data_graph) +
  geom_line(aes(x = Age, y = Estimates)) +
  geom_line(aes(x = Age, y = Ic_Min), col = "red") +
  geom_line(aes(x = Age, y = Ic_Max), col = "red") +
  facet_wrap(~Parameter)

