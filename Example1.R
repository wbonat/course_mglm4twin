## Course · Analysis of Twin Data in Health Science · Session IV ---------------
## Example 1 · BPD and RDS on preterm infants: Bivariate dichotomous data ------
## Prof. Wagner Hugo Bonat · Ômega online School of Data Science ---------------
## Date: 05/05/2022 ------------------------------------------------------------


## Loading extra packages
library(Matrix) # Linear algebra used by the mglm4twin package
library(dplyr) # Data manipulation
library(tidyr) # data manipulation
library(ggplot2) # Data visualization

## Multivariate generalized linear models for twin and family data -------------

# Download the .tar.gz (for linux) or .zip (for Windows)
fn <- "www.leg.ufpr.br/~wagner/mglm4twin/source/mglm4twin_0.2.0.tar.gz"
download.file(fn, destfile = "mglm4twin_0.2.0.tar.gz ")

# Installing (be sure the check the file address)
install.packages("/home/wagner/Downloads/mglm4twin_0.2.0.tar.gz", repos = NULL,
                 lib.loc = "/path/to/your/R/library",
                 dependencies = TRUE)

## Loading package
library(mglm4twin)
packageVersion("mglm4twin") # Be sure to use the version 0.2.0

## Loading data set
data(bpdrds)
head(bpdrds)

## Be sure to organize the data set correctly
# 1. Use only complete data (both twin should have data)
# 2. DZ twin should appear first
# 3. Twin pair should appear one after the other


# Checking number of twin pairs and ordering the data set
cod = table(bpdrds$Twin) == 2 
cod = rownames(cod[cod == TRUE])
data = bpdrds[which(bpdrds$Twin %in% cod),]
data = bpdrds[order(data$Twin),]
data = data[order(data$Group),]
dim(data)
head(data)

## Steps for model fitting

# 1. Specifying the linear predictor (mean model).
# 2. Specifying the matrix linear predictor (twin dependence structure ACDE).
# 3. Specifying the link and variance function according to the response variable type (popular choices)
## 3.1 Binary and binomial data (link = "logit" and variance = "binomialP")
## 3.2 Bounded data and continuous proportions (link = "logit" and variance = "binomialP")
## 3.3 Under-, equi- and over-dispersed count data (link = "log" and variance = "poisson_tweedie")
## 3.4 Semi-continuous data (continuous $+$ mass at zero) (link = "log" and variance = "tweedie")
## 3.5 Symmetric continuous data (link = "identity" and variance = "constant") 
## 3.6 Assymetric continuous data (link = "log" and variance = "tweedie")



## Linear predictor
form_BPD <- BPD ~ BW + GA + gender + Group*Twin_pair
form_RDS <- RDS ~ BW + GA + gender + Group*Twin_pair

## Matrix linear predictor
N_DZ <- dim(data[data$Group == "DZ",])[1]/2 # number of DZ twin pairs
N_MZ <- dim(data[data$Group == "MZ",])[1]/2 # number of MZ twin pairs
biv_ACE <- mt_twin(N_DZ = N_DZ, N_MZ = N_MZ, n_resp = 2, model = "ACE")
length(biv_ACE) # Number of dispersion parameters

## Important to check the size of the matrices
lapply(biv_ACE, dim) ## OK

## Model fitting
fit_ACE <- mglm4twin(linear_pred = c(form_BPD, form_RDS), 
                     matrix_pred = biv_ACE,
                    link = c("logit", "logit"),
                    variance = c("binomialP", "binomialP"),
                     data = data)

## Model's output

# Standard print method not so good :(
fit_ACE

# Summary
summary(fit_ACE, model = "ACE")

# Special summary
summary(fit_ACE, model = "ACE", biometric = TRUE)

################################################################################

## Model comparison

biv_E <- mt_twin(N_DZ = N_DZ, N_MZ = N_MZ, n_resp = 2, model = "E")
biv_AE <- mt_twin(N_DZ = N_DZ, N_MZ = N_MZ, n_resp = 2, model = "AE")
biv_CE <- mt_twin(N_DZ = N_DZ, N_MZ = N_MZ, n_resp = 2, model = "CE")
biv_ACE <- mt_twin(N_DZ = N_DZ, N_MZ = N_MZ, n_resp = 2, model = "ACE")

## Fitting

# E model
fit_E <- mglm4twin(linear_pred = c(form_BPD, form_RDS), 
                   matrix_pred = biv_E,
                   link = c("logit", "logit"),
                   variance = c("binomialP", "binomialP"),
                   data = data)

# AE model
fit_AE <- mglm4twin(linear_pred = c(form_BPD, form_RDS), 
                    matrix_pred = biv_AE,
                    link = c("logit", "logit"),
                    variance = c("binomialP", "binomialP"),
                    data = data)

# CE model
fit_CE <- mglm4twin(linear_pred = c(form_BPD, form_RDS), 
                    matrix_pred = biv_CE,
                    link = c("logit", "logit"),
                    variance = c("binomialP", "binomialP"),
                    data = data)

# ACE model
fit_ACE <- mglm4twin(linear_pred = c(form_BPD, form_RDS), 
                     matrix_pred = biv_ACE,
                     link = c("logit", "logit"),
                     variance = c("binomialP", "binomialP"),
                     data = data)

## Comparing
rbind(gof(fit_E), gof(fit_AE), gof(fit_CE), gof(fit_ACE))

## AE models provides the best balance between gof and complexity
summary(fit_AE, model = "AE", biometric = TRUE)


## Is the bivariate model better than fit two separate models?
AE <- mt_twin(N_DZ = N_DZ, N_MZ = N_MZ, n_resp = 1, model = "AE")


## Fitting univariate models
BPD_AE <- mglm4twin(linear_pred = c(form_BPD), 
                    matrix_pred = AE,
                    link = c("logit"),
                    variance = c("binomialP"),
                    data = data)
summary(BPD_AE, model = "AE", biometric = TRUE)

RDS_AE <- mglm4twin(linear_pred = c(form_RDS), 
                    matrix_pred = AE,
                    link = c("logit"),
                    variance = c("binomialP"),
                    data = data)
summary(RDS_AE, model = "AE", biometric = TRUE)

## How to compare univariate and bivariate models? 
# univariate are special cases of the bivariate
rbind(gof(list(BPD_AE, RDS_AE)), gof(fit_AE))


## Other features

## Change the link function
probit_AE <- mglm4twin(linear_pred = c(form_BPD, form_RDS), 
                       matrix_pred = biv_AE,
                       link = c("probit", "probit"),
                       variance = c("binomialP", "binomialP"),
                       data = data)
summary(probit_AE, model = "AE", biometric = TRUE)

## Mixing link function
clog_cauchit_AE <- mglm4twin(linear_pred = c(form_BPD, form_RDS), 
                             matrix_pred = biv_AE,
                             link = c("cloglog", "cauchit"),
                             variance = c("binomialP", "binomialP"),
                             data = data)
summary(clog_cauchit_AE, model = "AE", biometric = TRUE)

## Comparing
rbind(gof(fit_AE), gof(probit_AE), gof(clog_cauchit_AE))

## END -------------------------------------------------------------------------