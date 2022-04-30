## Motivation examples: Analysis of Twin Data in Health Science ----------
## Author: Prof. Wagner Hugo Bonat · Ômega Data Science ------------------
## Date: 05/05/2022 ------------------------------------------------------

## Load extra package
library(devtools)
library(ggplot2)
library(dplyr)
library(tidyr)

## Install mglm4twin package from github
install_github("wbonat/mglm4twin")

## Load mglm4twin package
library(mglm4twin)

## Load data set anthro
data(anthro)

## Wide format
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


library(mglm4twin)
library(ggplot2)
data(anthro)
ggplot(anthro) +
  geom_histogram(aes(height))


require(mvtnorm)
COR <- matrix(c(1, 0.8, 0.8, 1), 2,2)
Z <- rmvnorm(n = 10, mean = c(0,0), sigma = COR)
CONTAGENS <- qpois(pnorm(Z), lambda = 5)
cor(CONTAGENS)
CONTAGENS

require(mcglm)
