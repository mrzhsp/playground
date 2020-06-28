#' --------------------------------------------------
#' Title: Data Analysis Script
#' Subtitle: Quantitative Analysis Workshop 2020
#' Authors: Timo van Balen and Mohamadreza Hoseinpour
#'          Erasmus Universiteit Rotterdam
#' --------------------------------------------------

# Step 1: Load Libraries -------------------------------------------------------
install.packages('tidyverse')
install.packages('mediation')
install.packages('car')
install.packages('readr')
library(tidyverse)
library(mediation)
library(car)
library(readr)

# Step 2: Read Data ---------------------------------------------------------------
testdata <- read.csv2("team statistics file.csv")
View(testdata)
#' If you encounter problems with reading the file, you should make sure that 
#'  this file exist in the working directory assigned for the R Project, or the
#'  R script that you are using. 

attach(testdata)
#' This function makes sure that you do not have to type the data object first. 
#'  For example, identification.mean instead of testdata$identification.mean.


# Step 3: Assess Data -------------------------------------------------------------
#' Whenever you wanted to know more about the function, you can just type a
#'  question mark before it and type it in the console. For example, if you want
#'  to know how the function "hist" works and what it is, just type "?hist" in 
#'  concole and press enter. The help file will be shown. 
summary(identification.mean)
hist(identification.mean)
?hist
summary(commitment.mean)
hist(commitment.mean)

plot(commitment.mean~identification.mean)

summary(team.performance.t1)
hist(team.performance.t1)

plot(team.performance.t1~identification.mean)
plot(team.performance.t1~commitment.mean)


# Step 4: Data Analysis - Model 1 -------------------------------------------------
model.med <- lm(commitment.mean ~ 
                  identification.mean * cogndivers.mean, 
                testdata)
#' You may add anything with a +. For example +TMScoord.mean+power.disparity

summary(model.med)
plot(model.med)
vif(model.med)

model.outc <- lm(team.performance.t1 ~ 
                   commitment.mean + identification.mean * cogndivers.mean, 
                 testdata)
summary(model.outc)
plot(model.outc)
vif(model.outc)

med.mod <- mediate(model.med, 
                   model.outc, 
                   treat = "identification.mean", 
                   mediator = "commitment.mean", 
                   boot = T, 
                   boot.ci.type = "bca")
summary(med.mod)

med.modmin1 <- mediate(model.med, 
                       model.outc, 
                       treat = "identification.mean", 
                       mediator = "commitment.mean", 
                       boot = T, 
                       boot.ci.type = "bca", 
                       covariates = list(cogndivers.mean = 
                                           mean(cogndivers.mean) - 
                                           sd(cogndivers.mean)))
summary(med.modmin1)

med.modplus1 <- mediate(model.med, 
                        model.outc, 
                        treat = "identification.mean", 
                        mediator = "commitment.mean", 
                        boot = T, 
                        boot.ci.type = "bca", 
                        covariates = list(cogndivers.mean = 
                                            mean(cogndivers.mean) + 
                                            sd(cogndivers.mean)))
summary(med.modplus1)