# Med-Mod Playground June 25, 2020 ----------------------------------------

#' --------------------------------------------------
#' Title: Mediation Moderation Playgorund - Workshop 2020
#' Date: June 25, 2020
#' Authors: Mohamadreza Hoseinpour
#'          Erasmus Universiteit Rotterdam
#' --------------------------------------------------

# Step 1: Load Libraries ----------------------------------------------------------
library(tidyverse)
library(mediation)
library(car)
library(here)
library(interactions)

# Step 2: Read Data ---------------------------------------------------------------
dataset1 <- read_csv(here("Mediation Moderation", "Dataset1.csv"))
dataset2 <- read_csv(here("Mediation Moderation", "Dataset2.csv"))
dataset4 <- read_csv(here("Mediation Moderation", "Dataset4.csv"))

# Step 3: Assess data -----------------------------------------------------
scatter.smooth(x = dataset1$TimeC,
               y = dataset1$Procrast,
               main = "Time Constraint ~ Procrastination")

plot(density(dataset1$TimeC), 
     main = "Density Plot: Time Constraint",
     ylab = "Frequency",
     sub = paste("Skewness:"))

cor(dataset1$TimeC, dataset1$Procrast)

model <- lm(Procrast ~ TimeC, data = dataset2)
summary(model)


# Simple moderation: Dataset 4, Purchase Intention ------------------------
purchase_intention <- lm(Purchase ~ BrandAtt + Price, dataset4)
summary(purchase_intention)
plot(purchase_intention)

purchase_intention_mod <- lm(Purchase ~ 
                               BrandAtt + Price + BrandAtt * Price,
                             dataset4)
summary(purchase_intention_mod)

# Probe Interaction:
sim_slopes(model = purchase_intention_mod,
           pred = BrandAtt,
           modx = Price,
           modx.values = NULL)

probe_interaction(model = purchase_intention_mod,
                  pred = BrandAtt,
                  modx = Price)
#' We have to be careful with this one. For min and max, if it is below or above
#'  the min or max of observed data, the min or max of W should be used for
#'  conditioning instead of W-1SD & W+1SD.

# Johnson-Neyman Interaction:
johnson_neyman(model = purchase_intention_mod,
               pred = BrandAtt,
               modx = Price)

