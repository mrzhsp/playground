#' --------------------------------------------------
#' Title: Data Transformation Script
#' Subtitle: Quantitative Analysis Workshop 2020
#' Authors: Timo van Balen and Mohamadreza Hoseinpour
#'          Erasmus Universiteit Rotterdam
#' --------------------------------------------------

# Step 1: Load Libraries ----------------------------------------------------------
install.packages('tidyverse')
install.packages('readr')
install.packages('multilevel')
library(tidyverse)
library(readr)
library(multilevel)

# Step 2: Read Data -------------------------------------------------------
data.to.transform <- read.csv2("Example clean data.csv")

# Step 3: Assess Data (What do you need) ----------------------------------
head(data.to.transform)

# Step 4: Add Teams Labes -------------------------------------------------
data.to.transform$teams <- with(
  data.to.transform,
  paste0(industry.round.1, team.round.1)
)
data.to.transform$teams


# Step 5: Reverse Code Variables ------------------------------------------
data.to.transform$TMScredib4 <- 6 - data.to.transform$TMScredib4
data.to.transform$TMScredib5 <- 6 - data.to.transform$TMScredib5
data.to.transform$TMScoord3 <- 6 - data.to.transform$TMScoord3
data.to.transform$TMScoord5 <- 6 - data.to.transform$TMScoord5

# Step 6: Assess Validity of Items ----------------------------------------
cl <- grep("commitment", colnames(data.to.transform))

cronbachs <- round(cronbach(data.to.transform[, cl])$Alpha, 2)
cronbachs
rwg <- round(mean(rwg.j(data.to.transform[, cl], 
                        data.to.transform$teams)$rwg), 2)
rwg

testvar <- apply(data.to.transform[, cl], 1, mean)
anova <- aov(testvar ~ as.factor(teams), data = data.to.transform)
icc1 <- round(ICC1(anova), 4)
icc1
icc2 <- round(ICC2(anova), 4)
icc2

# Step 7: Calculate Values per Team ---------------------------------------
#' We can write a for loop.
allteams <- unique(data.to.transform$teams)
values <- array(NA, length(allteams))
names(values) <- allteams

for (k in 1:93) {
  print(allteams[k])
}
i <- 1

for (i in 1:length(allteams)) {
  values[i] <- mean(data.to.transform$identification1[data.to.transform$teams ==
    allteams[i]], na.rm = F)
}

#' Or we can use the tapply function.
values2 <- tapply(data.to.transform$identification1,
  data.to.transform$teams, mean,
  na.rm = F
)

#' We should make sure that they are the same.
values == values2

#' Seems that the values are not in the correct oreder.
values <- values[order(names(values))]
values == values2
#' We end up using both...

# Step 8: List all the Columns that you want ------------------------------
columns.to.calc <- c(10, 11, 15:(ncol(data.to.transform) - 1))

# Step 9: Creating a Data Frame and Fill the Values -----------------------
#' Let the for loop iterate over all the columns
calcdata <- data.frame(matrix(NA, length(allteams), length(columns.to.calc)))

colnames(calcdata) <- c(colnames(data.to.transform)[columns.to.calc])

for (i in 1:length(columns.to.calc)) {
  calcdata[, i] <- tapply(data.to.transform[, columns.to.calc[i]],
    data.to.transform$teams,
    mean,
    na.rm = T
  )
}

calcdata$teams <- as.character(sort(allteams))

# Step 10: Calculate Mean for Variables -----------------------------------
calcdata$identification.mean <- apply(calcdata[, 3:6], 1, mean, na.rm = T)
calcdata$cogndivers.mean <- apply(calcdata[, 8:11], 1, mean, na.rm = T)
calcdata$commitment.mean <- apply(calcdata[, 12:15], 1, mean, na.rm = T)
calcdata$power.disparity <- apply(calcdata[, 16:20], 1, function(x) {
  sd(x, na.rm = T) / mean(x, na.rm = T)
})
calcdata$TMSspecial.mean <- apply(calcdata[, 21:25], 1, mean, na.rm = T)
calcdata$TMScredibility.mean <- apply(calcdata[, 26:30], 1, mean, na.rm = T)
calcdata$TMScoord.mean <- apply(calcdata[, 31:35], 1, mean, na.rm = T)

# Step 11: Write and Create the Final Data File ---------------------------
write.csv2(calcdata, "team_statistics.csv")