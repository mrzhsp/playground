#' --------------------------------------------------
#' Title: Data Cleaning Script
#' Subtitle: Quantitative Analysis Workshop 2020
#' Authors: Timo van Balen and Mohamadreza Hoseinpour
#'          Erasmus Universiteit Rotterdam
#' --------------------------------------------------

# Step 1: Load Libraries ----------------------------------------------------------
install.packages("tidyverse")
install.packages("readr")
library(tidyverse)
library(readr)

# Step1.1: Set Working Directory (if needed) ---------------------------------------
#' You can use the command "setwd()". This is needed to determine a directory
#'  for the files that you will be using in R.

dir()
#' This function shows the files in the current directory. So, with this you can
#'  see to which directory your current R session has been set.


# Step 2: Read Data ---------------------------------------------------------------
data.to.clean1 <- read.csv2("Example survey data 1.csv")
data.to.clean2 <- read.csv2("Example survey data 2.csv")

#' In reading the data, pay  attention to how the raw data is structured.
#'  For example, it is important to see whether the data is comma.delimited or
#'  semicolon.delimited.
#' Also, pay attention to the format of numbers. For example, in european style,
#'  a comma is used to indicate decimals. That is why we are using the code
#'  "read.csv2" here and not "read.csv"
#' Normally, the errors that you get at the beginning of data cleaning, are most
#'  of the times about data formatting. So, begin debugging and searching in
#'  google with this aspect in mind.

# Step 3: Look at the Data Sets ---------------------------------------------------
class(data.to.clean1)
class(data.to.clean2)

colnames(data.to.clean1)
colnames(data.to.clean2)

head(data.to.clean1)
head(data.to.clean2)

View(data.to.clean1)
View(data.to.clean2)


# Step 4: Match and Combine the Data ----------------------------------------------
data.to.clean1 <- data.to.clean1[order(data.to.clean1$Subject), ]
data.to.clean2 <- data.to.clean2[order(data.to.clean2$Subject), ]

table(data.to.clean1$Subject == data.to.clean2$Subject)
all(data.to.clean1$Subject == data.to.clean2$Subject)

newdata <- cbind(data.to.clean1, data.to.clean2)
View(newdata)


# Step 5: Remove Unnecessary Variables --------------------------------------------
colnames(newdata)
columns.to.remove <- c(9, 12, 15, 16, 17, 18, 19, 21, 22, 23, 57, 58, 59, 60)
colnames(newdata)[columns.to.remove]
newdata <- newdata[, -columns.to.remove]
colnames(newdata)

# Step 6: Code Missing Values -----------------------------------------------------
newdata[newdata == -77] <- NA

# Step 7: Assess Variables --------------------------------------------------------
summary(newdata$identification1)
summary(newdata$team.performance.t1)

# Step 8: Write and Create a Data File --------------------------------------------
write.csv2(newdata, "Example clean data.csv")
