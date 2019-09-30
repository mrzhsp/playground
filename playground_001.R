# Playground_01

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(euR)

# Data Visualization Tasks ------------------------------------------------
# Task 1:
View(mpg)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg)

nrow(mpg)

ncol(mpg)

View(mtcars)

glimpse(mpg)

glimpse(mtcars)

?drv

ggplot(data = mpg) +
  geom_point(mapping = aes(x = hwy, y = displ))

?mpg
?mtcars

ggplot(data = mtcars) +
  geom_point(mapping = aes(x = hp, y = mpg, color = cyl))

ggplot(data = mpg, aes(x = displ, y = hwy, color = trans)) +
  geom_point() +
  geom_smooth()

ggplot(data = mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE)

?euR

# List all data sets in a package
data(package = "euR")
View(aisle)

View(Prestige)
head(Prestige)
tail(Prestige)

summary(mtcars)
summarize(mtcars)
summarize(mtcars$mpg)

plot(mtcars)
plot(Prestige)

load(patents.RDatafile)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

unique(BenAndJerry$formula_descr)
nlevels(BenAndJerry$flavor_descr)
unique(BenAndJerry$flavor_descr)

load("BenAndJerry.RData")

View(Scotch)
summarize(Scotch)
table(Scotch)
summary(Scotch)

data(package = "euR")

load("/Users/mr.hsp/Documents/Erasmus University - RSM/Courses/Introduction to R/2 - Graphics/patents.RData")
?patents

ggplot(Prestige, aes(x = prestige, y = logincome, color = type)) + geom_point()
ggplot(Prestige, aes(x = prestige)) + geom_bar()
ggplot(Prestige, aes(x = prestige)) + geom_histogram(bins = 15)

data("mtcars")
data("mpg")

ggplot(mpg, aes(x = displ)) + geom_density()
ggplot(mpg, aes(sample = displ)) + geom_qq()
ggplot(mpg, aes(x = drv, y = cty)) + geom_boxplot(notch = TRUE)
ggplot(mtcars, aes(x = cyl)) + geom_bar()
ggplot(mtcars, aes(x = gear)) + geom_bar()

ggplot(patents, aes(x = density, y = total)) + geom_point()
ggplot(patents, aes(x = logdensity, y = logtotal)) + geom_point() + geom_smooth()
ggplot(patents, aes(x = total)) + geom_histogram(bin = 15)
ggplot(patents, aes(x = logtotal)) + geom_histogram(bins = 15)
ggplot(patents, aes(x = total)) + geom_density()
ggplot(patents, aes(x = logtotal)) + geom_density()
ggplot(patents, aes(y = logdensity)) + geom_boxplot()
ggplot(patents, aes(x = "", y = logdensity)) + geom_boxplot()
ggplot(patents, aes(x = densitycat, y = logdensity)) + geom_boxplot()
ggplot(patents, aes(x = governor)) + geom_bar()

data("PhDPublications")

3:8
dim(PhDPublications)
ggplot(PhDPublications, aes(x = kids)) + geom_bar()

b <- (0:5)
c <- (1:5)
prestige_cat <- cut(PhDPublications$prestige, breaks = b, labels = c)
table(prestige_cat)

quantile(PhDPublications$articles)
with(PhDPublications, table(married, kids))

## Exercies for session 3, Intro to R
# Exercise 1.1
load("/Users/mr.hsp/Documents/Erasmus University - RSM/Courses/Introduction to R/3 - Useful Basics and Statistics/houseprice.RData")
dim(houseprice)
plot(houseprice)
summary(houseprice)
mean(houseprice$price)

# Exercise 1.2
median(houseprice$price)
ggplot(houseprice, aes(x = price)) + geom_density() +
  geom_vline(aes(xintercept = mean(houseprice$price)), color = "blue") +
  geom_vline(aes(xintercept = median(houseprice$price)), color = "red")
range(houseprice$price)
table(houseprice)
?houseprice
houseprice
str(houseprice)

table(houseprice$airco)
table(houseprice$bathrms)
table(houseprice$driveway)
table(houseprice$fullbase)
table(houseprice$bedrooms)

# Exercise 1.3
ggplot(houseprice, aes(x = price)) + geom_histogram(bins = 100)
ggplot(houseprice, aes(x = price)) + geom_histogram(bins = 15)
ggplot(houseprice, aes(x = stories)) + geom_bar()
ggplot(houseprice, aes(x = lotsize)) + geom_density()
ggplot(houseprice, aes(sample = lotsize)) + geom_qq()

# Exercise 1.4
ggplot(houseprice, aes(x = price, y = lotsize)) + geom_point()
ggplot(houseprice, aes(x = logprice, y = loglotsize)) + geom_point()
with(houseprice, table(bedrooms, bathrms))
with(houseprice, table(bedrooms, prefarea))
ggplot(houseprice, aes(x = prefarea, y = bedrooms)) + geom_bar()
prop.table(with(houseprice, table(bedrooms, prefarea)), margin = 2)
m <- matrix(c(1:3, 1:3, 1:3), nrow = 3, ncol = 3, byrow = TRUE)

# Exercise 2.1
data("vwgolf")
dim(vwgolf)
summary(vwgolf)
ggplot(vwgolf, aes(x = Mileage, y = AskingPrice)) + 
  geom_point() +
  geom_smooth() + 
  geom_vline(aes(xintercept = mean(vwgolf$Mileage)), color = "red")
ggplot(vwgolf, aes(x = Mileage, y = PriceNew - AskingPrice)) + geom_point()
ggplot(vwgolf, aes(x = Mileage)) + geom_density()
ggplot(vwgolf, aes(x = Fuel, y = Mileage)) + geom_boxplot()
ggplot(vwgolf, aes(x = NrOwners)) + geom_bar()
ggplot(vwgolf, aes(y = NrOwners)) + geom_boxplot()
with(vwgolf, quantile(PriceNew - AskingPrice))
with(vwgolf, table(Transmission, Fuel))
with(vwgolf, min(Mileage, na.rm = TRUE))
min(vwgolf$Mileage, na.rm = TRUE)
range(vwgolf$Mileage, na.rm = TRUE)
