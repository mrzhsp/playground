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



