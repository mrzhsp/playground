# Playground_01

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(euR)
library(colorspace)
library(GGally)

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

## Slides for Session 4, Intro to R
data(Prestige)
data("Eredivisie")
data(Feyenoord)
dim(Prestige)
ggplot(Prestige, aes(x = prestige, y = logincome)) + 
  geom_point() +
  geom_smooth()
ggplot(Prestige, aes(x = prestige)) + geom_density()
ggplot(Feyenoord, aes(x = Year, y = Points)) + geom_line()
ggplot(Feyenoord, aes(x = Year, y = Points)) + 
  geom_point(color = "red")
ggplot(Prestige, aes(x = prestige, y = logincome, color = type)) + 
  geom_point()

ggplot(Prestige, aes(x = prestige)) + 
  geom_density(color = "red", fill = "blue", alpha = 0.1)

ggplot(Prestige, aes(x = prestige, y = logincome)) + 
  geom_point(color = "black", shape = 21, size = 3, fill = "green")

ggplot(Prestige, aes(x = prestige)) +
  geom_density(linetype = 4, size = )

ggplot(Feyenoord, aes(x = Year, y = Points)) +
  geom_line(color = "red", linetype = "longdash", size = 0.6)

ggplot(Prestige, aes(x = prestige, y = logincome, fill = type)) +
  geom_point(shape = 21, size = 3)

ggplot(Eredivisie, aes(x = Year, y = Points, color = Team)) +
  geom_line()

ggplot(Prestige, aes(x = prestige, y = logincome, shape = type)) + 
  geom_point(size = 3) + 
  scale_shape_manual(values = 1:3)

ggplot(Eredivisie, aes(x = Year, y = Points, color = Team)) + 
  geom_line() +
  scale_color_manual(values = c("black", "red"))

ggplot(Prestige, aes(x = prestige, y = logincome, fill = women)) + 
  geom_point(shape = 23, color = "red", size = 3) +
  scale_fill_continuous(low = "white", high = "yellow")

palette()
rainbow_hcl(4)
pal <- choose_palette(rainbow_hcl(4))
diverge_hcl(7)
# I had problems in doing these!

ggplot(Prestige, aes(x = prestige, y = logincome, size = women)) +
  geom_point(shape = 21, color = "black", fill = "red") +
  scale_size_continuous(range = c(0, 8))

ggplot(Prestige, aes(x = type, fill = type)) +
  geom_bar(show.legend = FALSE)

ggplot(Prestige, aes(x = type, fill = type)) +
  geom_bar(show.legend = FALSE) +
  coord_flip()

ggplot(Prestige, aes(x = type, y = prestige, fill = type)) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  theme_bw()

ggplot(Eredivisie, aes(x = Year, y = Points, color = Team)) +
  geom_line() +
  scale_color_discrete() +
  ylim(25, 90) +
  labs(title = "Team Performance") 

ggplot(Prestige, aes(x = prestige, y = logincome, size = women)) +
  geom_point(shape = 21, color = "black", fill = "red") +
  geom_smooth(method = "lm", color = "yellow")

ggplot(Eredivisie, aes(x = Year, y = Points, color = Team)) + 
  geom_line() + geom_point() +
  scale_color_manual(values = c("black", "red")) +
  geom_vline(xintercept = 1996, color = "yellow") +
  ylim(0, 102)

eredivisie_wins <- filter(Eredivisie, Year >= 1996)

ajax_info <- filter(Eredivisie, Team == "Ajax" & Year >= 1996)
ggplot(ajax_info, aes(x = Year, y = Points)) + geom_line()

prestige_num <- select(Prestige, -type)
plot(prestige_num)

ggpairs(Prestige)
ggpairs(patents)

## Exercies for session 4, Intro to R
# Exercise 1.1
data(patents)
ggplot(patents, aes(x = logtotal, color = densitycat)) + geom_density()

# Slides for Session 5, Intro to R ----------------------------------------
data("Matches")
str(Matches)

ggplot(Matches, aes(x = Result, fill = Place)) + 
  geom_bar(position = "dodge")

home <- gather(home_raw, 
               key = Season, 
               value = Result, 
               -Opponent,
               factor_key = TRUE,
               na.rm = TRUE)
away <- gather(away_raw,
               key = Season,
               value = Result,
               -Opponent,
               factor_key = TRUE,
               na.rm = TRUE)
str(home)
str(away)

home <- separate(home,
                 Result,
                 c("Goals_scored", "Goals_against"),
                 sep = "-",
                 convert = TRUE)
away <- separate(away,
                 Result,
                 c("Goals_scored", "Goals_against"),
                 sep = "-",
                 convert = TRUE)
matches <- rbind(cbind(Place = "Home", home),
                 cbind(Place = "Away", away))
matches$Opponent <- recode(matches$Opponent,
                           "Sparta R." = "Sparta Rotterdam")
ggplot(matches, aes(x = Opponent)) + 
  geom_bar() +
  coord_flip()

matches <- mutate(matches, 
                  Goal_difference = Goals_scored - Goals_against,
                  Points = 3 * (Goal_difference > 0) +
                    (Goal_difference == 0))
str(matches)
matches <- mutate(matches,
                  Result = Points)
matches$Result <- recode_factor(matches$Points,
                                "0" = "Lost",
                                "1" = "Drawn",
                                "3" = "Won")
ggplot(matches, aes(x = Result, fill = Place)) + 
  geom_bar(position = "dodge") +
  facet_grid(. ~ Place)

summarize_at(matches,
             vars(Goals_scored:Points),
             mean)

summarize(matches,
          Avg_goals_scored = mean(Goals_scored),
          Avg_goals_against = mean(Goals_against),
          Points = sum(Points))

grp_matches <- group_by(matches, 
                            Season, 
                            Place)
sum_grp_matches <- summarize(grp_matches,
                Avg_goals_scored = mean(Goals_scored),
                Avg_goals_against = mean(Goals_against),
                Sum_points = sum(Points))
str(sum_grp_matches)
head(sum_grp_matches)

ggplot(sum_grp_matches,
       aes(x = Sum_points, y = Season, color = Place)) +
  geom_point() +
  xlim(0, 51)

reordered_sum_grp_matches <- sum_grp_matches       
reordered_sum_grp_matches$Season <- fct_rev(sum_grp_matches$Season)
head(reordered_sum_grp_matches)

# Slides for Session 6, Intro to R ----------------------------------------
library(DBI)
library(RSQLite)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "movies.sqlite")

dbListTables(con)
dbListFields

data("band_members")
data("band_instruments")

inner_join(band_members, band_instruments)
left_join(band_members, band_instruments)
right_join(band_members, band_instruments)
full_join(band_members, band_instruments)

# Advanced R, Slides for Session 1 ----------------------------------------
data("cityweather")
head(cityweather)
?cityweather
summary(cityweather)

cityweather$RainDays <- 
  (cityweather$RainDays - min(cityweather$RainDays)) /
  (max(cityweather$RainDays) - min(cityweather$RainDays))

#' Title: Range Standardizor (scale01)
#'
#' @param x 
#' @param remove_na 
#'
#' @return The standardized range of each variable x
#' @export
#'
#' @examples
scale01 <- function(x, na.rm = TRUE) {
  min_x <- min(x, na.rm = na.rm)
  max_x <- max(x, na.rm = na.rm)
  (x - min_x)/(max_x - min_x)
}

cityweather$RainDays <- scale01(cityweather$RainDays)
cityweather$SunHours <- scale01(cityweather$SunHours)
cityweather$AvgWarm <- scale01(cityweather$AvgWarm)
cityweather$AvgCold <- scale01(cityweather$AvgCold)
cityweather$Rainfall <- scale01(cityweather$Rainfall)
summary(cityweather)

scale01(c(1, 3, 5, 5, NA))

#' Title
#'
#' @param x 
#'
#' @return A vector that produce powers of x
#' @export
#'
#' @examples
powers <- function(x) {
  c(quadratic = x^2, cubic = x^3, quartic = x^4, quintic = x^5)
}
powers(5)

## When we are designing a function, always follow this procedure.
# test case
# calcualtion
# output
# function

# Exercise 1.1.a ----------------------------------------------------------
pow <- function(x, power) {
  x ^ power
}
pow(x = 1:5, power = 5)
pow(x = pi, power = -2)

# Exercise 1.1.b ----------------------------------------------------------
# this is my own which does not work
odd <- function(x) {
  remainder = x %% 2
  if (remainder > 0) {
    odd = "FALSE"
  } else {
    odd = "TRUE"
  }
  tibble(x, remainder, odd)
}
odd <- function(x) {
  remainder <- x %% 2
  odd = remainder == 1
  tibble(x, remainder, odd)
}

# test case
x <- 4
remainder <- x %% 2
if (remainder > 0) odd = "TRUE" else odd = "FALSE"

odd(4)
odd(c(11, -2, 4, -19, 6))

## Solution
# Test case
x <- c(11, -2, 4, -19, 6)

# Calculation
remainder <- x %% 2
remainder
check <- remainder == 1
check

# Output
data.frame(x = x,
           remainder = remainder,
           odd = check)

# Function
odd <- function(x) {
  remainder <- x %% 2
  check <- remainder == 1
  data.frame(x = x,
             remainder = remainder,
             odd = check)
}

# Test function
odd(1:10)

# Exercise 1.1.c ----------------------------------------------------------
# Test case
x <- c(1, 2)
y <- (1 * x ^ 2) + (1 * x) + 0

# Function
quadratics <- function(x, a = 1, b = 1, c = 0) {
  y = (a * x ^ 2) + (b * x) + c
  y
}
quadratics(x = 1:5)
quadratics(x = 1:5, a = 5, b = -3, c = 1)

## Solution
# Test case
x <- 1:5
a <- 5
b <- -3
c <- 1

# Calculations
a*x^2 + b*x + c

# Exercise 1.1.d ----------------------------------------------------------
# Test case
(a * x ^ 2) + (b * x) + c
a <- 1
b <- 1
c <- 0

# Calculations
y1 <- (-b + sqrt(b ^ (2 - 4 * a * c)))/(2 * a)
y2 <- (-b - sqrt(b ^ (2 - 4 * a * c)))/(2 * a)

term1 <- -b / (2 * a)
term2 <- sqrt(b ^ 2 - 4 * a * c) / (2 * a)


# Output
solutions <- c(y1, y2)

c(term1 - term2,
  term1 + term2)

# Function
solve_quadratic <- function (a, b, c) {
  y1 = (-b + sqrt(b ^ 2 - 4 * a * c))/(2 * a)
  y2 = (-b - sqrt(b ^ 2 - 4 * a * c))/(2 * a)
  solutions <- c(y1, y2)
  solutions
}
solve_quadratic(a = 40, b = 11, c = -7)
solve_quadratic(a = 1, b = 2, c = 1)
solve_quadratic()

cityweather <- map_df(cityweather, scale01)
map(cityweather, min)
map_dbl(cityweather, max)

# Exercise 2.1.a ----------------------------------------------------------
a <- map_df(cityweather, median)
b <- map_df(cityweather, mad)
c <- rbind(a, b)

# Solution
data("cityweather")
# We shoudl always start with simple
map(cityweather, median)
map_dbl(cityweather, median)

map(cityweather, mad)
map_dbl(cityweather, mad)

median_mad <- function(x) {
  c(median(x), mad(x))
}

map(cityweather, median_mad)
res <- map_df(cityweather, median_mad)
res %>% mutate(Statistics = c("median", "mad")) %>% 
  select(Statistics, everything())

# Exercsie 2.1.c ----------------------------------------------------------
map_df(cityweather, class)

class <- function (x) {
  map_chr(x, class)
}
class(cityweather)

# Exercise 1.2.a
# Test case
x <- 1:100
threshold <- 20
# we can try other values of X and Threshold to check the calcuations

# calculations
# 1. check <= threshold
# 2. number 
check_x <- x <= threshold
nr_below <- sum(check_x)
prop_below <- nr_below/length(x)

# Output
prop_below

# Function
prop_below <- function(x, threshold) {
  check_x <- x <= threshold
  nr_below <- sum(check_x)
  nr_below/length(x)
}
prop_below(1:10, 5)

# But it deos not work with missing values
# Exercise 1.2.b
# Test case
x <- c(1:10, NA)
threshold <-5
# we can try other values of X and Threshold to check the calcuations

# calculations
# 1. check <= threshold
# 2. number for which this is true
# 3. Divide by length of x
check_x <- x <= threshold
nr_below <- sum(check_x, na.rm = TRUE)
nr_below/sum(!is.na(x))

# Output
prop_below

# Function
prop_below <- function(x, threshold) {
  check_x <- x <= threshold
  nr_below <- sum(check_x, na.rm = TRUE)
  nr_below/sum(!is.na(x))
}
prop_below(c(1:10, NA), 5)

# Continuing with slides from page 59
data("Affairs")
View(Affairs)
head(Affairs)

# In R categorical variables are called factor!
map(Affairs, class)
str(Affairs)

# Conditional statements
# the condition should only be of one element
num_mean <- function(x, na.rm = TRUE) {
  if (is.numeric(x)) {
    mean(x, na.rm = na.rm)
  } else {
    NaN
  }
}
map_dbl(Affairs, num_mean)

summary(Affairs)
# we can use summary to check the data as well because it gives counts for
#' factor variables

# We can use else to prevent R to check for both conditions when checking
# IMP: if we need a vectorized version of if, then whe have to use another 
#' command of [ifelse], then it would work for a number of elements.

ifelse(1:10 > 5, "yes", "no")

# Exercise 3.1.a
data(Prestige)
View(Prestige)

# test case
x <- median(Prestige$education)
x

median_num <- function(x, na.rm = TRUE) {
  if (is.numeric(x)) {
    median(x, na.rm = TRUE)
  } else {
    NaN
  }
}
map(Prestige, median_num)

# Other solution
map(select_if(Prestige, is.numeric), median)
map_if(Prestige, is.numeric, median)

# test case
x <- Prestige$type

# calculation
median(x)

if (is.numeric(x)) {
  median(x)
} else {
  NaN
}

# Function
num_median <- function (x) {
  if (is.numeric(x)) {
    median(x)
  } else {
    NaN
  }
}

map(Prestige, num_median)
map_df(Prestige, num_median)

# Exercsie 3.1.b
data("vwgolf")
View(vwgolf)

num_category <- function(x, na.rm = TRUE){
  if (is.factor(x)) {
    unique(x)
  } else {
    NaN
  }
}
map(vwgolf, num_category)


# Exercise 3.2.a
x <- Prestige$type
if (is.numeric(x)) {
  quantile(x)
} else {
  (is.factor(x))
  table(x)
}

my_summary <- function(x) {
  if (is.numeric(x)) {
    quantile(x)
  } else {
    (is.factor(x))
    table(x)
  }
}
map(Prestige, my_summary)

# Other solution
my_summary <- function(x) {
  if (is.numeric(x)) {
    quantile(x)
  } else if (is.factor(x)) {
    table(x)
  } else {
    NaN
  }
}
map(Prestige, my_summary)

# Exercise 3.2.b

solve_quadratic2 <- function (a, b, c) {
  if ((b ^ 2 - 4 * a * c) > 0) {
    y1 = (-b + sqrt(b ^ 2 - 4 * a * c))/(2 * a)
    y2 = (-b - sqrt(b ^ 2 - 4 * a * c))/(2 * a)
    solutions <- c(y1, y2)
    solutions
  } else {
  stop("There are no real roots")
}
}

solve_quadratic2(a = 40, b = 11, c = -7)
solve_quadratic(a = 1, b = 2, c = 1)
solve_quadratic2(a = 1, b = 1, c = 1)

