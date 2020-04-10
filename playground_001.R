# Playground_01

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(euR)
library(colorspace)
library(GGally)

# Intro R, Data Visualization ------------------------------------------------

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

RColorBrewer::display.brewer.all()

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

# Intro R, Slide 5 ----------------------------------------
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

# Intro R, Slide 6 ----------------------------------------
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

# Adv R, Slide 1 ----------------------------------------
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

# Adv R, Slide 1, Ex 1.1.a ----------------------------------------------------------
pow <- function(x, power) {
  x ^ power
}
pow(x = 1:5, power = 5)
pow(x = pi, power = -2)

# Adv R, Slide 1, Ex 1.1.b ----------------------------------------------------------
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

# Adv R, Slide 1, Ex 1.1.c ----------------------------------------------------------
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

# Adv R, Slide 1, Ex 1.1.d ----------------------------------------------------------
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

# Adv R, Slide 1, Ex 2.1.a ----------------------------------------------------------
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

# Adv R, Slide 1, Ex 2.1 ----------------------------------------------------------
## Exercies 2.1.a
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




# ADv R, Slide 1, Conditions -------------------------------------------------------
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

# Adv R, Slide 1, Ex 3.1. ---------------------------------------------------
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


# Adv R, Slide 1, Ex 3.2 ---------------------------------------------------
## Exercise 3.2.a
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

# Adv R, Slide 1, Ex 1.2.a & b -----------------------------------------------------------
prop_below2 <- function(x, threshold, na.rm = TRUE) {
  sum(x <= threshold, na.rm = na.rm) / (length(x) - sum(is.na(x)))
}
prop_below2(1:100, 5)
prop_below2(c(1:100, NA), 50)

# Adv R, Slide 1, Ex 1.2.c ---------------------------------------------------------
# I can write a function that can produce a random sample with mean and SD.

my_rnorm <- function(n, mean, sd) {
  output <- c(mean + sd * scale(rnorm(n)))
}
my_rnorm(1000, 0, 1)
prop_below2(my_rnorm(1000, 0, 1), 1.96)

# Adv R, Slide 1, Ex 1.3.a ---------------------------------------------------------
x <- seq(-pi, pi, length.out = 100)
d <- data.frame(x = x, y = sin(x))
ggplot(d, aes(x = x, y = y)) +
  geom_line()

draw_curve2 <- function(low, high, length) {
  x <- seq(low, high, length.out = length)
  df <- data.frame(x = x, y = sin(x))
  ggplot(df, aes(x = x, y = y)) +
    geom_line()
}
draw_curve2(-pi, pi, 100)
draw_curve2(0, pi / 2, 100)

num_mean <- function(x, na.rm = TRUE) {
  if (is.numeric(x)) {
    mean(x, na.rm = na.rm)
  } else {
    NaN
  }
}
map_dbl(Affairs, num_mean)

# Adv R, Slide 1, Ex 1.4.a ---------------------------------------------------------


# Adv R, Slide 1, Ex 3.3 --------------------------------------------------

## Exercise 3.3.a
# Test case
vwgolf2 <- vwgolf
test <- (vwgolf2$RoadTaxPM - mean(vwgolf2$RoadTaxPM)) / (sd(vwgolf2$RoadTaxPM))
mean(test)
all.equal(mean(test), 0)
sd(test)

# Turn into function
scale_mean_sd <- function(x) {
  if(is.numeric(x)) {
    (x - mean(x)) / sd(x)
  } else {
    x
  }
}
scale_mean_sd(vwgolf2$Version)

scale_mean_sd <- function(data) {
  scale_mean <- function(column) {
    if(is.numeric(column)) {
      (column - mean(column)) / sd(column)
    } else {
      column
    }
  }
  modify(data, scale_mean)
}

scaled_vwgolf <- scale_mean_sd(vwgolf2)
View(scaled_vwgolf)

scaled_cityweather <- scale_mean_sd(cityweather)
View(scaled_cityweather)

map_dbl(scaled_vwgolf, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else NA)
map_dbl(scaled_vwgolf, function(x) if (is.numeric(x)) sd(x, na.rm = TRUE) else NA)

# Adv R, Slide 1, Ex 3.4 --------------------------------------------------
## Solution in the calss
# 0. Inputs
switch <- TRUE

# 1. Setup the game
doors <- paste("door", 1:3)
correct_door <- sample(doors, 1)

# 2. Contestant Guesses
guessed_door <- sample(doors, 1)

# 3. Host opens a door
cannot_open <- union(correct_door, guessed_door)
can_open <- setdiff(doors, cannot_open)
opened_door <- sample(can_open)

# 4. contestant applies his/her strategy
if (switch) {
  cannot_switch_to <- union(guessed_door, opened_door)
  guessed_door <- setdiff(doors, cannot_switch_to)
}

# 5. Check what the contestant won
guessed_door == correct_door

# 6. Turn it into a function
monty_hall <- function(switch) {
  doors <- paste("door", 1:3)
  correct_door <- sample(doors, 1)
  guessed_door <- sample(doors, 1)
  cannot_open <- union(correct_door, guessed_door)
  can_open <- setdiff(doors, cannot_open)
  opened_door <- sample(can_open)
  if (switch) {
    cannot_switch_to <- union(guessed_door, opened_door)
    guessed_door <- setdiff(doors, cannot_switch_to)
  }
  guessed_door == correct_door
}

# 7. Play many games
monty_hall(switch = TRUE)
monty_hall(switch = FALSE)

# 8. Iterate
b <- 100
res_true <- replicate(b, monty_hall(TRUE))
mean(res_true)

# Exercise 3.4.a
monty_hall <- function(switch) {
  switch <- TRUE
  set1 <- paste("door", 1:3)
  correct_door <- sample(set1, 1)
  guessed_door <- sample(set1, 1)
  cannot_open <- union(correct_door, guessed_door)
  can_open <- setdiff(set1, cannot_open)
  opened_door <- sample(can_open, 1)
  remaining_door <- setdiff(set1, union(opened_door, guessed_door))
  if (switch == TRUE) {
    guessed_door <- remaining_door
  }
  guessed_door == correct_door
}
set.seed(4155)
monty_hall(TRUE)
monty_hall(FALSE)

# Exercise 3.4.b
reps <- 100000
results_TRUE <- replicate(reps, monty_hall(TRUE))
mean(results_TRUE)

results_FALSE <- replicate(reps, monty_hall(FALSE))
mean(results_FALSE)

# Adv R, Slide 2, Ex 1.1 --------------------------------------------------
city <- c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht", "Eindhovem",
          "Tilburg", "Almore")
province <- c("NH", "ZH", "ZH", "UT", "NB", "NB", "FL")
population <- c(860, 641, 534, 349, 230, 216, 205)
province_population <- c(ZH = 3607, NH = 2776,
                         NB = 2495, UT = 1268,
                         GD = 2031, FL = 403)
## Ex 1.1.a
population[c(3, 5, 7)]

## Ex 1.1.b
city[-c(2, 4, 7)]
city[c(1, 3, 5, 6)]
city[c(-2, -4, -7)]

## Ex 1.1.c
city[province == "UT"]
city[province == "NB"]
c(city[province == "UT"], city[province == "NB"])
city[province == "UT" | province == "NB"]
# This is the ideal one
city[province %in% c("UT", "NB")]
city[which(province == "UT" | province == "NB")]

## Ex 1.1.d
sum(population[province == "ZH"])
sum(
  population[which(province == "ZH")]
)

## Ex 1.1.e
n <- length(city)
city[(n-1):n]
length(city)

city[c(
  (length(city) - 1),
  (length(city) - 2)
)]

## Ex 1.1.f
#' We can solve this by merging the two and making a data frame.

# 1. Find province population for each city
province_population[province]
# 2. Divide the city population by province population
population / province_population[province]
# 3. Turn it into a percentage
100 * population / province_population[province]
# 4. Fix names
res <- 100 * population / province_population[province]
names(res) <- city
res


# Adv R, Slide 2, Ex 2.1 -----------------------------------------------------------
# Ex 2.1.a
city
sort(city)
sort(city, decreasing = TRUE)
city[order(city)]

# Ex 2.1.b
sort(province, decreasing = TRUE)
order(province, decreasing = TRUE)
city[order(province, decreasing = TRUE)]

# Ex 2.1.c
population
order(province, city)
population[order(province, city)]

province[order(province, city)]
city[order(province, city)]

# Ex 2.1.d
names(province_population)[1] <- c("Zuid-Holland")

# Ex 2.1.e
population[city == "Rotterdam"] <- paste0(population[city == "Rotterdam"], "*")
population
typeof(population)


# Adv R, Slide 2, Ex 3.1 ---------------------------------------------------------
load("/Users/mr.hsp/Documents/Erasmus University - RSM/Courses/Advanced R 2019/hot100.RData")
str(chart_weeks)

# Ex 3.1.a
song_title[10]
artists[10]
featured_artists[10]
main_artists[10]

# Ex 3.1.b
which(song_title == "Let Me Go")
main_artists[which(song_title == "Let Me Go")]
main_artists[[which(song_title == "Let Me Go")]]
main_artists$'Let Me Go'

main_artists[["Let Me Go"]]
featured_artists[["Let Me Go"]]

# Ex 3.1.c
main_artists[1:5]

# Adv R, Slide 2, 3.2 -----------------------------------------------------

## In-calss solution

# 1. Function for one vector of ranks, and one value of k

# Test case
n <- 8
r <- sample.int(n)
k <- 4

# Calculations
r[seq_len(k)]
best_explore <- min(r[seq_len(k)])
r < best_explore
seq_along(r) > k
potential_matches <- r < best_explore & seq_along(r) > k
matched_rank <- if(any(potential_matches)) {
  ind_match <- which(potential_matches)[1]
  r[ind_match]
} else {
  r[length(r)]
}

# Return
matched_rank

# Function
match_partner <- function(r, k){
  best_explore <- min(r[seq_len(k)])
  potential_matches <- r < best_explore & seq_along(r) > k
  matched_rank <- if(any(potential_matches)) {
    ind_match <- which(potential_matches)[1]
    r[ind_match]
  } else {
    r[length(r)]
  }
  matched_rank
}

# Test the function again
r <- sample.int(n)
match_partner(r, 1)
match_partner(r, 2)
match_partner(r, 3)
match_partner(r, 4)

# 2. Function that handles once vector of ransk but for all k
n <- length(r)
map(1:(n-1), match_partner, r = r)

match_all <- function(r) {
  n <- length(r)
  map_int(seq_len(n - 1), match_partner, r = r)
}

# Function
match_all(r)

# 3. Run this repeatedly for many vectros of ranks

# Test case
reps <- 100
n <- 8

r <- sample.int(n)
match_all(r)

res <- replicate(reps, match_all(sample.int(n)))

prop_matches <- rowMeans(res == 1)

data.frame(prop_explore = seq_len(n - 1) / n,
           prop_matches = prop_matches)

rep_match_all <- function(reps, n) {
   res <- replicate(reps, match_all(sample.int(n)))
   prop_matches <- rowMeans(res == 1)
   data.frame(prop_explore = seq_len(n - 1) / n,
              prop_matches = prop_matches)
}

rep_match_all(1000, 40)
plot(rep_match_all(1000, 40))

## 3.2.a) my own solution
match_partner <- function(r, k) {
  n <- length(r)
  best_rank_seen <- min(r[seq_len(k)])
  better_choice <- r < best_rank_seen & seq_len(n) > k
  chosen_partner <- if (any(better_choice)) {
    which(better_choice)[1]
  } else {
    n
  }
  chosen_rank <- r[chosen_partner]
  chosen_rank
}


x <- 10
r <- sample.int(x)
r
k <- 3
n <- length(r)
best_rank_seen <- min(r[seq_len(k)])
better_choice <- r < best_rank_seen & seq_len(n) > k
chosen_partner <- if (any(better_choice)) {
  which(better_choice)[1]
} else {
  n
}
chosen_rank <- r[chosen_partner]
chosen_rank

# Adv R, Slide 2, Ex 3.3 -----------------------------------------------------

# Adv R, Slide 2, Ex 3.4 -----------------------------------------------------


# Adv R, Slide 3 ----------------------------------------------------------

for (i in 1:5) {
  1:i
  cat("Iteration", i, "\n")
}


# Adv R, Slide 3, Ex 1.1 --------------------------------------------------
# Exercise 1.1.a
data("cityweather")

map(cityweather, max)
map_dbl(cityweather, max)

# 1. Output
ncol(cityweather)
length(cityweather)
output <- vector("double", ncol(cityweather))

# Just something funny: rep(NA, ncol(cityweather))

# 2. Iterating
seq_along(cityweather)
names(cityweather)
for (i in seq_along(cityweather)) {
  output[[i]] <- max(cityweather[[i]])
}

# 3. Body
i <- 2
cityweather[[i]]
cityweather[i]

max(cityweather[[i]])

# 4. Everything
output <- vector("double", ncol(cityweather))
for (i in seq_along(cityweather)) {
  output[[i]] <- max(cityweather[[i]])
}
output
names(output) <- names(cityweather)
output

# My solution
output <- vector("double", ncol(cityweather))
for (i in seq_along(cityweather)) {
  output[[i]] <- max(cityweather[[i]])
}
output

# Exercise 1.1.b

# 1. Output
data.frame(median = vectro("double", ncol(cityweather)),
           mad = vector("double", ncol(cityweather)))
# or
res <- as.data.frame(matrix(0, ncol(cityweather), 2))
res <- matrix(0, ncol(cityweather), 2)
colnames <- c("median", "mad")
res <- as.data.frame(res)
res

# 2. Iterating
seq_along(cityweather)
for (i in seq_along(cityweather)) {
}

# 3. Body
i <- 3
res[i, ] <- c(median(cityweather[[i]]),
  mad(cityweather[[i]]))

# 4. Everything
res <- data.frame(median = vector("double", ncol(cityweather)),
           mad = vector("double", ncol(cityweather)))
for (i in seq_along(cityweather)) {
  res[i, ] <- c(median(cityweather[[i]]),
                mad(cityweather[[i]]))
}
res
res$variable <- names(cityweather)
res[, c(3, 1:2)]
names(res)

# My solution
output <- vector("double", ncol(cityweather))
for (i in seq_along(cityweather)) {
  output[[i]] <- median(cityweather[[i]])
}
output


# Adv R, Slide 3, Ex 1.2 --------------------------------------------------

# Test case
x <- rnorm(10)
max(x)

# Calculations

# 1. Output
res <- x[1]
# or -Inf

# 2. Iteration
seq_along(x)

# 3. Body
i <- 10
if (x[i] > res) {
  res <- x[i]
}

# 4. Loop
res <- x[1]
for (i in seq_along(x)) {
  if (x[i] > res) {
    res <- x[i]
  }
}
res

# Function
my_max <- function(x) {
  res <- x[1]
  for (i in seq_along(x)) {
    if (x[i] > res) {
      res <- x[i]
    }
  }
  res
}

x <- rnorm(10)


# My solution
# Output
output

# Iteration
seq_along(x)
for (i in seq_along(x)) {
  output[[i]] <- max(x[[i]])
}

# Body

# Loop
x <- c(1, 4, 2, 3)
max_x <- vector("double", 1)
for (i in seq_along(x)) {
  max_x <- max(x)
}
max_x

x <- rnorm(1000)
max(x)

# Adv R, Slide 3, Ex 1.3 --------------------------------------------------

# Exercise 1.3.a
# Test
n <- 10
factorial(n)
# Loop
res <- 1
for (i in seq_len(n)) {
  res <- res * i
  cat("Iteration:", i, "Value", res, "\n", sep = " ")
}
res
prod(seq_len(n))

# While - Solution 1
result <- 1
i <- 0
while (i < n) {
  # We have to stop the counter
  i <- i + 1
  cat("Value:", i, "\n")
  result <- result * i
}
result

# While - Solution 2
result <- 1
i <- 1
while (i <= n) {
  # We have to stop the counter
  cat("Value:", i, "\n")
  result <- result * i
  i <- i + 1
}
result

# Adv R, Slide 3, Ex 2.4 --------------------------------------------------

## Inclass Solution

# Test case
x <- sample.int(50)
sort(x)

# 1. Inner Loop: Swaps

# 2. Outer loop: repeat 1 until sorted
len_x <- length(x)
swaps <- len_x - 1
j <- 0
while (swaps > 0) {
  swaps <- 0
  for (i in seq_len(len_x - 1)) {
    if (x[i + 1] < x[i]) {
      x[i:(i + 1)] <- rev(x[i:(i + 1)])
      swaps <- swaps + 1
    }
  }
  j <- j + 1
}
x
j

# 1. For loop
x
len_x <- length(x)
swaps <- 0
for (i in seq_len(len_x - 1)) {
  if (x[i + 1] < x[i]) {
    x[i:(i + 1)] <- rev(x[i:(i + 1)])
    swaps <- swaps + 1
  }
}
x
swaps

# Output: updated x, swaps

# iteration sequence

# Body: make all swaps

x <- c(1, 0, 3, 2, 1, 6)
y <- x
i <- 1
iterate <- TRUE
while (i <= length(x)-1) {
  iterate <- FALSE
  if (y[[i]] > y[[i + 1]]) {
    y[[i + 1]] <- y[[i]]
    y[[i]] <- x[[i + 1]]
    iterate <- TRUE
  }
  i <- i + 1
}
y


# Adv R, slide 3, Debugging -----------------------------------------------
x <- c(1, 1, 0, 1, 1, 1)

find_runs <- function(x, k = 2, value = 1) {
  len_x <- length(x)
  runs <- vector("integer", 0)
  for (i in 1:(len_x - k)) {
    if (all(x[i:i + k - 1] == value)) {
      runs <- c(runs, i)
    }
  }
  runs
}

find_runs(x)

source(find_runs)

load("/Users/mr.hsp/Downloads/find_runs.R")



# Adv R, Slide 3, Exercise 2.3 --------------------------------------------

# In-class solution

# Test case
x <- 11 # 1011

# Code to represent x in binary
floor

5 %% 2 # Modulo
5 %/% 2 # Integer division
floor(5/2)

power <- floor(log2(x))

output <- integer(power + 1)

# Starter with 2^3
x/ 2 ^ power
x %/% 2 ^ power
output[power + 1] <- 1
remainder <- x %% 2 ^ power

# Continue with remainder
power <- floor(log2(remainder))
output[power + 1] <- 1
remainder <- remainder %% 2 ^ power
power <- floor(log2(remainder))
output[power + 1] <- 1
remainder <- remainder %% 2 ^ power

# Loop
x <- 111
# Initializing remainder
remainder <- x
# Settin up output
power <- floor(log2(remainder))
output <- integer(power + 1)

while (remainder > 0) {
  power <- floor(log2(remainder))
  output[power + 1] <- 1
  remainder <- remainder %% 2^power
  cat(output, "\n")
}
output
rev(output)
output[length(output):1]
paste(rev(output), collapse = "")


# Test case
x <- 12 # 1011
# 11 = 1*2^3 + 0

x <- 11
y <- x
i <- 1
output <- vector("integer", power + 1)
while (y > 1) {
  power1 <- floor(log2(y))
  output[[power1 + 1]] <- if (power1 != 0 ) {
    1
  } else {
    0
  }
  y <- y - 2 ^ power1
  i <- i + 1
}
output



floor(log2(3))

# code to represent x in binary
power1 <- floor(log2(x))
col <- if (power1 != 0 ) {
  1
} else {
  0
}

y <- x - 2 ^ power1



r2 <- floor(log2(r1))
y2 <- floor(log2(r2))
1 * 2 ^ (r1)


# Adv R, Slide 4, Ex 1.5 --------------------------------------------------

# In-class Solution
n <- 10

pos <- seq_len(n)
cur_rem <- length(pos)
pos <- kill(pos)
if(cur_rem %% 2 == 1) {
  pos <- c(pos[lenght(pos)], pos[-length(pos)])
}
cur_rem <- length(pos)

## Loop
n <- 10

pos <- seq_len(n)
cur_rem <- length(pos)

while (cur_rem > 1) {
  pos <- kill(pos)
  # Move last one to front if odd number
  if(cur_rem %% 2 == 1) {
    pos <- c(pos[lenght(pos)], pos[-length(pos)])
  }
  # Update the number remaining
  cur_rem <- length(pos)
}

josephus <- function(n) {
  pos <- seq_len(n)
  cur_rem <- length(pos)
  
  while (cur_rem > 1) {
    pos <- kill(pos)
    # Move last one to front if odd number
    if(cur_rem %% 2 == 1) {
      pos <- c(pos[length(pos)], pos[-length(pos)])
    }
    # Update the number remaining
    cur_rem <- length(pos)
  }
  pos
}

v <- c(1, 2, 3, 7)

b <- c(v[length(v)], v[-length(v)])

josephus(10)

# First round: eliminate even position
ind_even <- seq_along(orig_pos) %% 2 == 0
rem_pos <- orig_pos[!ind_even]

kill <- function(pos) {
  ind_even <- seq_along(pos) %% 2 == 0
  pos[!ind_even]
}

kill(1:10)
kill(c(1, 3, 5, 7, 9))
kill(c(9, 1, 5))
kill(c(9, 5))


# Atabak's code for this problem
  saveds <- function (n){
    people <- (1:n)
    saved <- people
    while (length(saved)>1)
    {
      
      for (i in 1:length(saved)) {
        if(!is.na(saved[[i]])   ){
          if(i<length(saved)){
            saved[[i+1]]<-NA
          }else{
            saved[[1]]<-NA
          }
          
        }
      }
      saved<-saved[!is.na(saved)]
    }
    saved
    
  }






# My own
n <- 5

x <- rep(1, n)
i <- n
while (i > 0) {
  remainder <- i %% 2
  if (remainder == 0) {
    x[[i]] <- 0
  }
  i <- i - 1
}

while (i > 0) {
  if (x[i] == 1) {
    x[1] <- 0
  }
  
}








