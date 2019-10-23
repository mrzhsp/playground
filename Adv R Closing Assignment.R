# ------------------------------------------- #
# Topic: ERIM Advanced R: Closing Assignment
# Author: Mohamadreza Hoseinpour
# Student ID: 518801
# ------------------------------------------- #

# Initial Setup -----------------------------------------------------------
## Requirements
library(tidyverse)
load("stocks.RData")

## Getting a sense of data
start_date <- as.Date("1990-01-01")
end_date <- as.Date("2015-12-31")
retail_stocks <- stocks %>% filter(date >= start_date, date <= end_date) %>% 
  select(date, TGT, WMT) %>% mutate(ratio = TGT/WMT)
mean_ratio_retail_stocks <- mean(retail_stocks$ratio)
sd_ratio_retail_stocks <- sd(retail_stocks$ratio)

ggplot(retail_stocks, aes(x = date, y = ratio)) +
  geom_line() +
  geom_hline(aes(yintercept = mean_ratio_retail_stocks), color = "blue") +
  geom_hline(aes(yintercept = mean_ratio_retail_stocks + sd_ratio_retail_stocks * k), color = "red") +
  geom_hline(aes(yintercept = mean_ratio_retail_stocks - sd_ratio_retail_stocks * k), color = "red")

# Question 1.a) -----------------------------------------------------------

#' Title: Find Next Position
#'        Which finds the opening and closing indices of the first new position 
#'        on or after a given starting day.
#' @param ratio The daily price ratio for the two stocks (a numeric vector).
#' @param starting_from Index of the first day on which the next position could
#'                      be opened (default value of 1).
#' @param k The value of "k" for calculating the decision boundaries (m - k * s)
#'          and (m + k * s) (default value of 1).
#' @param m The estimated mean ratio m in the formula for the boundaries 
#'          (default value equal to the mean of "ratio").
#' @param s The standard deviation s of the price ratio, used in the formula for
#'          the boundaries (default value equal to the standard deviation of 
#'          "ratio").
#'
#' @return An integer vector of length two containing the indices of "ratio"
#'         where the next position should be opened and closed. In case no
#'         position is found, it returns a length zero integer vector.
#' @export 
#'
#' @examples 
find_next_position <- function(ratio, starting_from = 1, k,
                               m = mean(ratio), s = sd(ratio)) {
  if ((m - k * s) < min(data) & (m + k * s) > max(data)) {
    return(integer())
  } else {
    data <- as.data.frame(ratio)
    open <- which(data[starting_from:nrow(data), ] < (m - k * s) | 
                    data[starting_from:nrow(data), ] > (m + k * s))[1] +
      (starting_from - 1)
    open_num <- data[open, ]
    close <- if (open_num < m) {
      which(data[open:nrow(data), ] >= m)[1] + open - 1
    } else {
      which(data[open:nrow(data), ] <= m)[1] + open - 1
    }
    if (is.na(close)) {
      close <- nrow(data)
    }
    c(open = open, close = close)
  }
}

## Testing the function
pos_1 <- find_next_position(ratio = retail_stocks$ratio, 
                            k = 1.25)
pos_1
retail_stocks[pos_1, ]
pos_2 <- find_next_position(ratio = retail_stocks$ratio,
                            starting_from = pos_1[2], k = 1.25)
pos_2
retail_stocks[pos_2, ]
pos_3 <- find_next_position(ratio = retail_stocks$ratio,
                            starting_from = pos_2[2], k = 1.25)
pos_3
retail_stocks[pos_3, ]
pos_4 <- find_next_position(ratio = retail_stocks$ratio,
                            starting_from = pos_3[2], k = 1.25)
pos_4
pos_5 <- find_next_position(ratio = retail_stocks$ratio, k = 3.25)
pos_5

# Question 1.b) ------------------------------------------------------------
ratio <- retail_stocks$ratio
data <- as.data.frame(ratio)
starting_from <- 1
k <- 1.25
m <- mean_ratio_retail_stocks
s <- sd_ratio_retail_stocks
condition1 <- m - k * s
condition2 <- m + k * s
min_data <- min(data)
max_data <- max(data)
i <- 1

find_all_positions <- function (ratio, k = 1, m, s) {
  if ((m - k * s) < min(data) & (m + k * s) > max(data)) {
    return(list())
    } else {
      starting_from <- 1
      i <- 1
      positions <- vector("list", i)
      positions[[i]] <- find_next_position(ratio = retail_stocks$ratio, 
                                           starting_from, k = 1.25)
      names(positions) <- paste("position", 1, sep = "_")
      while (map(positions, 2)[[i]] < nrow(data)) {
        starting_from <- map(positions, 2)[[i]]
        i <- i + 1
        positions[[i]] <- find_next_position(ratio = retail_stocks$ratio, 
                                               starting_from, k = 1.25)
        names(positions) <- paste("position", 1:i, sep = "_")
      }
    }
}




  if ((m - k * s) < min(data) & (m + k * s) > max(data)) {
    return(list())
  } else {
    starting_from <- 1
    i <- 1
    positions <- vector("list", i)
    positions[[i]] <- find_next_position(ratio = retail_stocks$ratio, 
                                         starting_from, k = 1.25)
    names(positions) <- paste("position", 1, sep = "_")
    while (map(positions, 2)[[i]] < nrow(data)) {
      starting_from <- map(positions, 2)[[i]]
      i <- i + 1
      positions[[i]] <- find_next_position(ratio = retail_stocks$ratio, 
                                           starting_from, k = 1.25)
      names(positions) <- paste("position", 1:i, sep = "_")
    }
  }

if (length(all_positions) > 0) {
  names(all_positions) <- paste("position", 1:(i - 1), sep = "_")
}
all_positions
}

positions <- vector("list", 1)
positions[[1]] <- find_next_position(ratio = retail_stocks$ratio, 
                                         starting_from = 1, k = 1.25)
positions[[2]] <- find_next_position(ratio = retail_stocks$ratio, 
                                         starting_from = map(positions, 2)[[1]],
                                         k = 1.25)
positions[[3]] <- find_next_position(ratio = retail_stocks$ratio, 
                                     starting_from = map(positions, 2)[[2]],
                                     k = 1.25)
positions[[4]] <- find_next_position(ratio = retail_stocks$ratio, 
                                     starting_from = map(positions, 2)[[3]],
                                     k = 1.25)
positions[[5]] <- find_next_position(ratio = retail_stocks$ratio, 
                                     starting_from = map(positions, 2)[[4]],
                                     k = 1.25)

# Dirty work for Q1.a)
starting_from <- 1743
k <- 4
m <- mean_ratio_retail_stocks
s <- sd_ratio_retail_stocks
condition1 <- m - k * s
condition2 <- m + k * s
min_data <- min(data)
max_data <- max(data)

## Calcualtion
first_open <- which(retail_stocks$ratio < m - k * s | 
                retail_stocks$ratio > m + k * s)[1]
open <- retail_stocks$ratio[first_open]
data_new <- slice(retail_stocks, first_open:n())
first_close <- if (open < m) {
  which(data_new$ratio >= m)[1] + first_open - 1
} else {
  which(data_new$ratio <= m)[1] + first_open - 1
}
# goooz
data <- as.data.frame(ratio)
first_open <- which(data[starting_from:nrow(data), ] < m - k * s | 
                      data[starting_from:nrow(data), ] > m + k * s)[1] + 
  (starting_from - 1)
open <- ratio[first_open]
first_close <- if (open < m) {
  which(data[first_open:nrow(data), ] >= m)[1] + first_open - 1
} else {
  which(data[first_open:nrow(data), ] <= m)[1] + first_open - 1
}

data[first_open:nrow(data), ] >= m


ratio[1:nrow(ratio), ]
x <- nrow(retail_stocks)

## Output
pos_1 <- c(first_open, first_close)
retail_stocks[pos_1, ]


if ((m - k * s) < min(data) & (m + k * s) > max(data)) {
  result <- 1
} else {
  result <- 0
}

(-0.2707*-0.2707)+(-0.8809*-0.8809)+(0.2526*0.2526)+(-0.2948*-0.2948)
