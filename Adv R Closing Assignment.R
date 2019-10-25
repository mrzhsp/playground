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
retail_stocks <- stocks %>%
  filter(date >= start_date, date <= end_date) %>%
  select(date, TGT, WMT) %>%
  mutate(ratio = TGT / WMT)
mean_ratio_retail_stocks <- mean(retail_stocks$ratio)
sd_ratio_retail_stocks <- sd(retail_stocks$ratio)

k <- 1.25
ggplot(retail_stocks, aes(x = date, y = ratio)) +
  geom_line() +
  geom_hline(aes(yintercept = mean_ratio_retail_stocks), color = "blue") +
  geom_hline(aes(yintercept = mean_ratio_retail_stocks + 
                   sd_ratio_retail_stocks * k), color = "red") +
  geom_hline(aes(yintercept = mean_ratio_retail_stocks - 
                   sd_ratio_retail_stocks * k), color = "red")

# Question 1.a) -----------------------------------------------------------

#' Title: Find Next Position
#'        Which finds the opening and closing indices of the first new position
#'        on or after a given starting day.
#' @param ratio The daily price ratio for the two stocks (a numeric vector).
#' @param starting_from Index of the first day on which the next position could
#'                      be opened (default value = 1).
#' @param k The value of "k" for calculating the decision boundaries (m - k * s)
#'          and (m + k * s) (default value = 1).
#' @param m The estimated mean ratio m in the formula for the boundaries
#'          (default value = the mean of "ratio").
#' @param s The standard deviation s of the price ratio, used in the formula for
#'          the boundaries (default value = the standard deviation of "ratio").
#' @return An integer vector of length two containing the indices of "ratio"
#'         where the next position should be opened and closed. In case no
#'         position is found, it returns a length zero integer vector.
#' @export
#'
#' @examples
find_next_position <- function(ratio, starting_from = 1, k,
                               m = mean(ratio), s = sd(ratio)) {
  #' Turn the input into a dataframe and make sure that starting_from is
  #'   stored as a number when I want the next positions in the next iterations.
  data <- as.data.frame(ratio)
  starting_from <- as.numeric(starting_from)
  #' A condition to check whether the "k" is too big or not. If it is, then an
  #'   empty integer vector.
  if ((m - k * s) < min(data) & (m + k * s) > max(data)) {
    return(integer())
  } else {
    #' The opening positions will be called and stored based on the criteria.
    open <- which(data[starting_from:nrow(data), ] < (m - k * s) |
      data[starting_from:nrow(data), ] > (m + k * s))[1] +
      (starting_from - 1)
    #' The closing posiiton will be called based on the criterai that the open
    #'   was below or above the mean.
    close <- if (data[open, ] < m) {
      which(data[open:nrow(data), ] >= m)[1] + open - 1
    } else {
      which(data[open:nrow(data), ] <= m)[1] + open - 1
    }
    #' This is the condition for checking whether the function has reached the
    #'   end of the dataframe.
    if (is.na(close)) {
      close <- nrow(data)
    }
    c("open" = open, "close" = close)
  }
}

## Testing the function
pos_1 <- find_next_position(
  ratio = retail_stocks$ratio,
  k = 1.25
)
pos_1
retail_stocks[pos_1, ]
pos_2 <- find_next_position(
  ratio = retail_stocks$ratio,
  starting_from = pos_1[2], k = 1.25
)
pos_2
retail_stocks[pos_2, ]
pos_3 <- find_next_position(
  ratio = retail_stocks$ratio,
  starting_from = pos_2[2], k = 1.25
)
pos_3
retail_stocks[pos_3, ]
pos_4 <- find_next_position(
  ratio = retail_stocks$ratio,
  starting_from = pos_3[2], k = 1.25
)
pos_4
retail_stocks[pos_4, ]
pos_5 <- find_next_position(ratio = retail_stocks$ratio, k = 3.25)
pos_5

# Question 1.b) ------------------------------------------------------------

#' Title Find All Positons
#'       Which finds all the opening and closing indices.
#'Which finds the opening and closing indices of the first new position
#'        on or after a given starting day.
#' @param ratio The daily price ratio for the two stocks (a numeric vector).
#' @param k The value of "k" for calculating the decision boundaries (m - k * s)
#'          and (m + k * s) (default value = 1).
#' @param m The estimated mean ratio m in the formula for the boundaries
#'          (default value = the mean of "ratio").
#' @param s The standard deviation s of the price ratio, used in the formula for
#'          the boundaries (default value = the standard deviation of "ratio").
#' @return A list of the indices of all positions.
#' @export
#'
#' @examples
find_all_positions <- function(ratio, k, m = mean(ratio),
                               s = sd(ratio)) {
  #' Turn the input into a dataframe.
  data <- as.data.frame(ratio)
  #' A condition to check whether there are positions. If there is none, then it
  #'   returns a length-zero list.
  if ((m - k * s) < min(data) & (m + k * s) > max(data)) {
    return(list())
  } else {
    #' The initial values needed for the loop are assigned.
    starting_from <- 1
    i <- 1
    positions <- vector("list", i)
    #' The function from Q1.a is used.
    positions[[i]] <- find_next_position(ratio, starting_from, k)
    #' This is for naming the elements of the list.
    names(positions) <- paste("positions", 1, sep = "_")
    while (map(positions, 2)[[i]] < nrow(data)) {
      #' The loop starts from the first closing position.
      starting_from <- map(positions, 2)[[i]]
      i <- i + 1
      positions[[i]] <- find_next_position(ratio, starting_from, k)
      names(positions) <- paste("positions", 1:i, sep = "_")
    }
  }
  return(positions)
}

## Testing the function
positions <- find_all_positions(ratio = retail_stocks$ratio, k = 1.25)

# Question 2.a) -------------------------------------------------------------------

#' Title Profit for a Position
#'
#' @param position A given position including opening and closing indices.
#' @param stock_a The daily price for the first stock.
#' @param stock_b The daily price for the second stock.
#' @param m The average of the ratio of the two stocks.
#' @param p The proportion commission for a transaction which %p of a 
#'          transaction.
#'
#' @return A numeric vector including the profit for stock_a, profit for stock_b
#'         cost for the whole transaction, and the total profit.
#' @export
#'
#' @examples
position_profit <- function(position, stock_a, stock_b,
                            m = mean(stock_a / stock_b), p = 0.01) {
  #' Turn the input into a dataframe.
  data <- as.data.frame(stock_a / stock_b)
  #' Compute the mean of the ratio of the two stocks
  m <- mean(stock_a / stock_b)
  #' Compute the unit of stock_a to be bought or sold. 
  open_unit <- as.numeric(1 / stock_a[position][[1]])
  #' Compute the unit of stock_b to be bought or sold.
  close_unit <- as.numeric(1 / stock_b[position][[1]])
  #' Compute the revenue from buying or selling stock_a.
  revenue_a <- open_unit * as.numeric(stock_a[position][[2]])
  #' Compute the revenue from buying or selling stock_b.
  revenue_b <- close_unit * as.numeric(stock_b[position][[2]])
  #' This is the condition check that determines the buying or selling of stock.
  #'   It is based on the opening position to be either below or above mean. if 
  #'   it is below mean, then it computes the profit based on "buying stock_a
  #'   and selling stock_b". If it is above mean, then it computes the profit
  #'   based on "selling stock_a and buying stock_b".
  if ((stock_a / stock_b)[position][[1]] < m) {
    profit_stock_a <- round((revenue_a - 1), 5)
    profit_stock_b <- round((1- revenue_b), 5)
  } else {
    profit_stock_a <- -round((revenue_a - 1), 5)
    profit_stock_b <- -round((1 - revenue_b), 5)
  }
  #' Compute the cost and profit.
  cost <- -round((2 * p + (revenue_a + revenue_b) * p), 5)
  profit <- round((profit_stock_a + profit_stock_b + cost), 5)
  #' The output of the function in terms of a numeric vector.
  output <- c(profit_stock_a, profit_stock_b, cost, profit)
  output
}

## Testing the function
position_profit(pos_1, 
                stock_a = retail_stocks$TGT,
                stock_b = retail_stocks$WMT,
                p = 0.01)
position_profit(pos_2, 
                stock_a = retail_stocks$TGT,
                stock_b = retail_stocks$WMT,
                p = 0.01)
position_profit(pos_3, 
                stock_a = retail_stocks$TGT,
                stock_b = retail_stocks$WMT,
                p = 0.01)
position_profit(pos_4, 
                stock_a = retail_stocks$TGT,
                stock_b = retail_stocks$WMT,
                p = 0.01)
position_profit(pos_5, 
                stock_a = retail_stocks$TGT,
                stock_b = retail_stocks$WMT,
                p = 0.01)

# Question 2.b) -----------------------------------------------------------

strategy_profit <- function (k, stock_a, stock_b,
                             m = mean(stock_a / stock_b),
                             sd = sd(stock_a / stock_b),
                             p = 0.01) {
  
  
  positions <- find_all_positions(ratio = stock_a / stock_b, k)
  disaggregate<- data.frame(matrix(ncol = 8, nrow = length(positions)))
  colnames(disaggregate) <- c("position",
                              "open",
                              "close",
                              "duration",
                              "stock_a",
                              "stock_b",
                              "cost",
                              "profit")
  for (i in seq_along(positions)) {
    profit_data <- position_profit(positions[[i]], 
                                   stock_a = retail_stocks$TGT,
                                   stock_b = retail_stocks$WMT,
                                   p = 0.01)
    disaggregate[i, ] <- c(names(positions)[i],
                           as.numeric(map(positions, 1)[[i]]),
                           as.numeric(map(positions, 2)[[i]]),
                           as.numeric(map(positions, 2)[[i]]) - 
                             as.numeric(map(positions, 1)[[i]]),
                           profit_data[[1]],
                           profit_data[[2]],
                           profit_data[[3]],
                           profit_data[[4]])
  }
}

positions <- find_all_positions(ratio = stock_a / stock_b, k = 1.25)
disaggregate<- data.frame(matrix(ncol = 8, nrow = length(positions)))
for (i in seq_along(positions)) {
  profit_data <- position_profit(positions[[i]], 
                                 stock_a = retail_stocks$TGT,
                                 stock_b = retail_stocks$WMT,
                                 p = 0.01)
  disaggregate[i, ] <- c(names(positions)[i],
                             as.numeric(map(positions, 1)[[i]]),
                             as.numeric(map(positions, 2)[[i]]),
                             as.numeric(map(positions, 2)[[i]]) - 
                               as.numeric(map(positions, 1)[[i]]),
                             profit_data[[1]],
                             profit_data[[2]],
                             profit_data[[3]],
                             profit_data[[4]])
}


# Atabak:
positions <- find_all_positions(ratio = stock_a / stock_b, k = 1.25)
disaggregate<- data.frame(matrix(ncol = 8, nrow = length(positions)))
colnames(disaggregate)<-c("position","open","close","duration" ,"stock_a",
                          "stock_b","cost","profit")
for (i in seq_along(positions)){
  profitInfo<-position_profit(positions[[i]],stock_a,stock_b,m,p)
  disaggregate[i,] = c(names(positions[i]),
                       positions[[i]][[1]],
                       positions[[i]][[2]],
                       positions[[i]][[2]] - positions[[i]][[1]],
                       round(profitInfo[[1]],digits=5),
                       round(profitInfo[[2]],digits=5),
                       round(profitInfo[[3]],digits=5),
                       round(profitInfo[[4]],digits=4))
}
















disaggregate <- matrix(nrow = length(positions), ncol = 8)
colnames(disaggregate) <- c("positon",
                            "open",
                            "close",
                            "duration",
                            "stock_a",
                            "stock_b",
                            "cost",
                            "profit")




output1 <- vector("list", length(positions))
for (i in seq_along(positions)) {
  output1[i] <- position_profit(positions[[i]], 
                  stock_a = retail_stocks$TGT,
                  stock_b = retail_stocks$WMT,
                  p = 0.01)
}
output1










# Dirty Work for Q1.a) ----------------------------------------------------
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

# Dirty Work for Q1.b) ----------------------------------------------------
data <- as.data.frame(ratio)
if ((m - k * s) < min(data) & (m + k * s) > max(data)) {
  return(list())
} else {
  starting_from <- 1
  i <- 1
  positions <- vector("list", i)
  positions[[i]] <- find_next_position(
    ratio = retail_stocks$ratio,
    starting_from, k = 1.25
  )
  names(positions) <- paste("position", 1, sep = "_")
  while (map(positions, 2)[[i]] < nrow(data)) {
    starting_from <- map(positions, 2)[[i]]
    i <- i + 1
    positions[[i]] <- find_next_position(
      ratio = retail_stocks$ratio,
      starting_from, k = 1.25
    )
    names(positions) <- paste("position", 1:i, sep = "_")
  }
}

positions <- vector("list", 1)
positions[[1]] <- find_next_position(
  ratio = retail_stocks$ratio,
  starting_from = 1, k = 1.25
)
positions[[2]] <- find_next_position(
  ratio = retail_stocks$ratio,
  starting_from = map(positions, 2)[[1]],
  k = 1.25
)
positions[[3]] <- find_next_position(
  ratio = retail_stocks$ratio,
  starting_from = map(positions, 2)[[2]],
  k = 1.25
)
positions[[4]] <- find_next_position(
  ratio = retail_stocks$ratio,
  starting_from = map(positions, 2)[[3]],
  k = 1.25
)
positions[[5]] <- find_next_position(
  ratio = retail_stocks$ratio,
  starting_from = map(positions, 2)[[4]],
  k = 1.25
)

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

# Dirty Work for Q2.a) ----------------------------------------------------
data <- as.data.frame(retail_stocks$ratio)
x <- c(map(positions, 1)[[1]], map(positions, 2)[[1]])
b <- retail_stocks[x, ]
m <- mean(retail_stocks$ratio)
p <- 0.01
open_unit <- as.numeric(1 / b[1, 2])
close_unit <- as.numeric(1 / b[1, 3])
revenue_a <- open_unit * as.numeric(b[2, 2])
revenue_b <- close_unit * as.numeric(b[2, 3])
if (data[x[1], ] < m) {
  profit_stock_a <- revenue_a - 1
  profit_stock_b <- 1 - revenue_b
} else {
  profit_stock_a <- -(revenue_a - 1)
  profit_stock_b <- -(1 - revenue_b)
}
cost <- -(2 * p + (revenue_a + revenue_b) * p)
profit <- profit_stock_a + profit_stock_b + cost
output <- c(profit_stock_a, profit_stock_b, cost, profit)
output


# Dirty Work for Q2.b) ----------------------------------------------------


