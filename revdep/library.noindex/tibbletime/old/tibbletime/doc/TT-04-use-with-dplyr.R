## ---- warning=FALSE, message=FALSE---------------------------------------
library(tibbletime)
library(dplyr)
library(lubridate)

series <- create_series('2013' ~ '2017', 'daily', class = "Date") %>%
  mutate(var = rnorm(n()))

series

series %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(mean_var = mean(var))

## ------------------------------------------------------------------------
series %>%
  collapse_by("monthly") %>%
  group_by(date) %>%
  summarise(mean_var = mean(var))

## ------------------------------------------------------------------------
second_series <- create_series('2013' ~ '2015', '5 second')

second_series %>%
  mutate(var = rnorm(n())) %>%
  collapse_by("hourly") %>%
  group_by(date) %>%
  summarise(mean_var = mean(var))

## ------------------------------------------------------------------------
set.seed(123)

# Create price series of hourly movements for apple and facebook stock.
apple <- create_series('2014' ~ '2016', period = '1 hour') %>%
  mutate(price = 100 + cumsum(rnorm(n(), mean = 0, sd = .5)))

facebook <- create_series('2014' ~ '2016', period = '1 hour') %>%
  mutate(price = 150 + cumsum(rnorm(n(), mean = 0, sd = .5)))

# Bind them together and create a symbol column to group on
price_series <- bind_rows(list(apple = apple, facebook = facebook), .id = "symbol") %>%
  as_tbl_time(date) %>%
  group_by(symbol)

# Collapse to daily and transform to OHLC (Open, High, Low, Close), a 
# common financial transformation
price_series %>%
  collapse_by("daily") %>%
  group_by(date, add = TRUE) %>%
  summarise(
    open  = first(price),
    high  = max(price),
    low   = min(price),
    close = last(price)
  ) %>%
  slice(1:5)

