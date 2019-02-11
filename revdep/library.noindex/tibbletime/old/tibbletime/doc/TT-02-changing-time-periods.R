## ---- message=FALSE, warning=FALSE---------------------------------------
library(tibbletime)
library(dplyr)

# Facebook stock prices.
data(FB)

# Convert FB to tbl_time
FB <- as_tbl_time(FB, index = date)

# FANG stock prices
data(FANG)

# Convert FANG to tbl_time and group
FANG <- as_tbl_time(FANG, index = date) %>%
  group_by(symbol)


## ------------------------------------------------------------------------
as_period(FB, '1 month')

# Additionally, the following are equivalent
# as_period(FB, 'monthly')
# as_period(FB, 'm')
# as_period(FB, '1 m')

## ------------------------------------------------------------------------
as_period(FB, '2 m')

## ------------------------------------------------------------------------
as_period(FB, '25 d')

## ------------------------------------------------------------------------
# Without start_date
as_period(FB, '2 d')

## ------------------------------------------------------------------------
# With start_date
as_period(FB, '2 d', start_date = "2013-01-01")

## ------------------------------------------------------------------------
as_period(FB, 'y', side = "end")

## ------------------------------------------------------------------------
FANG %>%
  as_period('2 y')

