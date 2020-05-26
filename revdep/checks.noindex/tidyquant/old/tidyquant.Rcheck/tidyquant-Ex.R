pkgname <- "tidyquant"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('tidyquant')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("av_api_key")
### * av_api_key

flush(stderr()); flush(stdout())

### Name: av_api_key
### Title: Set Alpha Vantage API Key
### Aliases: av_api_key

### ** Examples


## Not run: 
##D av_api_key(api_key = "foobar")
## End(Not run)




cleanEx()
nameEx("coord_x_date")
### * coord_x_date

flush(stderr()); flush(stdout())

### Name: coord_x_date
### Title: Zoom in on plot regions using date ranges or date-time ranges
### Aliases: coord_x_date coord_x_datetime

### ** Examples

# Load libraries
library(tidyquant)
library(dplyr)
library(ggplot2)

# coord_x_date
AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")
AAPL %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +                         # Plot stock price
    geom_ma(n = 50) +                     # Plot 50-day Moving Average
    geom_ma(n = 200, color = "red") +     # Plot 200-day Moving Average
    # Zoom in
    coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
                 ylim = c(75, 125))


# coord_x_datetime
time_index <- seq(from = as.POSIXct("2012-05-15 07:00"),
                  to   = as.POSIXct("2012-05-17 18:00"),
                  by   = "hour")
set.seed(1)
value <- rnorm(n = length(time_index))
hourly_data <- tibble(time.index = time_index,
                      value      = value)
hourly_data %>%
    ggplot(aes(x = time.index, y = value)) +
    geom_point() +
    coord_x_datetime(xlim = c("2012-05-15 07:00:00", "2012-05-15 16:00:00"))



cleanEx()
nameEx("excel_date_functions")
### * excel_date_functions

flush(stderr()); flush(stdout())

### Name: excel_date_functions
### Title: Excel Date and Time Functions
### Aliases: excel_date_functions AS_DATE AS_DATETIME DATE DATEVALUE YMD
###   MDY DMY YMD_HMS MDY_HMS DMY_HMS YMD_HM MDY_HM DMY_HM YMD_H MDY_H
###   DMY_H WEEKDAY WDAY DOW MONTHDAY MDAY DOM QUARTERDAY QDAY DAY WEEKNUM
###   WEEK WEEKNUM_ISO MONTH QUARTER YEAR YEAR_ISO DATE_TO_NUMERIC
###   DATE_TO_DECIMAL SECOND MINUTE HOUR NOW TODAY EOMONTH EDATE
###   NET_WORKDAYS COUNT_DAYS YEARFRAC DATE_SEQUENCE WORKDAY_SEQUENCE
###   HOLIDAY_SEQUENCE HOLIDAY_TABLE FLOOR_DATE FLOOR_DAY FLOOR_WEEK
###   FLOOR_MONTH FLOOR_QUARTER FLOOR_YEAR CEILING_DATE CEILING_DAY
###   CEILING_WEEK CEILING_MONTH CEILING_QUARTER CEILING_YEAR ROUND_DATE
###   ROUND_DAY ROUND_WEEK ROUND_MONTH ROUND_QUARTER ROUND_YEAR

### ** Examples

# Libraries
library(tidyquant)
library(tidyverse)
library(lubridate)

# --- Basic Usage ----

# Converters ---
AS_DATE("2011 Jan-01") # General
YMD("2011 Jan-01")     # Year, Month-Day Format
MDY("01-02-20")        # Month-Day, Year Format (January 2nd, 2020)
DMY("01-02-20")        # Day-Month, Year Format (February 1st, 2020)

# Extractors ---
WEEKDAY("2020-01-01")                                  # Labelled Day
WEEKDAY("2020-01-01", label = FALSE)                   # Numeric Day
WEEKDAY("2020-01-01", label = FALSE, week_start = 1)   # Start at 1 (Monday) vs 7 (Sunday)
MONTH("2020-01-01")
QUARTER("2020-01-01")
YEAR("2020-01-01")

# Current Date-Time ---
NOW()
TODAY()

# Date Math ---
EOMONTH("2020-01-01")
EOMONTH("2020-01-01", months = 1)
NET_WORKDAYS("2020-01-01", "2020-07-01") # 131 Skipping Weekends
NET_WORKDAYS("2020-01-01", "2020-07-01",
             holidays = HOLIDAY_SEQUENCE("2020-01-01", "2020-07-01",
                                         calendar = "NYSE")) # 126 Skipping 5 NYSE Holidays

# Date Sequences ---
DATE_SEQUENCE("2020-01-01", "2020-07-01")
WORKDAY_SEQUENCE("2020-01-01", "2020-07-01")
HOLIDAY_SEQUENCE("2020-01-01", "2020-07-01", calendar = "NYSE")
WORKDAY_SEQUENCE("2020-01-01", "2020-07-01",
                 holidays = HOLIDAY_SEQUENCE("2020-01-01", "2020-07-01",
                                             calendar = "NYSE"))

# Date Collapsers ---
FLOOR_DATE(AS_DATE("2020-01-15"), by = "month")
CEILING_DATE(AS_DATE("2020-01-15"), by = "month")
CEILING_DATE(AS_DATE("2020-01-15"), by = "month") - ddays(1) # EOMONTH using lubridate

# --- Usage with tidyverse ---

# Calculate returns by symbol/year/quarter
FANG %>%
    pivot_table(
        .rows       = c(symbol, ~ QUARTER(date)),
        .columns    = ~ YEAR(date),
        .values     = ~ PCT_CHANGE_FIRSTLAST(adjusted)
    )




cleanEx()
nameEx("excel_financial_math_functions")
### * excel_financial_math_functions

flush(stderr()); flush(stdout())

### Name: excel_financial_math_functions
### Title: Excel Financial Math Functions
### Aliases: excel_financial_math_functions NPV IRR FV PV PMT RATE

### ** Examples

# TODO




cleanEx()
nameEx("excel_if_functions")
### * excel_if_functions

flush(stderr()); flush(stdout())

### Name: excel_if_functions
### Title: Excel Summarising "If" Functions
### Aliases: excel_if_functions SUM_IFS COUNT_IFS AVERAGE_IFS MEDIAN_IFS
###   MIN_IFS MAX_IFS CREATE_IFS

### ** Examples

library(tidyverse)
library(tidyquant)
library(stringr)
library(lubridate)

# --- Basic Usage ---

SUM_IFS(x = 1:10, x > 5)

COUNT_IFS(x = letters, str_detect(x, "a|b|c"))

SUM_IFS(-10:10, x > 8 | x < -5)

# Create your own IFS function (Mind blowingly simple)!
Q75_IFS <- CREATE_IFS(.f = quantile, probs = 0.75, na.rm = TRUE)
Q75_IFS(1:10, x > 5)

# --- Usage with tidyverse ---

# Using multiple cases IFS cases to count the frequency of days with
# high trade volume in a given year
FANG %>%
    group_by(symbol) %>%
    summarise(
        high_volume_in_2015 = COUNT_IFS(volume,
                                        year(date) == 2015,
                                        volume > quantile(volume, 0.75))
    )

# Count negative returns by month
FANG %>%
    mutate(symbol = as_factor(symbol)) %>%
    group_by(symbol) %>%

    # Collapse from daily to FIRST value by month
    summarise_by_time(
        .date_var  = date,
        .by        = "month",
        adjusted   = FIRST(adjusted)
    ) %>%

    # Calculate monthly returns
    group_by(symbol) %>%
    mutate(
        returns = PCT_CHANGE(adjusted, fill_na = 0)
    ) %>%

    # Find returns less than zero and count the frequency
    summarise(
        negative_monthly_returns = COUNT_IFS(returns, returns < 0)
    )




cleanEx()
nameEx("excel_pivot_table")
### * excel_pivot_table

flush(stderr()); flush(stdout())

### Name: excel_pivot_table
### Title: Excel Pivot Table
### Aliases: excel_pivot_table pivot_table

### ** Examples

library(tidyquant)
library(tidyverse)

# PIVOT TABLE ----
# Calculate returns by year/quarter
FANG %>%
    pivot_table(
        .rows       = c(symbol, ~ QUARTER(date)),
        .columns    = ~ YEAR(date),
        .values     = ~ PCT_CHANGE_FIRSTLAST(adjusted)
    )




cleanEx()
nameEx("excel_ref_functions")
### * excel_ref_functions

flush(stderr()); flush(stdout())

### Name: excel_ref_functions
### Title: Excel Reference Functions
### Aliases: excel_ref_functions VLOOKUP

### ** Examples

library(tidyquant)
library(tidyverse)

lookup_table <- tibble(
    stock   = c("FB", "AMZN", "NFLX", "GOOG"),
    company = c("Facebook", "Amazon", "Netflix", "Google")
)

# --- Basic Usage ---

VLOOKUP("NFLX",
        .data = lookup_table,
        .lookup_column = stock,
        .return_column = company)

# --- Usage with tidyverse ---

# Add company names to the stock data
FANG %>%
    mutate(company = VLOOKUP(symbol, lookup_table, stock, company))




cleanEx()
nameEx("excel_stat_mutation_functions")
### * excel_stat_mutation_functions

flush(stderr()); flush(stdout())

### Name: excel_stat_mutation_functions
### Title: Excel Statistical Mutation Functions
### Aliases: excel_stat_mutation_functions ABS SQRT LOG EXP RETURN
###   PCT_CHANGE CHANGE LAG LEAD CUMULATIVE_SUM CUMULATIVE_PRODUCT
###   CUMULATIVE_MAX CUMULATIVE_MIN CUMULATIVE_MEAN CUMULATIVE_MEDIAN

### ** Examples

# Libraries
library(tidyquant)
library(tidyverse)
library(forcats)

# --- Basic Usage ----

CUMULATIVE_SUM(1:10)

PCT_CHANGE(c(21, 24, 22, 25), fill_na = 0)

# --- Usage with tidyverse ---

# Go from daily to monthly periodicity,
# then calculate returns and growth of $1 USD
FANG %>%
    mutate(symbol = as_factor(symbol)) %>%
    group_by(symbol) %>%

    # Summarization - Collapse from daily to FIRST value by month
    summarise_by_time(
        .date_var  = date,
        .time_unit = "month",
        adjusted   = FIRST(adjusted)
    ) %>%

    # Mutation - Calculate monthly returns and cumulative growth of $1 USD
    group_by(symbol) %>%
    mutate(
        returns = PCT_CHANGE(adjusted, fill_na = 0),
        growth  = CUMULATIVE_SUM(returns) + 1
    )




cleanEx()
nameEx("excel_stat_summary_functions")
### * excel_stat_summary_functions

flush(stderr()); flush(stdout())

### Name: excel_stat_summary_functions
### Title: Excel Statistical Summary Functions
### Aliases: excel_stat_summary_functions SUM AVERAGE MEDIAN MIN MAX COUNT
###   COUNT_UNIQUE STDEV VAR COR COV FIRST LAST NTH CHANGE_FIRSTLAST
###   PCT_CHANGE_FIRSTLAST

### ** Examples

# Libraries
library(tidyquant)
library(tidyverse)
library(forcats)

# --- Basic Usage ----

SUM(1:10)

PCT_CHANGE_FIRSTLAST(c(21, 24, 22, 25))

# --- Usage with tidyverse ---

# Go from daily to monthly periodicity,
# then calculate returns and growth of $1 USD
FANG %>%
    mutate(symbol = as_factor(symbol)) %>%
    group_by(symbol) %>%

    # Summarization - Collapse from daily to FIRST value by month
    summarise_by_time(
        .date_var  = date,
        .time_unit = "month",
        adjusted   = FIRST(adjusted)
    )




cleanEx()
nameEx("geom_bbands")
### * geom_bbands

flush(stderr()); flush(stdout())

### Name: geom_bbands
### Title: Plot Bollinger Bands using Moving Averages
### Aliases: geom_bbands geom_bbands_

### ** Examples

# Load libraries
library(tidyquant)
library(dplyr)
library(ggplot2)


AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")

# SMA
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_line() +           # Plot stock price
    geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = 50) +
    coord_x_date(xlim = c(as_date("2016-12-31") - dyears(1), as_date("2016-12-31")),
                 ylim = c(75, 125))


# EMA
AAPL %>%
   ggplot(aes(x = date, y = close)) +
   geom_line() +           # Plot stock price
   geom_bbands(aes(high = high, low = low, close = close),
                  ma_fun = EMA, wilder = TRUE, ratio = NULL, n = 50) +
   coord_x_date(xlim = c(as_date("2016-12-31") - dyears(1), as_date("2016-12-31")),
                ylim = c(75, 125))


# VWMA
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_line() +           # Plot stock price
    geom_bbands(aes(high = high, low = low, close = close, volume = volume),
                   ma_fun = VWMA, n = 50) +
    coord_x_date(xlim = c(as_date("2016-12-31") - dyears(1), as_date("2016-12-31")),
                ylim = c(75, 125))



cleanEx()
nameEx("geom_chart")
### * geom_chart

flush(stderr()); flush(stdout())

### Name: geom_chart
### Title: Plot Financial Charts in ggplot2
### Aliases: geom_chart geom_barchart geom_candlestick

### ** Examples

# Load libraries
library(tidyquant)
library(dplyr)
library(ggplot2)

AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")

# Bar Chart
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_barchart(aes(open = open, high = high, low = low, close = close)) +
    geom_ma(color = "darkgreen") +
    coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
                 ylim = c(75, 125))

# Candlestick Chart
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    geom_ma(color = "darkgreen") +
    coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
                 ylim = c(75, 125))



cleanEx()
nameEx("geom_ma")
### * geom_ma

flush(stderr()); flush(stdout())

### Name: geom_ma
### Title: Plot moving averages
### Aliases: geom_ma geom_ma_

### ** Examples

# Load libraries
library(tidyquant)
library(dplyr)
library(ggplot2)

AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")

# SMA
AAPL %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +                         # Plot stock price
    geom_ma(ma_fun = SMA, n = 50) +                 # Plot 50-day SMA
    geom_ma(ma_fun = SMA, n = 200, color = "red") + # Plot 200-day SMA
    coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
                 ylim = c(75, 125))                     # Zoom in

# EVWMA
AAPL %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +                                                   # Plot stock price
    geom_ma(aes(volume = volume), ma_fun = EVWMA, n = 50) +   # Plot 50-day EVWMA
    coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
                 ylim = c(75, 125))                                  # Zoom in



cleanEx()
nameEx("palette_tq")
### * palette_tq

flush(stderr()); flush(stdout())

### Name: palette_tq
### Title: tidyquant palettes for use with scales
### Aliases: palette_tq palette_light palette_dark palette_green

### ** Examples

library(scales)
scales::show_col(palette_light())




cleanEx()
nameEx("quandl_api_key")
### * quandl_api_key

flush(stderr()); flush(stdout())

### Name: quandl_api_key
### Title: Query or set Quandl API Key
### Aliases: quandl_api_key

### ** Examples


## Not run: 
##D quandl_api_key(api_key = "foobar")
## End(Not run)



cleanEx()
nameEx("quandl_search")
### * quandl_search

flush(stderr()); flush(stdout())

### Name: quandl_search
### Title: Search the Quandl database
### Aliases: quandl_search

### ** Examples


## Not run: 
##D quandl_search(query = "oil")
## End(Not run)



cleanEx()
nameEx("scale_manual")
### * scale_manual

flush(stderr()); flush(stdout())

### Name: scale_manual
### Title: tidyquant colors and fills for ggplot2.
### Aliases: scale_manual scale_color_tq scale_colour_tq scale_fill_tq

### ** Examples

# Load libraries
library(tidyquant)
library(dplyr)
library(ggplot2)

# Get stock prices
stocks <- c("AAPL", "FB", "NFLX") %>%
    tq_get(from = "2013-01-01",
           to   = "2017-01-01")

# Plot for stocks
g <- stocks %>%
    ggplot(aes(date, adjusted, color = symbol)) +
    geom_line() +
    labs(title = "Multi stock example",
         xlab = "Date",
         ylab = "Adjusted Close")

# Plot with tidyquant theme and colors
g +
    theme_tq() +
    scale_color_tq()





cleanEx()
nameEx("summarise_by_time")
### * summarise_by_time

flush(stderr()); flush(stdout())

### Name: summarise_by_time
### Title: Summarise each group by time
### Aliases: summarise_by_time summarize_by_time

### ** Examples

# Libraries
library(tidyquant)
library(dplyr)

# First adjusted price in each month
FANG %>%
    group_by(symbol) %>%
    summarise_by_time(
        .date_var  = date,
        .by         = "month",
        adjusted   = FIRST(adjusted)
    )

# Last adjused price in each month (day is last day of month with ceiling option)
FANG %>%
    group_by(symbol) %>%
    summarise_by_time(
        .date_var  = date,
        .by        = "month",
        adjusted   = LAST(adjusted),
        .type      = "ceiling")

# Total Volume each year (.by is set to "year" now)
FANG %>%
    group_by(symbol) %>%
    summarise_by_time(
        .date_var  = date,
        .by        = "year",
        adjusted   = SUM(volume))





cleanEx()
nameEx("theme_tq")
### * theme_tq

flush(stderr()); flush(stdout())

### Name: theme_tq
### Title: tidyquant themes for ggplot2.
### Aliases: theme_tq theme_tq_dark theme_tq_green

### ** Examples

# Load libraries
library(tidyquant)
library(dplyr)
library(ggplot2)

# Get stock prices
AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")

# Plot using ggplot with theme_tq
AAPL %>% ggplot(aes(x = date, y = close)) +
       geom_line() +
       geom_bbands(aes(high = high, low = low, close = close),
                   ma_fun = EMA,
                   wilder = TRUE,
                   ratio = NULL,
                   n = 50) +
       coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
                 ylim = c(75, 125)) +
       labs(title = "Apple BBands",
            x = "Date",
            y = "Price") +
       theme_tq()




cleanEx()
nameEx("tiingo_api_key")
### * tiingo_api_key

flush(stderr()); flush(stdout())

### Name: tiingo_api_key
### Title: Set Tiingo API Key
### Aliases: tiingo_api_key

### ** Examples


## Not run: 
##D tiingo_api_key(api_key = "foobar")
## End(Not run)




cleanEx()
nameEx("tq_get")
### * tq_get

flush(stderr()); flush(stdout())

### Name: tq_get
### Title: Get quantitative data in 'tibble' format
### Aliases: tq_get tq_get_options

### ** Examples

# Load libraries
library(tidyquant)
library(tidyverse)

# Get the list of `get` options
tq_get_options()

# Get stock prices for a stock from Yahoo
aapl_stock_prices <- tq_get("AAPL")

# Get stock prices for multiple stocks
mult_stocks <- tq_get(c("FB", "AMZN"),
                      get  = "stock.prices",
                      from = "2016-01-01",
                      to   = "2017-01-01")


## Not run: 
##D 
##D # --- Quandl ---
##D 
##D quandl_api_key('<your_api_key>')
##D 
##D # Energy data from EIA
##D tq_get("EIA/PET_MTTIMUS1_M", get = "quandl", from = "2010-01-01")
##D 
##D 
##D # --- Tiingo ---
##D 
##D tiingo_api_key('<your_api_key>')
##D 
##D # Tiingo Prices (Free alternative to Yahoo Finance!)
##D tq_get(c("AAPL", "GOOG"), get = "tiingo", from = "2010-01-01")
##D 
##D # Sub-daily prices from IEX ----
##D tq_get(c("AAPL", "GOOG"),
##D        get = "tiingo.iex",
##D        from   = "2020-01-01",
##D        to     = "2020-01-15",
##D        resample_frequency = "5min")
##D 
##D # Tiingo Bitcoin Prices ----
##D tq_get(c("btcusd", "btceur"),
##D        get    = "tiingo.crypto",
##D        from   = "2020-01-01",
##D        to     = "2020-01-15",
##D        resample_frequency = "5min")
##D 
##D 
##D # --- Alpha Vantage ---
##D 
##D av_api_key('<your_api_key>')
##D 
##D # Daily Time Series
##D tq_get("AAPL",
##D        get        = "alphavantager",
##D        av_fun     = "TIME_SERIES_DAILY_ADJUSTED",
##D        outputsize = "full")
##D 
##D # Intraday 15 Min Interval
##D tq_get("AAPL",
##D        get        = "alphavantage",
##D        av_fun     = "TIME_SERIES_INTRADAY",
##D        interval   = "15min",
##D        outputsize = "full")
##D 
##D # FX DAILY
##D tq_get("USD/EUR", get = "alphavantage", av_fun = "FX_DAILY", outputsize = "full")
##D 
##D # FX REAL-TIME QUOTE
##D tq_get("USD/EUR", get = "alphavantage", av_fun = "CURRENCY_EXCHANGE_RATE")
##D 
## End(Not run)



cleanEx()
nameEx("tq_index")
### * tq_index

flush(stderr()); flush(stdout())

### Name: tq_index
### Title: Get all stocks in a stock index or stock exchange in 'tibble'
###   format
### Aliases: tq_index tq_exchange tq_index_options tq_exchange_options

### ** Examples

# Load libraries
library(tidyquant)

# Get the list of stock index options
tq_index_options()

# Get all stock symbols in a stock index
## Not run: 
##D tq_index("DOW")
## End(Not run)

# Get the list of stock exchange options
tq_exchange_options()

# Get all stocks in a stock exchange
## Not run: 
##D tq_exchange("NYSE")
## End(Not run)




cleanEx()
nameEx("tq_mutate")
### * tq_mutate

flush(stderr()); flush(stdout())

### Name: tq_mutate
### Title: Mutates quantitative data
### Aliases: tq_mutate tq_mutate_ tq_mutate_xy tq_mutate_xy_
###   tq_mutate_fun_options tq_transmute tq_transmute_ tq_transmute_xy
###   tq_transmute_xy_ tq_transmute_fun_options

### ** Examples

# Load libraries
library(tidyquant)
library(dplyr)

##### Basic Functionality

fb_stock_prices  <- tq_get("FB",
                           get  = "stock.prices",
                           from = "2016-01-01",
                           to   = "2016-12-31")

# Example 1: Return logarithmic daily returns using periodReturn()
fb_stock_prices %>%
    tq_mutate(select = close, mutate_fun = periodReturn,
              period = "daily", type = "log")

# Example 2: Use tq_mutate_xy to use functions with two columns required
fb_stock_prices %>%
    tq_mutate_xy(x = close, y = volume, mutate_fun = EVWMA,
                 col_rename = "EVWMA")

# Example 3: Using tq_mutate to work with non-OHLC data
tq_get("DCOILWTICO", get = "economic.data") %>%
    tq_mutate(select = price, mutate_fun = lag.xts, k = 1, na.pad = TRUE)

# Example 4: Using tq_mutate to apply a rolling regression
fb_returns <- fb_stock_prices %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "fb.returns")
xlk_returns <- tq_get("XLK", from = "2016-01-01", to = "2016-12-31") %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "xlk.returns")
returns_combined <- left_join(fb_returns, xlk_returns, by = "date")
regr_fun <- function(data) {
    coef(lm(fb.returns ~ xlk.returns, data = as_tibble(data)))
}
returns_combined %>%
    tq_mutate(mutate_fun = rollapply,
              width      = 6,
              FUN        = regr_fun,
              by.column  = FALSE,
              col_rename = c("coef.0", "coef.1"))

# Example 5: Non-standard evaluation:
# Programming with tq_mutate_() and tq_mutate_xy_()
col_name <- "adjusted"
mutate <- c("MACD", "SMA")
tq_mutate_xy_(fb_stock_prices, x = col_name, mutate_fun = mutate[[1]])



cleanEx()
nameEx("tq_performance")
### * tq_performance

flush(stderr()); flush(stdout())

### Name: tq_performance
### Title: Computes a wide variety of summary performance metrics from
###   stock or portfolio returns
### Aliases: tq_performance tq_performance_ tq_performance_fun_options

### ** Examples

# Load libraries
library(tidyquant)
library(dplyr)

# Use FANG data set
data(FANG)

# Get returns for individual stock components grouped by symbol
Ra <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Ra")

# Get returns for SP500 as baseline
Rb <- "^GSPC" %>%
    tq_get(get  = "stock.prices",
           from = "2010-01-01",
           to   = "2015-12-31") %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Rb")

# Merge stock returns with baseline
RaRb <- left_join(Ra, Rb, by = c("date" = "date"))

##### Performance Metrics #####

# View options
tq_performance_fun_options()

# Get performance metrics
RaRb %>%
    tq_performance(Ra = Ra, performance_fun = SharpeRatio, p = 0.95)

RaRb %>%
    tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)




cleanEx()
nameEx("tq_portfolio")
### * tq_portfolio

flush(stderr()); flush(stdout())

### Name: tq_portfolio
### Title: Aggregates a group of returns by asset into portfolio returns
### Aliases: tq_portfolio tq_portfolio_ tq_repeat_df

### ** Examples

# Load libraries
library(tidyquant)
library(dplyr)

# Use FANG data set
data(FANG)

# Get returns for individual stock components
monthly_returns_stocks <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(adjusted, periodReturn, period = "monthly")

##### Portfolio Aggregation Methods #####

# Method 1: Use tq_portfolio with numeric vector of weights

weights <- c(0.50, 0.25, 0.25, 0)
tq_portfolio(data = monthly_returns_stocks,
             assets_col = symbol,
             returns_col = monthly.returns,
             weights = weights,
             col_rename = NULL,
             wealth.index = FALSE)

# Method 2: Use tq_portfolio with two column tibble and map weights

# Note that GOOG's weighting is zero in Method 1. In Method 2,
# GOOG is not added and same result is achieved.
weights_df <- tibble(symbol = c("FB", "AMZN", "NFLX"),
                     weights = c(0.50, 0.25, 0.25))
tq_portfolio(data = monthly_returns_stocks,
             assets_col = symbol,
             returns_col = monthly.returns,
             weights = weights_df,
             col_rename = NULL,
             wealth.index = FALSE)

# Method 3: Working with multiple portfolios

# 3A: Duplicate monthly_returns_stocks multiple times
mult_monthly_returns_stocks <- tq_repeat_df(monthly_returns_stocks, n = 4)

# 3B: Create weights table grouped by portfolio id
weights <- c(0.50, 0.25, 0.25, 0.00,
             0.00, 0.50, 0.25, 0.25,
             0.25, 0.00, 0.50, 0.25,
             0.25, 0.25, 0.00, 0.50)
stocks <- c("FB", "AMZN", "NFLX", "GOOG")
weights_table <- tibble(stocks) %>%
    tq_repeat_df(n = 4) %>%
    bind_cols(tibble(weights)) %>%
    group_by(portfolio)

# 3C: Scale to multiple portfolios
tq_portfolio(data = mult_monthly_returns_stocks,
             assets_col = symbol,
             returns_col = monthly.returns,
             weights = weights_table,
             col_rename = NULL,
             wealth.index = FALSE)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
