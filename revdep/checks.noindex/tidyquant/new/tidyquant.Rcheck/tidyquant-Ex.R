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

# coord_x_date
AAPL <- tq_get("AAPL")
AAPL %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +                         # Plot stock price
    geom_ma(n = 50) +                 # Plot 50-day Moving Average
    geom_ma(n = 200, color = "red") + # Plot 200-day Moving Average
    coord_x_date(xlim = c(today() - weeks(12), today()),
               ylim = c(100, 130))        # Zoom in


# coord_x_datetime
time_index <- seq(from = as.POSIXct("2012-05-15 07:00"),
                  to = as.POSIXct("2012-05-17 18:00"),
                  by = "hour")
set.seed(1)
value <- rnorm(n = length(time_index))
hourly_data <- tibble(time.index = time_index,
                      value = value)
hourly_data %>%
    ggplot(aes(x = time.index, y = value)) +
    geom_point() +
    coord_x_datetime(xlim = c("2012-05-15 07:00:00", "2012-05-15 16:00:00"))



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

AAPL <- tq_get("AAPL")

# SMA
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_line() +           # Plot stock price
    geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = 50) +
    coord_x_date(xlim = c(today() - years(1), today()), ylim = c(80, 130))


# EMA
AAPL %>%
   ggplot(aes(x = date, y = close)) +
   geom_line() +           # Plot stock price
   geom_bbands(aes(high = high, low = low, close = close),
                  ma_fun = EMA, wilder = TRUE, ratio = NULL, n = 50) +
   coord_x_date(xlim = c(today() - years(1), today()), ylim = c(80, 130))


# VWMA
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_line() +           # Plot stock price
    geom_bbands(aes(high = high, low = low, close = close, volume = volume),
                   ma_fun = VWMA, n = 50) +
    coord_x_date(xlim = c(today() - years(1), today()), ylim = c(80, 130))



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

AAPL <- tq_get("AAPL")

# Bar Chart
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_barchart(aes(open = open, high = high, low = low, close = close)) +
    geom_ma(color = "darkgreen") +
    coord_x_date(xlim = c(today() - weeks(6), today()),
                 ylim = c(100, 130))

# Candlestick Chart
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    geom_ma(color = "darkgreen") +
    coord_x_date(xlim = c(today() - weeks(6), today()),
                 ylim = c(100, 130))



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

AAPL <- tq_get("AAPL")

# SMA
AAPL %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +                         # Plot stock price
    geom_ma(ma_fun = SMA, n = 50) +                 # Plot 50-day SMA
    geom_ma(ma_fun = SMA, n = 200, color = "red") + # Plot 200-day SMA
    coord_x_date(xlim = c(today() - weeks(12), today()),
               ylim = c(100, 130))                     # Zoom in

# EVWMA
AAPL %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +                                                   # Plot stock price
    geom_ma(aes(volume = volume), ma_fun = EVWMA, n = 50) +   # Plot 50-day EVWMA
    coord_x_date(xlim = c(today() - weeks(12), today()),
               ylim = c(100, 130))                                  # Zoom in



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

# Get stock prices
stocks <- c("AAPL", "FB", "NFLX") %>%
    tq_get(from = "2013-01-01",
           to   = "2017-01-01")

# Plot for stocks
a <- stocks %>%
    ggplot(aes(date, adjusted, color = symbol)) +
    geom_line() +
    labs(title = "Multi stock example",
         xlab = "Date",
         ylab = "Adjusted Close")

# Plot with tidyquant theme and colors
a +
    theme_tq() +
    scale_color_tq()





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

# Get stock prices
AAPL <- tq_get("AAPL")

# Plot using ggplot with theme_tq
AAPL %>% ggplot(aes(x = date, y = close)) +
       geom_line() +
       geom_bbands(aes(high = high, low = low, close = close),
                   ma_fun = EMA,
                   wilder = TRUE,
                   ratio = NULL,
                   n = 50) +
       coord_x_date(xlim = c(today() - years(1), today()),
                    ylim = c(80, 130)) +
       labs(title = "Apple BBands",
            x = "Date",
            y = "Price") +
       theme_tq()




cleanEx()
nameEx("tq_get")
### * tq_get

flush(stderr()); flush(stdout())

### Name: tq_get
### Title: Get quantitative data in 'tibble' format
### Aliases: tq_get tq_get_options tq_get_stock_index_options

### ** Examples

# Load libraries
library(tidyquant)

# Get the list of `get` options
tq_get_options()

# Get stock prices for a stock from Yahoo
aapl_stock_prices <- tq_get("AAPL")

# Get stock prices for multiple stocks
mult_stocks <- tq_get(c("FB", "AMZN"),
                      get  = "stock.prices",
                      from = "2016-01-01",
                      to   = "2017-01-01")

# Multiple gets
mult_gets <- tq_get("AAPL",
                    get = c("stock.prices", "dividends"),
                    from = "2016-01-01",
                    to   = "2017-01-01")



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
    coef(lm(fb.returns ~ xlk.returns, data = as_data_frame(data)))
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



cleanEx()
nameEx("tq_stock_list")
### * tq_stock_list

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
