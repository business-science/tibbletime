pkgname <- "tsbox"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('tsbox')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("relevant_class")
### * relevant_class

flush(stderr()); flush(stdout())

### Name: relevant_class
### Title: Extract Relevant Class
### Aliases: relevant_class

### ** Examples

relevant_class(AirPassengers)
relevant_class(ts_df(AirPassengers))



cleanEx()
nameEx("ts_")
### * ts_

flush(stderr()); flush(stdout())

### Name: ts_
### Title: Constructing ts-Functions
### Aliases: ts_ load_suggested ts_ ts_ ts_apply

### ** Examples




cleanEx()
nameEx("ts_arithmetic")
### * ts_arithmetic

flush(stderr()); flush(stdout())

### Name: ts_arithmetic
### Title: Arithmetic Operators for ts-boxable objects
### Aliases: ts_arithmetic %ts+% ts_arithmetic %ts-% ts_arithmetic %ts*%
###   ts_arithmetic %ts/%

### ** Examples

head(fdeaths - mdeaths)
head(fdeaths %ts-% mdeaths)
head(ts_df(fdeaths) %ts-% mdeaths)



cleanEx()
nameEx("ts_bind")
### * ts_bind

flush(stderr()); flush(stdout())

### Name: ts_bind
### Title: Bind Time Series
### Aliases: ts_bind ts_bind ts_chain

### ** Examples

ts_bind(ts_span(mdeaths, end = "1975-12-01"), fdeaths)
ts_bind(mdeaths, c(2, 2))
ts_bind(mdeaths, 3, ts_bind(fdeaths, c(99, 2)))
ts_bind(ts_dt(mdeaths), AirPassengers)

# numeric vectors
ts_bind(12, AirPassengers, c(2, 3))

ts_chain(ts_span(mdeaths, end = "1975-12-01"), fdeaths)



cleanEx()
nameEx("ts_boxable")
### * ts_boxable

flush(stderr()); flush(stdout())

### Name: ts_boxable
### Title: Test if an Object is ts-Boxable
### Aliases: ts_boxable

### ** Examples

ts_boxable(AirPassengers)
ts_boxable(lm)



cleanEx()
nameEx("ts_c")
### * ts_c

flush(stderr()); flush(stdout())

### Name: ts_c
### Title: Collect Time Series
### Aliases: ts_c

### ** Examples

head(ts_c(ts_df(EuStockMarkets), AirPassengers))

# labeling
x <- ts_c(
  `International Airline Passengers` = ts_xts(AirPassengers),
  `Deaths from Lung Diseases` = ldeaths
)
head(x)




cleanEx()
nameEx("ts_examples")
### * ts_examples

flush(stderr()); flush(stdout())

### Name: ts_examples
### Title: Principal Components, Dygraphs, Forecasts, Seasonal Adjustment
### Aliases: ts_examples ts_prcomp ts_examples ts_dygraphs ts_examples
###   ts_forecast ts_examples ts_seas

### ** Examples




cleanEx()
nameEx("ts_frequency")
### * ts_frequency

flush(stderr()); flush(stdout())

### Name: ts_frequency
### Title: Change Frequency
### Aliases: ts_frequency

### ** Examples

ts_frequency(cbind(mdeaths, fdeaths), "year", "sum")
ts_frequency(cbind(mdeaths, fdeaths), "year", "sum")
ts_frequency(cbind(mdeaths, fdeaths), "quarter", "last")

ts_frequency(AirPassengers, 4, "sum")
ts_frequency(AirPassengers, 1, "sum")

# Note that incomplete years are omited by default
ts_frequency(EuStockMarkets, "year")
ts_frequency(EuStockMarkets, "year", na.rm = TRUE)




cleanEx()
nameEx("ts_ggplot")
### * ts_ggplot

flush(stderr()); flush(stdout())

### Name: ts_ggplot
### Title: Plot Time Series, Using ggplot2
### Aliases: ts_ggplot ts_ggplot theme_tsbox ts_ggplot colors_tsbox
###   ts_ggplot scale_color_tsbox ts_ggplot scale_fill_tsbox

### ** Examples


## Not run: 
##D library(dataseries)
##D dta <- ds(c("GDP.PBRTT.A.R", "CCI.CCIIR"), "xts")
##D ts_ggplot(ts_scale(ts_span(
##D     ts_c(
##D       `GDP Growth` = ts_pc(dta[, 'GDP.PBRTT.A.R']),
##D       `Consumer Sentiment Index` = dta[, 'CCI.CCIIR']
##D     ),
##D     start = "1995-01-01"
##D   ))) +
##D   ggplot2::ggtitle("GDP and Consumer Sentiment", subtitle = "normalized values") +
##D   theme_tsbox() +
##D   scale_color_tsbox()
## End(Not run)



cleanEx()
nameEx("ts_index")
### * ts_index

flush(stderr()); flush(stdout())

### Name: ts_index
### Title: Indices from Levels or Percentage Rates
### Aliases: ts_index ts_compound ts_index

### ** Examples

head(ts_compound(ts_pc(ts_c(fdeaths, mdeaths))))
head(ts_index(ts_df(ts_c(fdeaths, mdeaths)), "1974-02-01"))



cleanEx()
nameEx("ts_lag")
### * ts_lag

flush(stderr()); flush(stdout())

### Name: ts_lag
### Title: Lag or Lead of Time Series
### Aliases: ts_lag

### ** Examples

head(ts_lag(AirPassengers, "1 month"))
head(ts_lag(AirPassengers, "1 year"))
head(ts_lag(ts_df(AirPassengers), "2 day"))
# head(ts_lag(ts_df(AirPassengers), "2 min")) not yet working



cleanEx()
nameEx("ts_long")
### * ts_long

flush(stderr()); flush(stdout())

### Name: ts_long
### Title: Reshaping Multiple Time Series
### Aliases: ts_long ts_long ts_wide

### ** Examples

df.wide <- ts_wide(ts_df(ts_c(mdeaths, fdeaths)))
head(df.wide)
head(ts_long(df.wide))



cleanEx()
nameEx("ts_na_omit")
### * ts_na_omit

flush(stderr()); flush(stdout())

### Name: ts_na_omit
### Title: Omit NA values
### Aliases: ts_na_omit

### ** Examples

x <- AirPassengers
x[c(2, 4)] <- NA

# A ts object does only know explicit NAs
head(ts_na_omit(x))

# by default, NAs are implicit in data frames
head(ts_df(x))

# make NAs explicit
head(ts_regular(ts_df(x)))

# and implicit again
head(ts_na_omit(ts_regular(ts_df(x))))



cleanEx()
nameEx("ts_pc")
### * ts_pc

flush(stderr()); flush(stdout())

### Name: ts_pc
### Title: First Differences and Percentage Change Rates
### Aliases: ts_pc ts_pc ts_diff ts_pc ts_pcy ts_pc ts_diffy

### ** Examples

head(ts_diff(ts_c(fdeaths, mdeaths)))
head(ts_pc(ts_c(fdeaths, mdeaths)))
head(ts_pcy(ts_c(fdeaths, mdeaths)))
head(ts_diffy(ts_c(fdeaths, mdeaths)))



cleanEx()
nameEx("ts_pick")
### * ts_pick

flush(stderr()); flush(stdout())

### Name: ts_pick
### Title: Pick Series (Experimental)
### Aliases: ts_pick

### ** Examples

# Interactive use

# Programming use
to.be.picked.and.renamed <- c(`My Dax` = "DAX", `My Smi` = "SMI")
head(ts_pick(EuStockMarkets, to.be.picked.and.renamed))



cleanEx()
nameEx("ts_plot")
### * ts_plot

flush(stderr()); flush(stdout())

### Name: ts_plot
### Title: Plot Time Series
### Aliases: ts_plot

### ** Examples




cleanEx()
nameEx("ts_regular")
### * ts_regular

flush(stderr()); flush(stdout())

### Name: ts_regular
### Title: Enforce Regularity
### Aliases: ts_regular

### ** Examples

x0 <- AirPassengers
x0[c(10, 15)] <- NA
x <- ts_na_omit(ts_dts(x0))
ts_regular(x)

m <- mdeaths
m[c(10, 69)] <- NA
f <- fdeaths
f[c(1, 3, 15)] <- NA

ts_regular(ts_na_omit(ts_dts(ts_c(f, m))))



cleanEx()
nameEx("ts_scale")
### * ts_scale

flush(stderr()); flush(stdout())

### Name: ts_scale
### Title: Normalized Time Series
### Aliases: ts_scale

### ** Examples




cleanEx()
nameEx("ts_span")
### * ts_span

flush(stderr()); flush(stdout())

### Name: ts_span
### Title: Limit Time Span
### Aliases: ts_span ts_span ts_start ts_span ts_end

### ** Examples


# use 'anytime' shortcuts
ts_span(mdeaths, start = "1979")       # shortcut for 1979-01-01
ts_span(mdeaths, start = "1979-4")     # shortcut for 1979-04-01
ts_span(mdeaths, start = "197904")     # shortcut for 1979-04-01

# it's fine to use an to date outside of series span
ts_span(mdeaths, end = "2001-01-01")

# use strings to set start or end relative to each other

ts_span(mdeaths, start = "-7 month")   # last 7 months
ts_span(mdeaths, start = -7)           # last 7 periods
ts_span(mdeaths, start = -1)           # last single value
ts_span(mdeaths, end = "1e4 hours")    # first 10000 hours


# Limit span of 'discoveries' to the same span as 'AirPassengers'
ts_span(discoveries, template = AirPassengers)



cleanEx()
nameEx("ts_trend")
### * ts_trend

flush(stderr()); flush(stdout())

### Name: ts_trend
### Title: Loess Trend Estimation
### Aliases: ts_trend

### ** Examples




cleanEx()
nameEx("ts_ts")
### * ts_ts

flush(stderr()); flush(stdout())

### Name: ts_ts
### Title: Convert Everything to Everything
### Aliases: ts_ts ts_data.frame ts_ts ts_df ts_ts ts_data.table ts_ts
###   ts_dt ts_ts ts_tbl ts_ts ts_tibbletime ts_ts ts_timeSeries ts_ts
###   ts_ts ts_tsibble ts_ts ts_tslist ts_ts ts_xts ts_ts ts_zoo

### ** Examples


x.ts <- ts_c(mdeaths, fdeaths)
head(x.ts)
head(ts_df(x.ts))

suppressMessages(library(dplyr))
head(ts_tbl(x.ts))

suppressMessages(library(data.table))
head(ts_dt(x.ts))

suppressMessages(library(xts))
head(ts_xts(x.ts))

# heuristic time conversion
# 1 momth: approx. 1/12 year
head(ts_df(AirPassengers))

# exact time conversion
# 1 trading day: exactly 1/260 year
head(ts_df(EuStockMarkets))

# multiple id
multi.id.df <- rbind(
  within(ts_df(ts_c(fdeaths, mdeaths)), type <- "level"),
  within(ts_pc(ts_df(ts_c(fdeaths, mdeaths))), type <- "pc")
)
head(ts_ts(multi.id.df))
ts_plot(multi.id.df)




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
