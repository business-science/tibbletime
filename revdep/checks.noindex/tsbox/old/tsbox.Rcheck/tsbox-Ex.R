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
### Aliases: ts_ load_suggested ts_apply

### ** Examples




cleanEx()
nameEx("ts_arithmetic")
### * ts_arithmetic

flush(stderr()); flush(stdout())

### Name: ts_arithmetic
### Title: Arithmetic Operators for ts-boxable objects
### Aliases: ts_arithmetic %ts+% %ts-% %ts*% %ts/%

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
### Aliases: ts_bind ts_chain

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
nameEx("ts_default")
### * ts_default

flush(stderr()); flush(stdout())

### Name: ts_default
### Title: Default Column Names
### Aliases: ts_default

### ** Examples


df <- ts_df(ts_c(mdeaths, fdeaths))
# non-default colnames
colnames(df) <- c("id", "date", "count")
# switch back to default colnames
head(ts_default(df))



cleanEx()
nameEx("ts_examples")
### * ts_examples

flush(stderr()); flush(stdout())

### Name: ts_examples
### Title: Principal Components, Dygraphs, Forecasts, Seasonal Adjustment
### Aliases: ts_examples ts_prcomp ts_dygraphs ts_forecast ts_seas

### ** Examples




cleanEx()
nameEx("ts_frequency")
### * ts_frequency

flush(stderr()); flush(stdout())

### Name: ts_frequency
### Title: Change Frequency
### Aliases: ts_frequency

### ** Examples




cleanEx()
nameEx("ts_ggplot")
### * ts_ggplot

flush(stderr()); flush(stdout())

### Name: ts_ggplot
### Title: Plot Time Series, Using ggplot2
### Aliases: ts_ggplot theme_tsbox colors_tsbox scale_color_tsbox
###   scale_fill_tsbox

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
### Aliases: ts_index ts_compound

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

head(ts_lag(fdeaths, "1 month"))
head(ts_lag(fdeaths, "1 year"))
head(ts_lag(ts_df(fdeaths), "2 day"))
head(ts_lag(ts_df(fdeaths), "2 min"))
head(ts_lag(ts_df(fdeaths), "-1 day"))



cleanEx()
nameEx("ts_long")
### * ts_long

flush(stderr()); flush(stdout())

### Name: ts_long
### Title: Reshaping Multiple Time Series
### Aliases: ts_long ts_wide

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
### Aliases: ts_pc ts_diff ts_pca ts_pcy ts_diffy

### ** Examples

tail(ts_diff(ts_c(fdeaths, mdeaths)))
tail(ts_pc(ts_c(fdeaths, mdeaths)))
tail(ts_pca(ts_c(fdeaths, mdeaths)))
tail(ts_pcy(ts_c(fdeaths, mdeaths)))
tail(ts_diffy(ts_c(fdeaths, mdeaths)))



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
ts_regular(x, fill = 0)

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
### Aliases: ts_span

### ** Examples



# Limit span of 'discoveries' to the same span as 'AirPassengers'
ts_span(discoveries, template = AirPassengers)
ts_span(mdeaths, end = "19801201", extend = TRUE)



cleanEx()
nameEx("ts_summary")
### * ts_summary

flush(stderr()); flush(stdout())

### Name: ts_summary
### Title: Time Series Properties
### Aliases: ts_summary

### ** Examples

ts_summary(ts_c(mdeaths, austres))
ts_summary(ts_c(mdeaths, austres), spark = TRUE)
# Extracting specific properties
ts_summary(AirPassengers)$start
ts_summary(AirPassengers)$freq
ts_summary(AirPassengers)$obs



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
### Aliases: ts_ts ts_data.frame ts_df ts_data.table ts_dt ts_tbl
###   ts_tibbletime ts_timeSeries ts_tis ts_irts ts_tsibble ts_tslist
###   ts_xts ts_zoo

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
# 1 month: approx. 1/12 year
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
