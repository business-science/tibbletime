pkgname <- "anomalize"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('anomalize')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("anomalize")
### * anomalize

flush(stderr()); flush(stdout())

### Name: anomalize
### Title: Detect anomalies using the tidyverse
### Aliases: anomalize

### ** Examples


library(dplyr)

# Needed to pass CRAN check / This is loaded by default
set_time_scale_template(time_scale_template())

data(tidyverse_cran_downloads)

tidyverse_cran_downloads %>%
    time_decompose(count, method = "stl") %>%
    anomalize(remainder, method = "iqr")





cleanEx()
nameEx("anomalize_methods")
### * anomalize_methods

flush(stderr()); flush(stdout())

### Name: anomalize_methods
### Title: Methods that power anomalize()
### Aliases: anomalize_methods iqr gesd

### ** Examples


set.seed(100)
x <- rnorm(100)
idx_outliers <- sample(100, size = 5)
x[idx_outliers] <- x[idx_outliers] + 10

iqr(x, alpha = 0.05, max_anoms = 0.2)
iqr(x, alpha = 0.05, max_anoms = 0.2, verbose = TRUE)

gesd(x, alpha = 0.05, max_anoms = 0.2)
gesd(x, alpha = 0.05, max_anoms = 0.2, verbose = TRUE)





cleanEx()
nameEx("clean_anomalies")
### * clean_anomalies

flush(stderr()); flush(stdout())

### Name: clean_anomalies
### Title: Clean anomalies from anomalized data
### Aliases: clean_anomalies

### ** Examples


library(dplyr)

# Needed to pass CRAN check / This is loaded by default
set_time_scale_template(time_scale_template())

data(tidyverse_cran_downloads)

tidyverse_cran_downloads %>%
    time_decompose(count, method = "stl") %>%
    anomalize(remainder, method = "iqr") %>%
    clean_anomalies()





cleanEx()
nameEx("decompose_methods")
### * decompose_methods

flush(stderr()); flush(stdout())

### Name: decompose_methods
### Title: Methods that power time_decompose()
### Aliases: decompose_methods decompose_twitter decompose_stl

### ** Examples


library(dplyr)

tidyverse_cran_downloads %>%
    ungroup() %>%
    filter(package == "tidyquant") %>%
    decompose_stl(count)





cleanEx()
nameEx("plot_anomalies")
### * plot_anomalies

flush(stderr()); flush(stdout())

### Name: plot_anomalies
### Title: Visualize the anomalies in one or multiple time series
### Aliases: plot_anomalies

### ** Examples


library(dplyr)
library(ggplot2)

data(tidyverse_cran_downloads)

#### SINGLE TIME SERIES ####
tidyverse_cran_downloads %>%
    filter(package == "tidyquant") %>%
    ungroup() %>%
    time_decompose(count, method = "stl") %>%
    anomalize(remainder, method = "iqr") %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE)


#### MULTIPLE TIME SERIES ####
tidyverse_cran_downloads %>%
    time_decompose(count, method = "stl") %>%
    anomalize(remainder, method = "iqr") %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE, ncol = 3)




cleanEx()
nameEx("plot_anomaly_decomposition")
### * plot_anomaly_decomposition

flush(stderr()); flush(stdout())

### Name: plot_anomaly_decomposition
### Title: Visualize the time series decomposition with anomalies shown
### Aliases: plot_anomaly_decomposition

### ** Examples


library(dplyr)
library(ggplot2)

data(tidyverse_cran_downloads)

tidyverse_cran_downloads %>%
    filter(package == "tidyquant") %>%
    ungroup() %>%
    time_decompose(count, method = "stl") %>%
    anomalize(remainder, method = "iqr") %>%
    plot_anomaly_decomposition()




cleanEx()
nameEx("prep_tbl_time")
### * prep_tbl_time

flush(stderr()); flush(stdout())

### Name: prep_tbl_time
### Title: Automatically create tibbletime objects from tibbles
### Aliases: prep_tbl_time

### ** Examples


library(dplyr)
library(tibbletime)

data_tbl <- tibble(
    date  = seq.Date(from = as.Date("2018-01-01"), by = "day", length.out = 10),
    value = rnorm(10)
    )

prep_tbl_time(data_tbl)




cleanEx()
nameEx("time_apply")
### * time_apply

flush(stderr()); flush(stdout())

### Name: time_apply
### Title: Apply a function to a time series by period
### Aliases: time_apply

### ** Examples


library(dplyr)

data(tidyverse_cran_downloads)

# Basic Usage
tidyverse_cran_downloads %>%
    time_apply(count, period = "1 week", .fun = mean, na.rm = TRUE)




cleanEx()
nameEx("time_decompose")
### * time_decompose

flush(stderr()); flush(stdout())

### Name: time_decompose
### Title: Decompose a time series in preparation for anomaly detection
### Aliases: time_decompose

### ** Examples


library(dplyr)

data(tidyverse_cran_downloads)

# Basic Usage
tidyverse_cran_downloads %>%
    time_decompose(count, method = "stl")

# twitter
tidyverse_cran_downloads %>%
    time_decompose(count,
                   method       = "twitter",
                   frequency    = "1 week",
                   trend        = "2 months",
                   merge        = TRUE,
                   message      = FALSE)




cleanEx()
nameEx("time_frequency")
### * time_frequency

flush(stderr()); flush(stdout())

### Name: time_frequency
### Title: Generate a time series frequency from a periodicity
### Aliases: time_frequency time_trend

### ** Examples


library(dplyr)

data(tidyverse_cran_downloads)

#### FREQUENCY DETECTION ####

# period = "auto"
tidyverse_cran_downloads %>%
    filter(package == "tidyquant") %>%
    ungroup() %>%
    time_frequency(period = "auto")

time_scale_template()

# period = "1 month"
tidyverse_cran_downloads %>%
    filter(package == "tidyquant") %>%
    ungroup() %>%
    time_frequency(period = "1 month")

#### TREND DETECTION ####

tidyverse_cran_downloads %>%
    filter(package == "tidyquant") %>%
    ungroup() %>%
    time_trend(period = "auto")



cleanEx()
nameEx("time_recompose")
### * time_recompose

flush(stderr()); flush(stdout())

### Name: time_recompose
### Title: Recompose bands separating anomalies from "normal" observations
### Aliases: time_recompose

### ** Examples


library(dplyr)

data(tidyverse_cran_downloads)

# Basic Usage
tidyverse_cran_downloads %>%
    time_decompose(count, method = "stl") %>%
    anomalize(remainder, method = "iqr") %>%
    time_recompose()





cleanEx()
nameEx("time_scale_template")
### * time_scale_template

flush(stderr()); flush(stdout())

### Name: set_time_scale_template
### Title: Get and modify time scale template
### Aliases: set_time_scale_template get_time_scale_template
###   time_scale_template

### ** Examples


get_time_scale_template()

set_time_scale_template(time_scale_template())




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
