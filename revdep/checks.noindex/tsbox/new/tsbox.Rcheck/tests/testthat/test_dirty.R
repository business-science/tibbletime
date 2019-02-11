library(testthat)
library(tsbox)

context("dirty situations")

test_that("works with df with improper col classes", {

  library(dplyr)
  x.chr <- ts_tbl(mdeaths) %>% 
    mutate(time = as.character(time))
 
  expect_is(ts_ts(x.chr), "ts")

  x.fct <- ts_tbl(mdeaths) %>% 
    mutate(time = as.factor(as.character(time)))
 
  expect_is(ts_ts(x.fct), "ts")
  
})

