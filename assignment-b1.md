Assignment B1
================

## Setup

``` r
# set up
suppressMessages(library(rlang))
suppressMessages(library(testthat))
suppressMessages(library(tidyverse))
```

## Function

``` r
#' @title
#' Summary of numeric variable within data frame
#' 
#' @description
#' This function takes a numeric variable and a categorical variable within a data frame and calculates 
#' the minimum, maximum, mean, median, and number of observations for the numeric values within each 
#' group.
#' 
#' @param df A data frame that contains the categorical variable and numerical variable of interest. For 
#' clarity and user intuitive purposes, this variable is named "df", shortform for *d*ata*f*rame.
#' 
#' @param categorical A categorical variable within the data frame. For clarity and user intuitive purposes,
#' this variable is named "categorical". 
#' 
#' @param numerical A numeric variable within the data frame. For clarity and user intuitive purposes,
#' this variable is named "numerical".
#' 
#' @return A summary table with statistical values.
#' \itemize{
#'   \item min: The minimum value per group(s). 
#'   \item max: The maximum value per group(s). 
#'   \item mean: The average value per group(s). 
#'   \item med: The median value per group(s).
#'   \item n: The number of value per group(s).
#' } 

summary_table <- function (df, categorical, numerical, na.rm = TRUE) 
{check <- df %>%
  pull({{numerical}}) %>%
  is.numeric

if (check==TRUE) {
   df %>%
     group_by({{categorical}}) %>%
     summarise(min = min({{numerical}}, na.rm = TRUE),
            max = max({{numerical}}, na.rm = TRUE),
            mean = mean({{numerical}}, na.rm = TRUE),
            med = median({{numerical}}, na.rm = TRUE),
            n = n())
  } 

else stop("the numerical variable you've included is not numerical")    
}
```

## Examples

#### Example 1

This example demonstrates the function calculating summary table for the
numerical variable **“Petal.Width”** based on categorical variable
**“Species”** within the data frame **“iris”**.

``` r
# executing function
summary_table(iris, Species, Petal.Width)
```

    ## # A tibble: 3 × 6
    ##   Species      min   max  mean   med     n
    ##   <fct>      <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 setosa       0.1   0.6 0.246   0.2    50
    ## 2 versicolor   1     1.8 1.33    1.3    50
    ## 3 virginica    1.4   2.5 2.03    2      50

#### Example 2

This example demonstrates the function calculating a summary table for
the numerical variable **“weight”** based on categorical variable
**“group”** within the data frame **“PlantGrowth”**.

``` r
# executing function
summary_table(PlantGrowth, group, weight)
```

    ## # A tibble: 3 × 6
    ##   group   min   max  mean   med     n
    ##   <fct> <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 ctrl   4.17  6.11  5.03  5.15    10
    ## 2 trt1   3.59  6.03  4.66  4.55    10
    ## 3 trt2   4.92  6.31  5.53  5.44    10

#### Example 3

This example demonstrates an input error where the numerical variable
**“Petal.Width”** is written where a categorical variable is expected,
and the categorical variable **“Species”** is written where the
numerical variable is expected. Both are from the data frame **“iris”**.

``` r
# executing function error
summary_table(iris, Petal.Width, Species)
```

    ## Error in summary_table(iris, Petal.Width, Species): the numerical variable you've included is not numerical

## Test the Function

#### Initial Setup

``` r
# calculates the summary table manually
summary_table_manual <- iris %>%
  group_by(Species) %>%
  summarise(min = min(Petal.Width, na.rm = TRUE),
            max = max(Petal.Width, na.rm = TRUE),
            mean = mean(Petal.Width, na.rm = TRUE),
            med = median(Petal.Width, na.rm = TRUE),
            n = n())
```

#### Test 1

``` r
test_that("checks that the summary_table function outputs the same table as the manual code", {
  expect_vector(summary_table_manual, summary_table(iris, Species, Petal.Width)) # vector of expected size
  expect_identical(summary_table_manual, summary_table(iris, Species, Petal.Width)) # contents the same
  })
```

    ## Test passed 🥳

#### Test 2

``` r
test_that("checks that the output of the summary_table function has the expected columns", {
  expect_named(summary_table(iris, Species, Petal.Width), c("Species", "min", "max", "mean", "med", "n"))
  })
```

    ## Test passed 🎉

#### Test 3

``` r
test_that("checks that an error occurs when a numerical variable is not included where it should be", {
  expect_error(summary_table(iris, Petal.Width, Species), 
               "the numerical variable you've included is not numerical")
  })
```

    ## Test passed 🥳
