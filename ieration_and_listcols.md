Iterations and List Columns
================
Elaine Yanxi Chen
2022-11-01

## Packages and settings

First we load the packages necessary to knit this document.

``` r
library(tidyverse)
library(rvest)

knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
    fig.width = 8, 
  fig.height = 6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Lists

``` r
vec_numeric = 5:8
vec_logical = c(TRUE, FALSE, TRUE, TRUE)
```

Let’s look at a list.

``` r
l = list(
  vec_numeric = 5:8,
  mat         = matrix(1:8, 2, 4),
  vec_logical = c(TRUE, FALSE),
  summary     = summary(rnorm(1000))
)
```

Accessing list items

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[3]]
```

    ## [1]  TRUE FALSE

``` r
l[["mat"]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

## Loops!

Let’s write a `for` loop

``` r
list_norm =
  list(
    a = rnorm(20, 5, 4),
    b = rnorm(20, -12, 3),
    c = rnorm(20, 17, 0.4),
    d = rnorm(20, 100, 1)
  )
```

Here’s my function.

``` r
mean_and_sd = function(x) {
   
  if (!is.numeric(x)) {
    stop("Z scores only work for numbers")
  }
  
  if (length(x) < 3) {
    stop("Z scores really only work if you have three or more numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}
```

Let’s try to make this work.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.90  4.49

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -11.9  3.24

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  16.9 0.368

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  99.9 0.968

But this is painfully repetitive. And also hard to save.

Let’s do a `for` loop instead.

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.90  4.49
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -11.9  3.24
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  16.9 0.368
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  99.9 0.968

If we pnly look at `output`, it is just an empty object with 4 spots.
When we run `output[[1]]`, we see that the mean and sd for the first
list has been calculated and put in the right spot.

## can we map??

we can map!!

``` r
map(list_norm, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.90  4.49
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -11.9  3.24
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  16.9 0.368
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  99.9 0.968

The exact same output is produced.

so…what about other functions?

``` r
map(list_norm, median)
```

    ## $a
    ## [1] 4.171708
    ## 
    ## $b
    ## [1] -12.21773
    ## 
    ## $c
    ## [1] 16.84821
    ## 
    ## $d
    ## [1] 99.86497

map variants…

``` r
map_dbl(list_norm, median)
```

    ##          a          b          c          d 
    ##   4.171708 -12.217726  16.848211  99.864969

``` r
map_df(list_norm, mean_and_sd)
```

    ## # A tibble: 4 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1   3.90 4.49 
    ## 2 -11.9  3.24 
    ## 3  16.9  0.368
    ## 4  99.9  0.968

stack the rows up and condense the table, simplify the output and can be
easily saved

## list columns

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    norm = list_norm
  )

listcol_df[["norm"]]
```

    ## $a
    ##  [1]  5.1105130 11.9615751  2.6776607  9.8086450  3.9952966  5.6203164
    ##  [7]  0.3783512 -6.7146600 -1.0374867 10.7370014  4.9482581  4.5869586
    ## [13]  7.8259223  4.3481200  7.3431477  2.6393398  0.5124249 -2.1322031
    ## [19]  3.0316478  2.3300935
    ## 
    ## $b
    ##  [1] -14.044951 -13.260036 -12.391501 -12.043950 -16.043575 -15.707836
    ##  [7]  -9.230825 -17.474832  -9.359729 -11.685335 -14.290340  -7.073452
    ## [13]  -9.425742  -6.890075 -10.490291 -14.092745 -14.112438 -15.503259
    ## [19]  -7.684184  -8.112074
    ## 
    ## $c
    ##  [1] 17.05837 16.32954 16.83893 17.02810 16.53473 16.58682 17.36775 16.85749
    ##  [9] 16.86426 16.67716 17.11112 16.54760 16.14714 17.30275 16.75425 16.57349
    ## [17] 17.67523 17.21309 16.78437 16.92205
    ## 
    ## $d
    ##  [1] 100.24692  99.57437  98.94216 100.43250 101.09273  99.87193  98.17698
    ##  [8] 100.96603  99.85801 101.64893 101.05167  98.68039  99.28416 100.67417
    ## [15]  98.87544  99.08260  98.88445 100.53094 100.82110  99.76941

``` r
output = map(listcol_df[["norm"]], mean_and_sd)
```

can we add list columns, and then what

save the results into a new column by mapping the norm vectors into the
`mean_and_sd` function

``` r
listcol_df %>% 
  mutate(
    m_sd = map(norm, mean_and_sd)
  ) %>% 
  select(-norm)
```

    ## # A tibble: 4 × 2
    ##   name  m_sd            
    ##   <chr> <named list>    
    ## 1 a     <tibble [1 × 2]>
    ## 2 b     <tibble [1 × 2]>
    ## 3 c     <tibble [1 × 2]>
    ## 4 d     <tibble [1 × 2]>

## What about something more realisitic…

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2022-09-22 20:15:00 (8.402)

    ## file min/max dates: 1869-01-01 / 2022-09-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2022-09-22 20:15:04 (1.699)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2022-09-22 20:15:06 (0.95)

    ## file min/max dates: 1999-09-01 / 2022-09-30

Let’s nest within weather stations…

``` r
weather_df %>% 
  nest(data = date:tmin)
```

    ## # A tibble: 3 × 3
    ##   name           id          data              
    ##   <chr>          <chr>       <list>            
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]>
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]>
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]>

``` r
weather_nest_df = 
 weather_df %>% 
  nest(data = date:tmin)
```

collapse into a single column; `weather_nest_df` is really just a list
column

``` r
weather_nest_df[["data"]]
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
weather_nest_df[["data"]][[1]]
```

    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows

``` r
lm(tmax ~ tmin, data = weather_nest_df[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039
