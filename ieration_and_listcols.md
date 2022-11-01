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
    ## 1  4.59  3.25

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -10.6  3.62

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.0 0.379

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  99.6 0.944

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
    ## 1  4.59  3.25
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -10.6  3.62
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.0 0.379
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  99.6 0.944

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
    ## 1  4.59  3.25
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -10.6  3.62
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.0 0.379
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  99.6 0.944

The exact same output is produced.

so…what about other functions?

``` r
map(list_norm, median)
```

    ## $a
    ## [1] 4.94769
    ## 
    ## $b
    ## [1] -9.596721
    ## 
    ## $c
    ## [1] 17.03518
    ## 
    ## $d
    ## [1] 99.39583

map variants…

``` r
map_dbl(list_norm, median)
```

    ##         a         b         c         d 
    ##  4.947690 -9.596721 17.035178 99.395833

``` r
map_df(list_norm, mean_and_sd)
```

    ## # A tibble: 4 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1   4.59 3.25 
    ## 2 -10.6  3.62 
    ## 3  17.0  0.379
    ## 4  99.6  0.944

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
    ##  [1]  3.9865452  4.9421314  8.5392887  7.8272572  1.6468799  2.3357536
    ##  [7]  4.9532482  0.4302033  4.9771155 -0.4320727  7.0988235  3.2519286
    ## [13]  5.2222832  9.2612059  2.7485912  7.2711712  7.4503795  4.3278414
    ## [19] -2.6385488  8.6211766
    ## 
    ## $b
    ##  [1]  -8.028035  -9.146858 -14.728373  -6.783508 -13.555110  -5.207594
    ##  [7]  -8.333475 -13.569228 -11.348298 -17.984597 -11.563404 -13.163642
    ## [13] -10.046583  -8.492470  -6.545410 -17.654536  -9.056937  -6.597407
    ## [19]  -8.846025 -12.108535
    ## 
    ## $c
    ##  [1] 16.38491 16.79884 16.65355 17.61946 17.10087 16.44679 17.17678 17.02194
    ##  [9] 16.75461 17.56808 16.91058 17.45650 17.08675 16.84154 17.72220 16.85230
    ## [17] 16.82812 17.15563 17.51924 17.04842
    ## 
    ## $d
    ##  [1] 101.07366  99.05010  99.39834  97.91523  98.98573  98.92914 100.04858
    ##  [8]  99.39332 100.02572 100.49933  99.22550  98.28601  99.41879  99.25602
    ## [15] 100.84935 100.67900  98.45877 101.25918  99.29756 100.39579

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

Let’s write a short lil ol function

``` r
weather_lm = function(df){
  
  lm(tmax ~  tmin, data = df)
  
}

weather_lm(weather_nest_df[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nest_df[["data"]], weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

With a function we can map across the list

can i do all this in a tidy way

``` r
weather_nest_df %>% 
  mutate(
    model = map(data, weather_lm)
  )
```

    ## # A tibble: 3 × 4
    ##   name           id          data               model 
    ##   <chr>          <chr>       <list>             <list>
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>  
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

unnesting

``` r
weather_nest_df %>% 
  unnest(data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

## Napoleon

Here’s my scraping function that works for a single page

``` r
read_page_reviews = function(url) {
  
  dynamite_html = read_html(url)

  review_titles = 
    dynamite_html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()

  review_stars = 
    dynamite_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()

  review_text = 
    dynamite_html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim() %>% 
    str_subset("The media could not be loaded.", negate = TRUE) %>% 
    str_subset("^$", negate = TRUE)

  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
)

  reviews
}
```

What we did last time

``` r
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

vec_url = str_c(base_url, c(1, 2, 3, 4, 5))

dynamite_reviews = 
  bind_rows(
  read_page_reviews(vec_url[1]),
  read_page_reviews(vec_url[2]),
  read_page_reviews(vec_url[3]),
  read_page_reviews(vec_url[4]),
  read_page_reviews(vec_url[5]),
)

map(vec_url, read_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                                stars text                              
    ##    <chr>                                <dbl> <chr>                             
    ##  1 Still the best                           5 Completely stupid, absolutely no …
    ##  2 70’s and 80’s Schtick Comedy             5 …especially funny if you have eve…
    ##  3 Amazon Censorship                        5 I hope Amazon does not censor my …
    ##  4 Watch to say you did                     3 I know it's supposed to be a cult…
    ##  5 Best Movie Ever!                         5 We just love this movie and even …
    ##  6 Quirky                                   5 Good family film                  
    ##  7 Funny movie - can't play it !            1 Sony 4k player won't even recogni…
    ##  8 A brilliant story about teenage life     5 Napoleon Dynamite delivers dry hu…
    ##  9 HUHYAH                                   5 Spicy                             
    ## 10 Cult Classic                             4 Takes a time or two to fully appr…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                                         stars text                     
    ##    <chr>                                         <dbl> <chr>                    
    ##  1 Sweet                                             5 Timeless Movie. My Grand…
    ##  2 Cute                                              4 Fun                      
    ##  3 great collectible                                 5 one of the greatest movi…
    ##  4 Iconic, hilarious flick ! About friend ship .     5 Who doesn’t love this mo…
    ##  5 Funny                                             5 Me and my dad watched th…
    ##  6 Low budget but okay                               3 This has been a classic …
    ##  7 Disappointing                                     2 We tried to like this, b…
    ##  8 Favorite movie 🍿                                 5 This is one of my favori…
    ##  9 none                                              5 this movie was great Nap…
    ## 10 Great movie                                       5 Vote for pedro           
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 Get this to improve your nunchuck and bowstaff skills. Dancing i…     5 "Got…
    ##  2 Incredible Movie                                                      5 "Fun…
    ##  3 Always loved this movie!                                              5 "I h…
    ##  4 Great movie                                                           5 "Bou…
    ##  5 The case was damaged                                                  3 "It …
    ##  6 It’s classic                                                          5 "Cle…
    ##  7 Irreverent comedy                                                     5 "If …
    ##  8 Great classic!                                                        5 "Fun…
    ##  9 Most Awesomsomest Movie EVER!!!                                       5 "Thi…
    ## 10 Always a favorite                                                     5 "I r…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 It’s not working the disc keeps showing error when I tried other…     1 "It’…
    ##  2 Gosh!                                                                 5 "Eve…
    ##  3 An Acquired Taste                                                     1 "Thi…
    ##  4 What is this ?                                                        4 "Nic…
    ##  5 Napoleon Dynamite                                                     2 "I w…
    ##  6 Great movie                                                           5 "Gre…
    ##  7 Good movie                                                            5 "Goo…
    ##  8 Came as Described                                                     5 "Cam…
    ##  9 Oddly on my list of keepers.                                          5 "Goo…
    ## 10 Low budget fun                                                        5 "Odd…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 On a scale of 1 to 10 this rates a minus                              1 "Thi…
    ##  2 I always wondered...                                                  5 "wha…
    ##  3 Audio/video not synced                                                1 "I t…
    ##  4 Kind of feels like only a bully would actually laugh at this...       1 "...…
    ##  5 movie                                                                 5 "goo…
    ##  6 An Overdose of Comical Cringe                                         5 "Exc…
    ##  7 Glad I never wasted money on this                                     2 "I r…
    ##  8 A little disappointed                                                 3 "The…
    ##  9 An (almost) gem. Brought me back to the sweet awkwardness of hig…     5 "To …
    ## 10 How Could You Not Love Napoleon??                                     5 "I r…

``` r
napoleon_reviews = 
  tibble(
    page = 1:5,
    page_url = str_c(base_url, page)
  ) %>% 
  mutate(
    reviews = map(page_url, read_page_reviews)
  )

napoleon_reviews %>% 
  select(-page_url) %>% 
  unnest(reviews)
```

    ## # A tibble: 50 × 4
    ##     page title                                stars text                        
    ##    <int> <chr>                                <dbl> <chr>                       
    ##  1     1 Still the best                           5 Completely stupid, absolute…
    ##  2     1 70’s and 80’s Schtick Comedy             5 …especially funny if you ha…
    ##  3     1 Amazon Censorship                        5 I hope Amazon does not cens…
    ##  4     1 Watch to say you did                     3 I know it's supposed to be …
    ##  5     1 Best Movie Ever!                         5 We just love this movie and…
    ##  6     1 Quirky                                   5 Good family film            
    ##  7     1 Funny movie - can't play it !            1 Sony 4k player won't even r…
    ##  8     1 A brilliant story about teenage life     5 Napoleon Dynamite delivers …
    ##  9     1 HUHYAH                                   5 Spicy                       
    ## 10     1 Cult Classic                             4 Takes a time or two to full…
    ## # … with 40 more rows
