Writing better code
================
2019-10-17

``` r
library(tidyverse)
```

Each exercise uses the `diamonds` data set in the ggplot2 package.

### Exercise 1

*Task: Calculate the relative frequency of `cut` for each level of
`color`*

#### Original Code

``` r
diamonds %>%
 select(color) %>%
 unique()
```

    ## # A tibble: 7 x 1
    ##   color
    ##   <ord>
    ## 1 E    
    ## 2 I    
    ## 3 J    
    ## 4 H    
    ## 5 F    
    ## 6 G    
    ## 7 D

``` r
# create separate DFs for each color
colorD <- diamonds %>%
 filter(color == "D")
colorE <- diamonds %>%
 filter(color == "E")
colorF <- diamonds %>%
 filter(color == "F")
colorG <- diamonds %>%
 filter(color == "G")
colorH <- diamonds %>%
 filter(color == "H")
colorI <- diamonds %>%
 filter(color == "I")
colorJ <- diamonds %>%
 filter(color == "J")

# calculate relative freq within each color df
relfreqD <- colorD %>%
 count(cut) %>%
 mutate(color = "D", rel_freq = n / sum(n))
relfreqE <- colorE %>%
 count(cut) %>%
 mutate(color = "E", rel_freq = n / sum(n))
relfreqF <- colorF %>%
 count(cut) %>%
 mutate(color = "F", rel_freq = n / sum(n))
relfreqG <- colorG %>%
 count(cut) %>%
 mutate(color = "G", rel_freq = n / sum(n))
relfreqH <- colorH %>%
 count(cut) %>%
 mutate(color = "H", rel_freq = n / sum(n))
relfreqI <- colorI %>%
 count(cut) %>%
 mutate(color = "I", rel_freq = n / sum(n))
relfreqJ <- colorJ %>%
 count(cut) %>%
 mutate(color = "J", rel_freq = n / sum(n))

# put it all together
bind_rows(relfreqD, relfreqE, relfreqF, relfreqG, relfreqH, relfreqI, relfreqJ) %>%
 select(-n)
```

    ## # A tibble: 35 x 3
    ##    cut       color rel_freq
    ##    <ord>     <chr>    <dbl>
    ##  1 Fair      D       0.0241
    ##  2 Good      D       0.0977
    ##  3 Very Good D       0.223 
    ##  4 Premium   D       0.237 
    ##  5 Ideal     D       0.418 
    ##  6 Fair      E       0.0229
    ##  7 Good      E       0.0952
    ##  8 Very Good E       0.245 
    ##  9 Premium   E       0.239 
    ## 10 Ideal     E       0.398 
    ## # … with 25 more rows

#### Revised Code

``` r
## Better way to do it ##
diamonds %>%
 count(color, cut) %>%
 group_by(color) %>%
 mutate(rel_freq = n/sum(n)) %>% 
  print(n = nrow(.)) #to show the whole tibble (only if necessary!)
```

    ## # A tibble: 35 x 4
    ## # Groups:   color [7]
    ##    color cut           n rel_freq
    ##    <ord> <ord>     <int>    <dbl>
    ##  1 D     Fair        163   0.0241
    ##  2 D     Good        662   0.0977
    ##  3 D     Very Good  1513   0.223 
    ##  4 D     Premium    1603   0.237 
    ##  5 D     Ideal      2834   0.418 
    ##  6 E     Fair        224   0.0229
    ##  7 E     Good        933   0.0952
    ##  8 E     Very Good  2400   0.245 
    ##  9 E     Premium    2337   0.239 
    ## 10 E     Ideal      3903   0.398 
    ## 11 F     Fair        312   0.0327
    ## 12 F     Good        909   0.0953
    ## 13 F     Very Good  2164   0.227 
    ## 14 F     Premium    2331   0.244 
    ## 15 F     Ideal      3826   0.401 
    ## 16 G     Fair        314   0.0278
    ## 17 G     Good        871   0.0771
    ## 18 G     Very Good  2299   0.204 
    ## 19 G     Premium    2924   0.259 
    ## 20 G     Ideal      4884   0.433 
    ## 21 H     Fair        303   0.0365
    ## 22 H     Good        702   0.0845
    ## 23 H     Very Good  1824   0.220 
    ## 24 H     Premium    2360   0.284 
    ## 25 H     Ideal      3115   0.375 
    ## 26 I     Fair        175   0.0323
    ## 27 I     Good        522   0.0963
    ## 28 I     Very Good  1204   0.222 
    ## 29 I     Premium    1428   0.263 
    ## 30 I     Ideal      2093   0.386 
    ## 31 J     Fair        119   0.0424
    ## 32 J     Good        307   0.109 
    ## 33 J     Very Good   678   0.241 
    ## 34 J     Premium     808   0.288 
    ## 35 J     Ideal       896   0.319

### Exercise 2

*Task: Find the `color` with the highest median price.*

#### Original Code

``` r
#function to calculate median price
calculate_median_price <- function(color_type) {
  diamonds %>%
    filter(color == color_type) %>%
    summarise(median(price)) %>%
    pull()
}

#get list of colors
unique_colors <- diamonds %>%
  select(color) %>%
  unique() %>%
  pull()

median_price_colors <- tibble(color = unique_colors, 
                              median = numeric(length(unique_colors)))

#calculate median price for each color
for (i in 1:length(unique_colors)) {
  median_price_colors[i, 2] <- calculate_median_price(unique_colors[i])
}

#find max
median_price_colors %>%
  filter(median == max(median)) %>%
  select(color, median)
```

    ## # A tibble: 1 x 2
    ##   color median
    ##   <ord>  <dbl>
    ## 1 J       4234

#### Revised Code

``` r
## One approach
 
diamonds %>%
  group_by(color) %>%
  summarise(median_price = median(price)) %>%
  filter(median_price == max(median_price)) %>%
  select(color, median_price)
```

    ## # A tibble: 1 x 2
    ##   color median_price
    ##   <ord>        <dbl>
    ## 1 J             4234

``` r
## Another approach
 
diamonds %>%
  group_by(color) %>%
  summarise(median_price = median(price)) %>%
  arrange(desc(median_price)) %>%
  slice(1)
```

    ## # A tibble: 1 x 2
    ##   color median_price
    ##   <ord>        <dbl>
    ## 1 J             4234
