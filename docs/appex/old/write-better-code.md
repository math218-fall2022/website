Writing better code
================
2019-10-16

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1       ✔ purrr   0.3.2  
    ## ✔ tibble  2.1.3       ✔ dplyr   0.8.0.1
    ## ✔ tidyr   0.8.1       ✔ stringr 1.4.0  
    ## ✔ readr   1.3.1       ✔ forcats 0.3.0

    ## ── Conflicts ───────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

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

1.  What makes this code inefficient and/or not reproducible? How can
    the code be improved?

2.  Write revised code to calculate the relative frequency of `cut` for
    each level of `color`.

**[Click here](write-better-code-sol.html) for example of revised
code.**

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
  select(color)
```

    ## # A tibble: 1 x 1
    ##   color
    ##   <ord>
    ## 1 J

1.  What makes this code inefficient and/or not reproducible? How can
    the code be improved?

2.  Write revised code to find the `color` with the highest median
    price.

**[Click here](write-better-code-sol.html) for example of revised
code.**
