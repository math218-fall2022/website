STA 199 Sticker
================
Maria Tackett
01/02/2019

This document contains the code to make the sticker for [STA 199: Intro to Data Science](https://www2.stat.duke.edu/courses/Spring19/sta199.001/). The following packages are used in this project:

``` r
library(hexSticker)
library(readr)
library(tidyverse)
library(tibble)
library(showtext)
library(nnet)
library(knitr)
```

The Data
--------

The data is the [Capital Bikeshare data set](https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset) pulled from the UCI Machine Learning Repository. The following variables are used in this project:

-   `season`: 1 - Winter, 2 - Spring, 3 - Summer 4 - Fall
-   `atemp`: feeling temperature ÷ 50 (in degrees Celsius) <br>

``` r
bikeshare <- read_csv("https://raw.githubusercontent.com/matackett/data/master/capital-bikeshare.csv")    
bikeshare <- bikeshare %>%
  mutate(season = case_when(
    season==1 ~ "Winter",
    season==2 ~ "Spring",
    season==3 ~ "Summer",
    season==4 ~ "Fall"
  )) %>% 
  select(season,cnt,atemp)
```

Density Plot
------------

A multinomial logistic regression model is used to create the main plot. The response variable is `season` and the predictor variable is `atemp`.

``` r
p <- ggplot(data=bikeshare,aes(x=cnt,fill=season)) +
  geom_density(alpha=0.7) +
  scale_fill_manual( values = c("#F8766D","#7CAE00","#C77CFF","#00BFC4")) +
  theme_void() +
  theme(legend.position="none") 
```

Make Sticker
------------

``` r
# add font to be used in sticker function
font_add_google("Open Sans", "open")
```

``` r
# create and save sticker
sticker(p, package="STA 199",p_color="#7A4183", p_family="open", p_size=7.5, p_y=0.55, s_x=1, s_y=1.25, s_width=1.3, s_height=1, h_fill = "#FFFFFF", h_color="#7A4183", h_size =0.8, filename="static/img/sta199_sticker.png")
```

``` r
#display final sticker
include_graphics("static/img/sta199_sticker.png")
```

![](static/img/sta199_sticker.png)
