Data Products Course Project
========================================================
author: Varamihir
date:   Dec 6, 2017
autosize: true

Overview
========================================================

This presentaion is being created as a part of the peer assessment for the coursera developing data products class. This assignment has two parts

1. A shiny applictaion and deploy it on Rstudio's server.
   
2. Slidify or Rstudio Presenter to prepare a reproducible presentaion.


Diamond Dataset
========================================================
A dataset containing the prices and other variables of almost 54,000 diamonds.
There are 10 variables including price, carat weight, cut, clairity, and color.
I used mainly price , carat, cut variables to find out that  how many diamonds are there in particular data. I used the slide bars that changes the price range and  carat you can choose the range, you would like to set in. 
I used radio buttons to select cut. Most imp thing is that If you move any  sliders or radio buttons you have to hit the submit  button to see change in the graph. The most of the diamonds fall in that $300-$10000 range.

Slide With code
========================================================


```r
# load the packages in R
library(ggplot2)
data(diamonds)
data <- as.data.frame(diamonds)
str(data)
```

```
'data.frame':	53940 obs. of  10 variables:
 $ carat  : num  0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...
 $ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...
 $ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
 $ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...
 $ depth  : num  61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...
 $ table  : num  55 61 65 58 58 57 57 55 61 61 ...
 $ price  : int  326 326 327 334 335 336 336 337 337 338 ...
 $ x      : num  3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
 $ y      : num  3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
 $ z      : num  2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...
```
 
Slide with plot
=======================================================

![plot of chunk diamonds](Project-figure/diamonds-1.png)
