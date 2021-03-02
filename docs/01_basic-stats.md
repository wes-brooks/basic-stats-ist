# Uses of simulation


```r
install.packages( "remotes", repos="https://cloud.r-project.org" )
```

```
## Installing package into '/Users/wbrooks/Library/R/4.0/library'
## (as 'lib' is unspecified)
```

```
## 
## The downloaded binary packages are in
## 	/var/folders/7x/s3x3ymxs1_lcppnqy99_4kx40000gp/T//Rtmp5ibq7W/downloaded_packages
```

```r
remotes::install_github( "ProjectMOSAIC/mosaicModel" )
```

```
## Skipping install of 'mosaicModel' from a github remote, the SHA1 (0c7afbde) has not changed since last install.
##   Use `force = TRUE` to force installation
```


```r
library( "ggformula" )
```

```
## Loading required package: ggplot2
```

```
## Loading required package: ggstance
```

```
## 
## Attaching package: 'ggstance'
```

```
## The following objects are masked from 'package:ggplot2':
## 
##     geom_errorbarh, GeomErrorbarh
```

```
## Loading required package: scales
```

```
## Loading required package: ggridges
```

```
## 
## New to ggformula?  Try the tutorials: 
## 	learnr::run_tutorial("introduction", package = "ggformula")
## 	learnr::run_tutorial("refining", package = "ggformula")
```

```r
library( "mosaic" )
```

```
## Registered S3 method overwritten by 'mosaic':
##   method                           from   
##   fortify.SpatialPolygonsDataFrame ggplot2
```

```
## 
## The 'mosaic' package masks several functions from core packages in order to add 
## additional features.  The original behavior of these functions should not be affected by this.
```

```
## 
## Attaching package: 'mosaic'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     count, do, tally
```

```
## The following object is masked from 'package:Matrix':
## 
##     mean
```

```
## The following object is masked from 'package:scales':
## 
##     rescale
```

```
## The following object is masked from 'package:ggplot2':
## 
##     stat
```

```
## The following objects are masked from 'package:stats':
## 
##     binom.test, cor, cor.test, cov, fivenum, IQR, median, prop.test,
##     quantile, sd, t.test, var
```

```
## The following objects are masked from 'package:base':
## 
##     max, mean, min, prod, range, sample, sum
```

```r
library( "mosaicModel" )
```

```
## Loading required package: mosaicCore
```

```
## 
## Attaching package: 'mosaicCore'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     count, tally
```

```
## Loading required package: splines
```

The study of statistics started in the 1800s but only barely. The field's modern version was almost entirely developed during the first half of the 20th century - notably a time when data and processing power were in short supply. Today, that's not so much the case and if you did the assigned reading then you saw that statisticians are very much still grappling with how to teach statistics in light of the developments in computational power over the past 40 years.

Traditionally, statisticians are very concerned with assessing the normality of a sample, because the conclusions you get from traditional statistical methods depend on a sample coming from a normal distribution. Nowadays, there are a lot of clever methods that can avoid the need to assume normality. I'm going to show you some of those methods, because they help avoid getting into mathematical statistics. If you want to know more, one of the assigned readings was the introduction to a book that would be a great reference for self-guided study.

We will use simulation-based methods extensively today. 

This is the density curve of a standard normal distribution:


```r
# set z to -4, 4
z = seq(-4, 4, length.out = 1000)

# plot the standard normal density function with some annotations
plot( x=z, y=dnorm(z), bty='n', type='l', main="standard normal density")

# annotate the density function with the 5% probability mass tails
polygon(x=c(z[z<=qnorm(0.025)], qnorm(0.025), min(z)), y=c(dnorm(z[z<=qnorm(0.025)]), 0, 0), col=grey(0.8))

polygon(x=c(z[z>=qnorm(0.975)], max(z), qnorm(0.975)), y=c(dnorm(z[z>=qnorm(0.975)]), 0, 0), col=grey(0.8))
```

<img src="01_basic-stats_files/figure-html/annotated-density-tails-1.png" width="672" />

And this is a histogram of samples taken from that same distribution:

```r
# sample 20 numbers from a standard normal and draw the histogram
x = rnorm(20)
x
```

```
##  [1] -0.1203869 -0.7298514 -0.9480872 -0.6660846  0.2616201  0.4195260
##  [7]  1.2368281  0.7526406 -0.9510013 -0.1263623  0.1036075 -0.1122298
## [13]  0.3714595  1.0292116  0.4287446 -2.0602708 -1.7382680  0.3847519
## [19] -0.9698796  0.4521101
```

```r
hist(x)
```

<img src="01_basic-stats_files/figure-html/normal-histogram-1.png" width="672" />

Do the numbers seem to come from the high-density part of the Normal density curve? Are there any that don't? It isn't surprising if some of your `x` samples are not particularly close to zero. One out of twenty (that's five percent) samples from a standard Normal population are greater than two or less than negative two, on average. That's "on average" over the population. Your sample may be different.


Here is the density of the exponential distribution:

```r
# draw the desity of an Exponential distribution
t = seq(-1, 5, length.out=1000)
plot( x=t, y=dexp(t), bty='n', type='l')
```

<img src="01_basic-stats_files/figure-html/exponential-density-1.png" width="672" />

And here is a histogram of 20 samples taken from that distribution:

```r
# sample 20 numbers from a histogram and plot the histogram
ex = rexp( 20 )
ex
```

```
##  [1] 0.096905350 1.977980444 0.006700902 0.068623867 3.483478271 1.297969632
##  [7] 1.143132348 0.019287267 1.672262538 1.035041442 2.224021899 2.543659715
## [13] 0.626247054 0.533312676 0.640056599 1.382060519 1.098154545 1.431666216
## [19] 2.224287035 1.392858126
```

```r
hist( ex )
```

<img src="01_basic-stats_files/figure-html/exponential-histogram-1.png" width="672" />

The histograms are clearly different, but it would be difficult to definitively name the distribution of the data by looking at a sample.


## Mathematical statistics
The mean has some special properties: you've seen how we can calculate the frequency of samples being within an interval based on known distributions. But we need to know the distribution. It turns out that the distribution of the sample mean approaches the Normal distribution as the sample size increases, for almost any independent data. That allows us to create intervals and reason about the distribution of real data, even though the data's distribution is unknown.

### Law of large numbers
The law of large numbers says that if the individual measurements are independent, then the mean of a sample tends toward the mean of the population as the sample size gets larger. This is what we'd expect, since we showed the rate at which the variance of the sample mean gets smaller is $1/n$.


```r
nn = c(1, 2, 4, 8, 12, 20, 33, 45, 66, 100)
means = sapply( nn, function(n) mean( rnorm(n) ) )
plot(nn, means, bty='n', ylab = "sample mean")
abline(h=0, lty=2)
```

<img src="01_basic-stats_files/figure-html/unnamed-chunk-1-1.png" width="672" />


### Central limit theorem
The most important mathematical result in statistics, the Central Limit Theorem says that if you take (almost) any sample of random numbers and calculate its mean, the distribution of the mean tends toward a normal distribution. We illustrate the "tending toward" with an arrow and it indicates that the distribution of a sample mean is only *approximately* Normal. But if the original samples were from a Normal distribution then the sample mean has an *exactly* Normal distribution. From here, I'll start writing the mean of a random variable $X$ as $\bar{X}$ and the mean of a sample $x$ as $\bar{x}$.

$$ \bar{X} \rightarrow N(\mu, \frac{\sigma^2}{n}) $$
And because of the identities we learned before, you can write this as 
$$\frac{\bar{X} - \mu}{\sigma/\sqrt{n}} \rightarrow N(0, 1) $$

This is significant because we can use the standard normal functions on the right, and the data on the left, to start answering questions like, "what is the 95% confidence interval for the population mean?" 


```r
# generate 20 samples from a uniform distribution and plot their histogram
N = 20
u = rexp( N )
hist( u )
```

<img src="01_basic-stats_files/figure-html/clt-1.png" width="672" />

```r
# generate 100 repeated samples of the same size, calculate the mean of each one, and plot the histogram of the means.
B = 100
uu = numeric( B )
for ( i in 1:B ) {
  uu[[i]] = mean( rexp(N) )
}

hist(uu)
```

<img src="01_basic-stats_files/figure-html/clt-2.png" width="672" />

```r
# what happens as B and N get larger and smaller? Do they play different roles?
```

## Statistical inference

### Confidence intervals

In the `fosdata` package there is a dataset called `mice_pot`, which contains data from an experiment where mice were dosed with THC and then measured for motor activity as a percentage of their baseline activity. We are going to look at the group that got a medium dose of THC.


```r
# extract just the mice that got the medium dose of THC
mice_med = mice_pot[ mice_pot$group == 1, ]

# assess normality with histogram and QQ plot
hist( mice_med$percent_of_act )
```

<img src="01_basic-stats_files/figure-html/unnamed-chunk-2-1.png" width="672" />

```r
qqnorm( mice_med$percent_of_act )
```

<img src="01_basic-stats_files/figure-html/unnamed-chunk-2-2.png" width="672" />

#### Find the 80% confidence interval for the population mean

Now we are using our sample to make some determination about the population, so this is statistical inference. Our best guess of the population mean is the sample mean, `mean( mice_med$percent_of_act )`, which is 99.1%. But to get a confidence interval, we need to use the formula
$$ \bar{x} \pm t_{n-1, 0.1} * S / \sqrt{n} $$
Fortunately, R can do all the work for us:


```r
# 80% confidence interval for location of mice_med mean:
t.test( mice_med$percent_of_act, conf.level=0.8 )
```

```
## 
## 	One Sample t-test
## 
## data:  mice_med$percent_of_act
## t = 13.068, df = 11, p-value = 4.822e-08
## alternative hypothesis: true mean is not equal to 0
## 80 percent confidence interval:
##   88.71757 109.38712
## sample estimates:
## mean of x 
##  99.05235
```


### Two-population test
The test of $\mu_0 = 100$ is a one-population test because it seeks to compare a single population against a specified standard. On the other hand, you may wish to assess the null hypothesis that the movement of mice in the high-THC group is equal to the movement of mice in the medium-THC group. This is called a two-population test, since there are two populations to compare against each other. The null hypothesis is $\mu_{0, med} = \mu_{0, high}$. Testing a two-population hypothesis requires first assessing normality and also checking whether the variances are equal. There are separate procedures when the variances are equal vs. unequal.


```r
#extract the samples to be compared
a = mice_pot$percent_of_act[ mice_pot$group == 1]
b = mice_pot$percent_of_act[ mice_pot$group == 3]

# check for equal variances - these are close enough
var(a)
```

```
## [1] 689.4729
```

```r
var(b)
```

```
## [1] 429.4551
```

```r
# confirm equal variances with a boxplot
boxplot(a, b)
```

<img src="01_basic-stats_files/figure-html/two-pop-t.test-1.png" width="672" />

```r
# check whether the high-THC mice movement is Normal
# (we already checked for the medium-dose mice)
qqnorm(b)
```

<img src="01_basic-stats_files/figure-html/two-pop-t.test-2.png" width="672" />

```r
# two pop test
t.test(a, b, var.equal=TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  a and b
## t = 2.7707, df = 20, p-value = 0.0118
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   7.014608 49.754345
## sample estimates:
## mean of x mean of y 
##  99.05235  70.66787
```

### Hypothesis tests for non-normal data
Just as with the confidence intervals, there is a bootstrap hypothesis test that can be used where the data are not normal. There are other options, too, with clever derivations. The one I'll show you is the Wilcoxon test, which is based on the ranks of the data.

Since we'e already seen that the barnacles per square meter data are not normal, I will illustrate testing the null hypothesis that $\mu_0$ = 300 barnacles per square meter. This is a one-population test, and a two-sided alternative.


```r
# wilcoxon test for 300 barnacles per square meter
wilcox.test( barnacles$per_m )
```

```
## 
## 	Wilcoxon signed rank test with continuity correction
## 
## data:  barnacles$per_m
## V = 3916, p-value = 3.797e-16
## alternative hypothesis: true location is not equal to 0
```


## Regression

Regression is a mathematical tool that allows you to estimate how some response variable is related to some predictor variable(s). There are methods that handle continuous or discrete responses of many different distributions, but we are going to focus on linear regression here.

Linear regression means that the relationship between the predictor variable(s) and the response is a linear one. To illustrate, we'll create a plot of the relationship between the waist measurement and bmi of 81 adults:


```r
# import the adipose body fat data file
adipose = read.csv( url("https://raw.githubusercontent.com/ucdavisdatalab/basic_stats_r_1/main/data/adipose.csv") )

# plot the relationship between the waist_cm and bmi variables
with(adipose, plot(waist_cm, bmi), bty='n' )
```

<img src="01_basic-stats_files/figure-html/regression-example-1.png" width="672" />

The relationship between the two is apparently linear (you can imagine drawing a straight line through the data). The general mathematical form of a linear regression line is

$$ y = a + \beta x + \epsilon $$

Here, the response variable (e.g., BMI) is called $y$ and the predictor (e.g. waist measurement) is $x$. The coefficient $\beta$ indicates how much the response changes for a change in the predictors (e.g., the expected change in BMI with a 1cm change in waist measurement). Variable $a$ denotes the intercept, which is a constant offset that aligns the mean of $y$ with the mean of $x$. Finally, $\epsilon$ is the so-called residual error in the relationship. It represents the variation in the response that is not due to the predictor(s).


### Fitting a regression line
The R function to fit the model is called `lm()`. Let's take a look at an example:


```r
# fit the linear regression BMI vs waist_cm
fit = lm( bmi ~ waist_cm, data=adipose )

# plot the fitted regression: begin with the raw data
with( adipose, plot(waist_cm, bmi, bty='n') )

#now plot the fitted regression line (in red)
abline( coef(fit)[[1]], coef(fit)[[2]], col='red' )
```

<img src="01_basic-stats_files/figure-html/bmi-vs-waist-regression-1.png" width="672" />

### Assumptions and diagnostics
"Fitting" a linear regression model involves estimating $a$ and $\beta$ in the regression equation. You can can do this fitting procedure using any data, but the results won't be reliable unless some conditions are met. The conditions are:

1. Observations are independent.
2. The linear model is correct.
3. The residual error is Normally distributed.
4. The variance of the residual error is constant for all observations.

The first of these conditions can't be checked - it has to do with the design of the experiment. The rest can be checked, though, and I'll take them in order.

#### Checking that the linear model is correct
In the cast of a simple linear regression model (one predictor variable), you can check this by plotting the predictor against the response and looking for a linear trend. If you have more than one predictor variable, then you need to plot the predictions against the response to look for a linear trend. We'll see an example by adding height as a predictor for BMI (in addition to waist measurement).


```r
# linear model for BMI using waist size and height as predictors
fit2 = lm( bmi ~ waist_cm + stature_cm, data=adipose )

# plot the fitted versus the predicted values
plot( fit2$fitted.values, adipose$bmi, bty='n' )
```

<img src="01_basic-stats_files/figure-html/bmi-waist-height-1.png" width="672" />

#### Checking that the residuals are normally distributed
We have already learned about the QQ plot, which shows visually whether some values are Normally distributed. In order to depend upon the fit from a linear regression model, we need to see that the residuals are Normally distributed, and we use the QQ plot to check.


#### Checking that the vaiance is constant
In an earlier part, we saw that the variance is the average of the squared error. But that would just be a single number, when we want to see if there is a trend. So like the QQ plot, you'l plot the residuals and use your eyeball to discern whether there is a trend in the residuals or if they are approximately constant - this is called the scale-location plot. The QQ plot and scale-location plot are both created by plotting the fitted model object


```r
# set up the pattern of the panels
layout( matrix(1:4, 2, 2) )

# make the diagnostic plots
plot( fit )
```

<img src="01_basic-stats_files/figure-html/diagnostics-1.png" width="672" />

The "Residuals vs. Fitted" plot is checking whether the linear model is correct. There should be no obvious pattern if the data are linear (as is the casre here). The Scale-Location plot will have no obvios pattern if the variance of the residuals is constant, as is the case here (you might see a slight pattern in the smoothed red line but it isn't obvious). And the QQ plot will look like a straight line if the residuals are from a Normal distribution, as is the case here. So this model is good. The fourth diagnostic plot is the Residuals vs. Leverage plot, which is used to identify influential outliers. We won't get into that here.

### Functions for inspecting regression fits
When you fit a linear regression model, you are estimating the parameters of the regression equation. In order to see those estimates, use the `summary()` function on the fitted model object.


```r
# get the model summary
summary( fit2 )
```

```
## 
## Call:
## lm(formula = bmi ~ waist_cm + stature_cm, data = adipose)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.1290 -1.0484 -0.2603  1.2661  5.2572 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 14.38196    3.82700   3.758 0.000329 ***
## waist_cm     0.29928    0.01461  20.491  < 2e-16 ***
## stature_cm  -0.08140    0.02300  -3.539 0.000680 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.724 on 78 degrees of freedom
## Multiple R-squared:  0.844,	Adjusted R-squared:   0.84 
## F-statistic:   211 on 2 and 78 DF,  p-value: < 2.2e-16
```

Here you can see that the average marginal effect of one additional centimeter of waist measurement is to increase BMI by 0.3 and an additional centimeter of height is associated with a change to BMI of -0.08. You can get the coefficients from the fitted model object using the `coef()` function, and there are some other functions that allow you to generate the values shown in the summary table.


```r
# get the coefficients of the fitted regression
beta = coef( fit2 )
round( beta, 2 )
```

```
## (Intercept)    waist_cm  stature_cm 
##       14.38        0.30       -0.08
```


```r
# get the variance-covariance matrix
cat( "\nvariance-covariance matrix:\n" )
```

```
## 
## variance-covariance matrix:
```

```r
round( vcov( fit2 ), 4)
```

```
##             (Intercept) waist_cm stature_cm
## (Intercept)     14.6459  -0.0039    -0.0836
## waist_cm        -0.0039   0.0002    -0.0001
## stature_cm      -0.0836  -0.0001     0.0005
```


```r
# compare the square root of the diagonals of the variance-covariance matrix
# to the standard errors are reported in the summary table:
se = sqrt( diag( vcov(fit2) ))

cat( "\nstandard errors:\n")
```

```
## 
## standard errors:
```

```r
round( se, 3 )
```

```
## (Intercept)    waist_cm  stature_cm 
##       3.827       0.015       0.023
```


```r
# calculate the t-statistics for the regression coefficients
# (compare these to the t-statistics reorted in the summary table)
t_stats = beta / se

cat("\nt-statistics:\n")
```

```
## 
## t-statistics:
```

```r
round( t_stats, 2 )
```

```
## (Intercept)    waist_cm  stature_cm 
##        3.76       20.49       -3.54
```


```r
# calculate the p-values:
pval = 2 * pt( abs(t_stats), df=78, lower.tail=FALSE )
round(pval, 4)
```

```
## (Intercept)    waist_cm  stature_cm 
##       3e-04       0e+00       7e-04
```


```r
# this is the residual standard error:
sd( fit2$residuals ) * sqrt(80 / 78)
```

```
## [1] 1.72357
```

```r
# R-squared is the proportion of variance
# explained by the regression model
round( 1 - var(fit2$residuals) / var(adipose$bmi), 3 )
```

```
## [1] 0.844
```


### A model that fails diagnostics

We've seen a model that has good diagnostics. Now let's look at one that doesn't. This time, we'll use linear regression to make a model of the relationship between waist measurement and the visceral adipose tissue fat (measured in grams). The visceral adipose tissue fat is abbreviated `vat` in the data. First, since the model uses a single predictor variable, let's look at the relationship with a pair plot.


```r
# plot the relationship between waist_cm and vat
with( adipose, plot( waist_cm, vat, bty='n' ))
```

<img src="01_basic-stats_files/figure-html/vat-plot-1.png" width="672" />

The plot is obviously not showing a linear relationship, which will violate one of the conditions for linear regression. Also, you can see that there is less variance of vat among the observations that have smaller waist measurements. So that will violate the assumption that the residual variance has no relationship to the fitted values. To see how these will show up in the diagnostic plots, we need to fit the linear regression model.


```r
# estimate the model for vat
fit_vat = lm( vat ~ waist_cm, data = adipose )

# there is no problem creating the summary table:
summary( fit_vat )
```

```
## 
## Call:
## lm(formula = vat ~ waist_cm, data = adipose)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -996.25 -265.96  -61.87  191.24 1903.46 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3604.196    334.241  -10.78   <2e-16 ***
## waist_cm       51.353      3.937   13.04   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 479 on 79 degrees of freedom
## Multiple R-squared:  0.6829,	Adjusted R-squared:  0.6789 
## F-statistic: 170.2 on 1 and 79 DF,  p-value: < 2.2e-16
```

```r
# show the diagnostic plots
layout( matrix(1:4, 2, 2) )
plot( fit_vat )
```

<img src="01_basic-stats_files/figure-html/vat-regression-diagnostics-1.png" width="672" />

There is obviously a curved pattern in the Residuals vs. Fitted plot, and in the Scale vs. Location plot. Residuals vs. Fitted shows a fan-shaped pattern, too, which reflects the increasing variance among the greater fitted values. The QQ plot is not a straight line, although the difference is not as obvious. In particular, the upper tail of residuals is heavier than expected. Together, all of these are indications that we may need to do a log transformation of the response. A log transformation helps to exaggerate the differences between smaller numbers (make the lower tail heavier) and collapse some difference among larger numbers (make the upper tail less heavy).


```r
# fit a regression model where the response is log-transformed
fit_log = lm( log(vat) ~ waist_cm, data=adipose )

# plot the diagnostics for the log-transformed model
plot( fit_log )
```

<img src="01_basic-stats_files/figure-html/log-vat-1.png" width="672" /><img src="01_basic-stats_files/figure-html/log-vat-2.png" width="672" /><img src="01_basic-stats_files/figure-html/log-vat-3.png" width="672" /><img src="01_basic-stats_files/figure-html/log-vat-4.png" width="672" />

The diagnostics do not look good after the log transformation, but now the problem is the opposite: a too-heavy lower tail and residual variance decreases as the fitted value increases. Perhaps a better transformation is something in between the raw data and the log transform. Try a square-root transformation.


```r
# fit a model where the vat is square root transformed
fit_sqrt = lm( sqrt(vat) ~ waist_cm, data=adipose )

# plot the diagnostics for the log-transformed model
plot( fit_sqrt )
```

<img src="01_basic-stats_files/figure-html/vat-sqrt-1.png" width="672" /><img src="01_basic-stats_files/figure-html/vat-sqrt-2.png" width="672" /><img src="01_basic-stats_files/figure-html/vat-sqrt-3.png" width="672" /><img src="01_basic-stats_files/figure-html/vat-sqrt-4.png" width="672" />
These look acceptable for real-world data.

### Predictions and variability

There are two scales of uncertainty for a regression model: uncertainty in the fitted relationship, and the uncertainty of a predicted outcome. The uncertainty of a prediction is always greater because it is calculated by adding the uncertainty of the fitted line to the uncertainty of a single data point around that fitted line. We can illustrate using the example of the model we just created to relate the waist measurement to the square root of vat. For this block of code, we'll need the `mvtnorm` library to be loaded.


```r
# import mvtnorm. install it if necessary.
library( mvtnorm )

# draw the data on the transformed scale
with( adipose, plot(waist_cm, sqrt(vat), bty='n') )

# plot the fitted regression line
abline( coef(fit_sqrt)[[1]], coef(fit_sqrt)[[2]], col='red' )

# plot 100 samples from the distributon of the regression line.
for (i in 1:100) {
  cc = rmvnorm( n=1, mean=coef(fit_sqrt), sigma=vcov(fit_sqrt) )
  abline( cc[[1]], cc[[2]], col=grey(0.8))
}
```

<img src="01_basic-stats_files/figure-html/predictions-1.png" width="672" />

Clearly, the variability of the data points is greater than the variability of the fitted line (that's why they lie outside the envelope of the fitted lines). We can extract a confidence interval for fitted values or predictions with the `predict()` function.


```r
# draw the data on the transformed scale
with( adipose, plot(waist_cm, sqrt(vat), bty='n') )

# plot the fitted regression line
abline( coef(fit_sqrt)[[1]], coef(fit_sqrt)[[2]], col='red' )

# define some waist measurements where we'll construct confidence intervals
pred_pts = data.frame( waist_cm = c(70, 85, 110) )

# calculate the 90% CI at each of the pred_pts
ff = predict(fit_sqrt, pred_pts, interval="confidence", level=0.9)
pp = predict(fit_sqrt, pred_pts, interval="prediction", level=0.9)

# convert the confidence intervals to data.frames
ff = as.data.frame(ff)
pp = as.data.frame(pp)

# add the three confidence intervals to the plots
# (offset them a bit for clarity in the plot)
for (i in 1:3) {
  lines( x=rep( pred_pts$waist_cm[[i]] - 0.5, 2),
        y=c( ff$lwr[[i]], ff$upr[[i]] ), col='blue', lwd=2 )

  lines( x=rep( pred_pts$waist_cm[[i]] + 0.5, 2),
        y=c( pp$lwr[[i]], pp$upr[[i]] ), col='orange', lwd=2 )
}

# add a legend
legend(c("90% CI (fitted values)", "90% CI (predicted values)"),
       x="topleft", lwd=2, col=c("blue", "orange"), bty='n')
```

<img src="01_basic-stats_files/figure-html/prediction-intervals-1.png" width="672" />

One thing to notice about the confidence intervals is that the interval is smallest (so the precision of the estimation is greatest) at the mean of the predictor variable. This is a general rule of fitting regression.


## Model selection
Choosing how to represent your data is a common task in statistics. The most common target is to choose the representation (or model) that does the best job of predicting new data. We set this target because if we have a representation that predicts the future, then we can say it must accurately represent the process that generates the data.

## Cross-validation
Unfortunately, we rarely have information about the future, so there isn't new data to predict. One way to do prediction with the available data is to break it into a training part and a testing part. You make represent the training part with a model, and then use it to predict the left-out testing part. If you then swap the to parts and repeat the process, you'll have a prediction for every data point. This would be called two-fold cross valdation because the data was broken into two parts.

It's more common to break the data into more than two parts - typically five or ten or one per data point. Then one part is taken as the testing part and all the others go into the training part. The result is five-fold or ten-fold, or leave-one-out cross validation.

Let's use cross-validation to do model selection. The model this time is a representation of the number of births per day in 1978 in the United States.


```r
# import the Births78 data
Births78 = read.csv( url("https://raw.githubusercontent.com/ucdavisdatalab/adventures_in_data_science/master/data/births.csv") )

# create the weekend factor
Births78$wknd=as.factor( ifelse(Births78$wday %in% c("Sat","Sun"), "yes", "no"))

# plot the data
gf_point( births ~ day_of_year, color = ~wknd, data=Births78 )
```

<img src="01_basic-stats_files/figure-html/cv-1.png" width="672" />

```r
# make models with two through ten knots in the spline for day_of_year
bmod2 = lm( births ~ wknd + ns(day_of_year, 2), data=Births78 )
bmod4 = lm( births ~ wknd + ns(day_of_year, 4), data=Births78 )
bmod6 = lm( births ~ wknd + ns(day_of_year, 6), data=Births78 )
bmod8 = lm( births ~ wknd + ns(day_of_year, 8), data=Births78 )
bmod10 = lm( births ~ wknd + ns(day_of_year, 10), data=Births78 )

# plot the 2 and 10 knot models
mod_plot(bmod2, births~day_of_year + wknd) +
  geom_point(mapping=aes(x=day_of_year, y=births, color=wknd), data=Births78)
```

<img src="01_basic-stats_files/figure-html/cv-2.png" width="672" />

```r
mod_plot(bmod10, births~day_of_year + wknd) +
  geom_point(mapping=aes(x=day_of_year, y=births, color=wknd), data=Births78)
```

<img src="01_basic-stats_files/figure-html/cv-3.png" width="672" />

```r
# cross-validate to choose the best model
mod_cv( bmod2, bmod4, bmod6, bmod8, bmod10, k=nrow(Births78), ntrials=1 )
```

```
##        mse  model
## 1 190815.9  bmod2
## 2 143305.1  bmod4
## 3 104875.7  bmod6
## 4 106094.2  bmod8
## 5 107130.5 bmod10
```

```r
# plot the data
mod_plot(bmod6, births~day_of_year + wknd) +
  geom_point(mapping=aes(x=day_of_year, y=births, color=wknd), data=Births78)
```

<img src="01_basic-stats_files/figure-html/cv-4.png" width="672" />

Cross-validation suggests that six knots is the ideal number, because it has the smallest mean-squared error (mse). The resulting model looks good, too.
