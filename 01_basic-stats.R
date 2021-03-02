## ----install-remotes-mosaicModels--------------------------------------------------------
install.packages( "remotes", repos="https://cloud.r-project.org" )
remotes::install_github( "ProjectMOSAIC/mosaicModel" )


## ----import-packages---------------------------------------------------------------------
library( "ggformula" )
library( "mosaic" )
library( "mosaicModel" )


## ----annotated-density-tails-------------------------------------------------------------
# set z to -4, 4
z = seq(-4, 4, length.out = 1000)

# plot the standard normal density function with some annotations
plot( x=z, y=dnorm(z), bty='n', type='l', main="standard normal density")

# annotate the density function with the 5% probability mass tails
polygon(x=c(z[z<=qnorm(0.025)], qnorm(0.025), min(z)), y=c(dnorm(z[z<=qnorm(0.025)]), 0, 0), col=grey(0.8))

polygon(x=c(z[z>=qnorm(0.975)], max(z), qnorm(0.975)), y=c(dnorm(z[z>=qnorm(0.975)]), 0, 0), col=grey(0.8))


## ----normal-histogram--------------------------------------------------------------------
# sample 20 numbers from a standard normal and draw the histogram
x = rnorm(20)
x
hist(x)


## ----exponential-density-----------------------------------------------------------------
# draw the desity of an Exponential distribution
t = seq(-1, 5, length.out=1000)
plot( x=t, y=dexp(t), bty='n', type='l')


## ----exponential-histogram---------------------------------------------------------------
# sample 20 numbers from a histogram and plot the histogram
ex = rexp( 20 )
ex
hist( ex )


## ----------------------------------------------------------------------------------------
nn = c(1, 2, 4, 8, 12, 20, 33, 45, 66, 100)
means = sapply( nn, function(n) mean( rnorm(n) ) )
plot(nn, means, bty='n', ylab = "sample mean")
abline(h=0, lty=2)


## ----clt---------------------------------------------------------------------------------
# generate 20 samples from a uniform distribution and plot their histogram
N = 20
u = rexp( N )
hist( u )

# generate 100 repeated samples of the same size, calculate the mean of each one, and plot the histogram of the means.
B = 100
uu = numeric( B )
for ( i in 1:B ) {
  uu[[i]] = mean( rexp(N) )
}

hist(uu)

# what happens as B and N get larger and smaller? Do they play different roles?


## ----------------------------------------------------------------------------------------
# extract just the mice that got the medium dose of THC
mice_med = mice_pot[ mice_pot$group == 1, ]

# assess normality with histogram and QQ plot
hist( mice_med$percent_of_act )
qqnorm( mice_med$percent_of_act )


## ----on-popt-test------------------------------------------------------------------------
# 80% confidence interval for location of mice_med mean:
t.test( mice_med$percent_of_act, conf.level=0.8 )


## ----two-pop-t.test----------------------------------------------------------------------
#extract the samples to be compared
a = mice_pot$percent_of_act[ mice_pot$group == 1]
b = mice_pot$percent_of_act[ mice_pot$group == 3]

# check for equal variances - these are close enough
var(a)
var(b)

# confirm equal variances with a boxplot
boxplot(a, b)

# check whether the high-THC mice movement is Normal
# (we already checked for the medium-dose mice)
qqnorm(b)

# two pop test
t.test(a, b, var.equal=TRUE)


## ----wilcox-barnacles--------------------------------------------------------------------
# wilcoxon test for 300 barnacles per square meter
wilcox.test( barnacles$per_m )


## ----regression-example------------------------------------------------------------------
# import the adipose body fat data file
adipose = read.csv( url("https://raw.githubusercontent.com/ucdavisdatalab/basic_stats_r_1/main/data/adipose.csv") )

# plot the relationship between the waist_cm and bmi variables
with(adipose, plot(waist_cm, bmi), bty='n' )


## ----bmi-vs-waist-regression-------------------------------------------------------------
# fit the linear regression BMI vs waist_cm
fit = lm( bmi ~ waist_cm, data=adipose )

# plot the fitted regression: begin with the raw data
with( adipose, plot(waist_cm, bmi, bty='n') )

#now plot the fitted regression line (in red)
abline( coef(fit)[[1]], coef(fit)[[2]], col='red' )


## ----bmi-waist-height--------------------------------------------------------------------
# linear model for BMI using waist size and height as predictors
fit2 = lm( bmi ~ waist_cm + stature_cm, data=adipose )

# plot the fitted versus the predicted values
plot( fit2$fitted.values, adipose$bmi, bty='n' )


## ----diagnostics-------------------------------------------------------------------------
# set up the pattern of the panels
layout( matrix(1:4, 2, 2) )

# make the diagnostic plots
plot( fit )


## ----regression-summary------------------------------------------------------------------
# get the model summary
summary( fit2 )


## ----regression-coefficients-------------------------------------------------------------
# get the coefficients of the fitted regression
beta = coef( fit2 )
round( beta, 2 )


## ----variance-covariance-----------------------------------------------------------------
# get the variance-covariance matrix
cat( "\nvariance-covariance matrix:\n" )
round( vcov( fit2 ), 4)


## ----standard-error----------------------------------------------------------------------
# compare the square root of the diagonals of the variance-covariance matrix
# to the standard errors are reported in the summary table:
se = sqrt( diag( vcov(fit2) ))

cat( "\nstandard errors:\n")
round( se, 3 )


## ----t-stat------------------------------------------------------------------------------
# calculate the t-statistics for the regression coefficients
# (compare these to the t-statistics reorted in the summary table)
t_stats = beta / se

cat("\nt-statistics:\n")
round( t_stats, 2 )


## ----p-values----------------------------------------------------------------------------
# calculate the p-values:
pval = 2 * pt( abs(t_stats), df=78, lower.tail=FALSE )
round(pval, 4)


## ----residual-variance-------------------------------------------------------------------
# this is the residual standard error:
sd( fit2$residuals ) * sqrt(80 / 78)

# R-squared is the proportion of variance
# explained by the regression model
round( 1 - var(fit2$residuals) / var(adipose$bmi), 3 )


## ----vat-plot----------------------------------------------------------------------------
# plot the relationship between waist_cm and vat
with( adipose, plot( waist_cm, vat, bty='n' ))


## ----vat-regression-diagnostics----------------------------------------------------------
# estimate the model for vat
fit_vat = lm( vat ~ waist_cm, data = adipose )

# there is no problem creating the summary table:
summary( fit_vat )

# show the diagnostic plots
layout( matrix(1:4, 2, 2) )
plot( fit_vat )


## ----log-vat-----------------------------------------------------------------------------
# fit a regression model where the response is log-transformed
fit_log = lm( log(vat) ~ waist_cm, data=adipose )

# plot the diagnostics for the log-transformed model
plot( fit_log )


## ----vat-sqrt----------------------------------------------------------------------------
# fit a model where the vat is square root transformed
fit_sqrt = lm( sqrt(vat) ~ waist_cm, data=adipose )

# plot the diagnostics for the log-transformed model
plot( fit_sqrt )


## ----predictions-------------------------------------------------------------------------
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


## ----prediction-intervals----------------------------------------------------------------
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


## ----cv----------------------------------------------------------------------------------
# import the Births78 data
Births78 = read.csv( url("https://raw.githubusercontent.com/ucdavisdatalab/adventures_in_data_science/master/data/births.csv") )

# create the weekend factor
Births78$wknd=as.factor( ifelse(Births78$wday %in% c("Sat","Sun"), "yes", "no"))

# plot the data
gf_point( births ~ day_of_year, color = ~wknd, data=Births78 )

# make models with two through ten knots in the spline for day_of_year
bmod2 = lm( births ~ wknd + ns(day_of_year, 2), data=Births78 )
bmod4 = lm( births ~ wknd + ns(day_of_year, 4), data=Births78 )
bmod6 = lm( births ~ wknd + ns(day_of_year, 6), data=Births78 )
bmod8 = lm( births ~ wknd + ns(day_of_year, 8), data=Births78 )
bmod10 = lm( births ~ wknd + ns(day_of_year, 10), data=Births78 )

# plot the 2 and 10 knot models
mod_plot(bmod2, births~day_of_year + wknd) +
  geom_point(mapping=aes(x=day_of_year, y=births, color=wknd), data=Births78)

mod_plot(bmod10, births~day_of_year + wknd) +
  geom_point(mapping=aes(x=day_of_year, y=births, color=wknd), data=Births78)


# cross-validate to choose the best model
mod_cv( bmod2, bmod4, bmod6, bmod8, bmod10, k=nrow(Births78), ntrials=1 )


# plot the data
mod_plot(bmod6, births~day_of_year + wknd) +
  geom_point(mapping=aes(x=day_of_year, y=births, color=wknd), data=Births78)

