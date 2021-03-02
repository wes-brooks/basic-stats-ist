## -------------------------------------------------------------------------------------------------------
data( package="fosdata" )
library( 'fosdata' )

?fosdata::wrist
data( "wrist" )

View( wrist )



## -------------------------------------------------------------------------------------------------------
# X is a random variable, it follows the sandard normal distribution
X = rnorm(1)

# Once we have "realized" a value of X, it is data and no longer random.
X


## -------------------------------------------------------------------------------------------------------
x = rnorm(10)
mean(x)
median(x)


## -------------------------------------------------------------------------------------------------------
# Demonstrate the standard normal distribution function:
pnorm(0)
pnorm(-1)
pnorm(1)

# draw the standard normal distribution function, with some annotations
t = seq(-4, 4, length.out=1001)
plot( x=t, y=pnorm(t), bty='n', type='line', main="standard normal distribution function")

# annotate the plot with lines indicating the quantiles for a 95% probability interval
lines(x=c(min(t), qnorm(0.025)), y=rep(0.025, 2), lty=2)
lines(x=c(min(t), qnorm(0.975)), y=rep(0.975, 2), lty=2)

lines(x=rep(qnorm(0.025), 2), y=c(0, 0.025), lty=3)
lines(x=rep(qnorm(0.975), 2), y=c(0, 0.975), lty=3)


## -------------------------------------------------------------------------------------------------------
plot( x=t, y=ppois(t, lambda=1), bty='n', type='line', main="Poisson(1) distribution function")
plot( x=t, y=pchisq(t, df=2), bty='n', type='line', main="Chi-squared (df=7) distribution function")


## -------------------------------------------------------------------------------------------------------
# Demonstrate the standard normal density function:
dnorm(0)
dnorm(-1)
dnorm(1)

# plot the standard normal density function with some annotations
t = seq(-3, 3, length.out=1001)
plot( x=t, y=dnorm(t), bty='n', type='l', main="standard normal density")

# annotate the density function with the 5% probability mass tails
polygon(x=c(t[t<=qnorm(0.025)], qnorm(0.025), min(t)), y=c(dnorm(t[t<=qnorm(0.025)]), 0, 0), col=grey(0.8))

polygon(x=c(t[t>=qnorm(0.975)], max(t), qnorm(0.975)), y=c(dnorm(t[t>=qnorm(0.975)]), 0, 0), col=grey(0.8))



## -------------------------------------------------------------------------------------------------------
# plot the poisson and chi-squared probability densities
plot( x=t, y=dunif(t), bty='n', type='l', main="Uniform(0, 1) distribution function")
plot( x=t, y=dchisq(t, df=2), bty='n', type='l', main="Chi-squared (df=7) distribution function")


## -------------------------------------------------------------------------------------------------------
# demonstrate quantiles of the standard normal distribution
qnorm(0.95)
qnorm(0.5)

# if you want an interval that contains the random variable 95% of the time, you may divide the remaining 5% in half, so the interval is in the center:
(1 - 0.95) / 2
qnorm(0.025)
qnorm(0.975)


## -------------------------------------------------------------------------------------------------------
X = function(n=10, mean=-3, sd=3) { rnorm(n, mean=mean, sd=sd) }
Y = function(n=10, mean=10, sd=2) { rnorm(n, mean=mean, sd=sd) }

mean( X() )
mean( Y() )

var( X() )
var( Y() )

var( 2*X() )
var( X() + 0.5*Y() )


## -------------------------------------------------------------------------------------------------------
nn = c(1, 2, 4, 8, 12, 20, 33, 45, 66, 100)
means = sapply( nn, function(n) mean( rnorm(n) ) )
plot(nn, means, bty='n', ylab = "sample mean")
abline(h=0, lty=2)


## -------------------------------------------------------------------------------------------------------
# plot the densities of a standard normal distribution, and of a t-distribution with five degrees of freedom:

plot( x=t, y=dnorm(t), ylim=c(0, 0.5), bty='n', lty=2, type='l', ylab="density" )
par( new=TRUE )
plot( x=t, y=dt(t, df=5), ylim=c(0, 0.5), xaxt='n', yaxt='n', xlab='', ylab='', bty='n', col='red', type='l' )


## -------------------------------------------------------------------------------------------------------
# generate 20 samples from a uniform distribution and plot their histogram
N = 20
u = runif( N )
hist( u )

# generate 100 repeated samples of the same size, calculate the mean of each one, and plot the histogram of the means.
B = 100
uu = numeric( B )
for ( i in 1:B ) {
  uu[[i]] = mean( runif(N) )
}

hist(uu)

# what happens as B and N get larger and smaller? Do they play different roles?


## -------------------------------------------------------------------------------------------------------
# sample 50 numbers from the standard normal distribution
y = rnorm(50)

# look at the histogram - it's like the density function
hist(y)

# look at the empirical distribution function - it's like the distribution function
plot( ecdf(y) )

# look at the QQ plot - it demonstrates how closely the distribution matches the quantiles of a Normal distribution.
qqnorm(y)


## -------------------------------------------------------------------------------------------------------
# import the fosdata package and the mice_pot data
library( 'fosdata' )
data( mice_pot )

# extract just the mice that got the medium dose of THC
mice_med = mice_pot[ mice_pot$group == 1, ]

# look at the histogram and QQ plot to assess whether the data are approximately normal
hist( mice_med$percent_of_act )
qqnorm( mice_med$percent_of_act )


## -------------------------------------------------------------------------------------------------------
# calculate the sample mean, sample standard deviation, and sample size:
x_bar = mean( mice_med$percent_of_act )
s = sd( mice_med$percent_of_act )
n = nrow( mice_med )

# calculate the relevant quantiles of the t-distribution.
# use the 0.1 and 0.9 quantiles because we want the 80% confidence interval
# and (1-0.8) / 2 = 0.1 and 1 - 0.1 = 0.9.
t_low = qt(0.1, df=n-1)
t_high = qt(0.9, df=n-1)

# calculate the confidence interval:
cat( "The 80% CI is (", x_bar + t_low * s / sqrt(n), ", ", x_bar + t_high * s / sqrt(n), ").")

# check your calculation with the t.test function:
t.test( mice_med$percent_of_act, conf.level=0.8 )



## -------------------------------------------------------------------------------------------------------
# import the fosdata package and the barnacles data
library( 'fosdata' )
data( barnacles )

# calculate the number of barnacles per unit area for each sample:
barnacles$per_m = barnacles$count / barnacles$area_m

# examine the histogram and QQ plot of the barnacles per unit area:
hist( barnacles$per_m, main="barnacles per square meter" )
qqnorm( barnacles$per_m )

# does it look to you like the barnacles per unit area are distributed like a normal distribution?


## -------------------------------------------------------------------------------------------------------
# define the sample size and a 
B = 200
t_boot = numeric( B )

for (i in 1:B) {
  z = sample( barnacles$per_m, replace=TRUE )
  t_boot[[i]] = ( mean(z) - mean(barnacles$per_m) ) / ( sd(z) / sqrt(length(z)) )
}

hist(t_boot)

