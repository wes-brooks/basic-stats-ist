## -------------------------------------------------------------------------------------------------------
install.packages("remotes")
remotes::install_github("speegled/fosdata")
data( package="fosdata" )
library( "fosdata" )
data( wrist )
View( wrist)
?wrist

## -------------------------------------------------------------------------------------------------------
# X is a random variable, it follows the sandard normal distribution
X = rnorm(1)
?rnorm
X = rnorm(10)

# Once we have "realized" a value of X, it is data and no longer random.
X


## -------------------------------------------------------------------------------------------------------
mean(X)
median(X)
sd(X)
var(X)


## -------------------------------------------------------------------------------------------------------
# Demonstrate the standard normal distribution function:
pnorm(0)

t = seq(-4, 4, length=1001)
plot(x=t, y=pnorm(t), bty='n', type='l', main="standard normal distribution function")

# draw the standard normal distribution function, with some annotations
plot( x=t, y=dnorm(t), bty='n', type='l', main="standard normal density function")
polygon(x=c(t[t<=qnorm(0.025)], qnorm(0.025), min(t)), y=c(dnorm(t[t<=qnorm(0.025)]), 0, 0), col=grey(0.8))

polygon(x=c(t[t>=qnorm(0.975)], max(t), qnorm(0.975)), y=c(dnorm(t[t>=qnorm(0.975)]), 0, 0), col=grey(0.8))
# annotate the plot with lines indicating the quantiles for a 95% probability interval


## -------------------------------------------------------------------------------------------------------
plot( x=t, y=dunif(t), type='l', bty='n', main="Uniform(0,1) probability density")
runif(10)

## -------------------------------------------------------------------------------------------------------
# Demonstrate the standard normal density function:


# plot the standard normal density function with some annotations



# annotate the density function with the 5% probability mass tails



## -------------------------------------------------------------------------------------------------------
# plot the poisson and chi-squared probability densities
plot(x=t, y=dchisq(t, df=1), bty='n', type='l', main="chi-squared (df=1) probability density")


## -------------------------------------------------------------------------------------------------------
#2:40 demonstrate quantiles of the standard normal distribution
qnorm(0.025)

# if you want an interval that contains the random variable 95% of the time, you may divide the remaining 5% in half, so the interval is in the center:
qnorm(c( 0.05/2, 1-0.05/2))



##2:44-2:47 break -------------------------------------------------------------------------------------------------------



## -------------------------------------------------------------------------------------------------------
Y = function(n=10, mean=10, sd=3) { rnorm(n, mean, sd) }
X = function(n=10, mean=-3, sd=2) { rnorm(n, mean, sd) }

mean(Y())
mean(X())

mean( 0.5* X() + Y())

var( 0.5*X() + Y())


#2:55
nn = c(1, 2, 4, 8, 12, 20, 33, 45, 60, 100)
means = sapply(nn, function(n) { mean( rnorm(n))})
plot(nn, means, bty='n', main="sample mean")
abline(h=0, lty=3)

## 2:59-------------------------------------------------------------------------------------------------------
# plot the densities of a standard normal distribution, and of a t-distribution with five degrees of freedom:




## -------------------------------------------------------------------------------------------------------
# generate 20 samples from a uniform distribution and plot their histogram
N = 20
u = runif( N )
hist(u)

# generate 100 repeated samples of the same size, calculate the mean of each one, and plot the histogram of the means.
B = 100
uu = numeric( B )
for (i in 1:B) {
  uu[[ i ]] = mean( runif( N ) )
}
hist(uu)

# what happens as B and N get larger and smaller? Do they play different roles?


##3:03 -------------------------------------------------------------------------------------------------------


#break 3:03-3:09 sample 50 numbers from the standard normal distribution
y = rnorm(50)


# look at the histogram - it's like the density function
hist(y)

# look at the empirical distribution function - it's like the distribution function
plot( ecdf(y) )

# look at the QQ plot - it demonstrates how closely the distribution matches the quantiles of a Normal distribution.
qqnorm(y)

#3:17
y = runif(50)
qqnorm(y)

y = rchisq(50, df=2)
qqnorm(y)

## -------------------------------------------------------------------------------------------------------
# import the fosdata package and the mice_pot data
library( 'fosdata' )
data( mice_pot )
View(mice_pot )


# extract just the mice that got the medium dose of THC
mice_med = mice_pot[ mice_pot$group == 1, ]
hist(mice_med$percent_of_act)
qqnorm(mice_med$percent_of_act)

# look at the histogram and QQ plot to assess whether the data are approximately normal

## -------------------------------------------------------------------------------------------------------


# calculate the relevant quantiles of the t-distribution.
# use the 0.1 and 0.9 quantiles because we want the 80% confidence interval
# and (1-0.8) / 2 = 0.1 and 1 - 0.1 = 0.9.
n = nrow( mice_med )
df = n - 1
qt(0.1, df=11)
qt(0.9, df=11)
s = sd( mice_med$percent_of_act )

# calculate the confidence interval:
mean( mice_med$percent_of_act ) + qt(c(0.1, 0.9), df=11) * s / sqrt(n)

# check your calculation with the t.test function:
t.test(mice_med$percent_of_act, conf.level = 0.8)


## -------------------------------------------------------------------------------------------------------
#3:31 import the fosdata package and the barnacles data
data(barnacles)
View(barnacles)
?barnacles

# calculate the number of barnacles per unit area for each sample:
barnacles$per_m = barnacles$count / barnacles$area_m

# examine the histogram and QQ plot of the barnacles per unit area:
hist(barnacles$per_m)
qqnorm(barnacles$per_m)

# does it look to you like the barnacles per unit area are distributed like a normal distribution?


## -------------------------------------------------------------------------------------------------------
# define the sample size and a 
B = 200
t_boot = numeric(B)

for (i in 1:B) {
  z = sample( barnacles$per_m, replace=TRUE )
  t_boot[[i]] = (mean(z) - mean(barnacles$per_m)) / (sd(z) / sqrt(length(z)) )
}

hist(t_boot)

mean( barnacles$per_m ) + quantile(t_boot, c(0.1, 0.9)) * sd(barnacles$per_m) / sqrt(nrow(barnacles))

