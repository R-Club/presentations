# plyr introduction with use cases
# peter braubach
# comments by sarah pohl
# 2016-07-20

#### load libraries ####
library(ggplot2)
library(plyr)

#### create some random data ####
# The dot in front of the variable name hides the variable from the global
# environment.
.nexperiment <- 10
.nsamples    <- 10
.nreplicates <- 3

d <- data.frame(
  # create experiment names
  experiment = rep(sprintf("E%02d", 1:.nexperiment), each = .nsamples * .nreplicates),
  # create sample numbering
  sample = rep(rep(1:.nsamples, each = .nreplicates), .nexperiment),
  # create replicate numbering
  replicate = rep(1:.nreplicates, .nexperiment * .nsamples * .nreplicates))

# create random, normally distributed data x and y
d$x <- d$sample + rnorm(nrow(d), 1, 1)
d$y <- rnorm(nrow(d), d$x, 1)

# turn sample and replicate numbers into factors
d$sample <- factor(d$sample)
d$replicate <- factor(d$replicate)


#### view data ####
head(d)
# plot data by replicates and experiments
# colour values by sample
ggplot(d, aes(x=x, y=y, col=sample)) + geom_point() + facet_grid(experiment ~ replicate)


#### use ddply ####
# plyr contains multiple functions, all following the scheme
# InputFormat + OutputFormat + ply
# ddply() therefore expects a dataframe and will also return a dataframe.

# aggregate by replicate and calculate sample mean
d2 <- ddply(d[,-3], .(experiment, sample), colwise(mean))

# plot the means by experiment
ggplot(d2, aes(x=x, y=y)) + geom_point() + facet_wrap(~ experiment)


#### use dlply and ldply####
# dlply() expects a dataframe and returns a list.
# ldply() expects a list and returns a dataframe.

# create linear models for each experiment using lm()
models <- dlply(d, .(experiment), function(x) lm(x$y ~ x$x))
models

# extract the model coefficients
mcoefs <- ldply(models, .fun = coef)