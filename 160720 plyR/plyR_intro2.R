# plyr introduction of interesting functions from examples
# peter braubach
# comments by sarah pohl
# 2016-07-20

#### load libraries ####
library(plyr)


#### colwise ####
# create dataframe with random normally distributed, uniformly distributed and
# binomial data.
dta <- data.frame(x = rnorm(10, 1, 1),
                  y = runif(10, 0, 1),
                  z = rbinom(10, 1, 0.5))
dta

# create a function that calculates the mean of every column in a dataframe
colMean <- colwise(mean)
colMean(dta)

#### each ####
# create example data vector
x <- rnorm(10, 0, 1)

# each() combines multiple functions into a single function call.
meanSd <- each(mean, sd)
meanSd(x)

# This can also be supplied to colwise().
colMeanSd <- colwise(meanSd)
colMeanSd(dta)  # does not preserve labels


#### mutate ####
# mutate() can transform data in a given column.
dta <- mutate(dta, x_scale = scale(x))

# doing it manually
dta$x2 <- dta$x / max(dta$x)
# using mutate()
dta <- mutate(dta, x3 = x / max(x))

# This uses data from plyR_intro1.R!
# find the maximum x value per experiment
d2 <- ddply(d, .(experiment), mutate, xmax = max(x))


#### load other exmaple data ####
data(mtcars)
mtcars


#### arrange and name_rows ####
# order data by certain columns
mtsort <- arrange(mtcars, cyl, mpg) # does not keep rownames
mtsort

# keep the rownames
# name_rows() adds a new column with the rownames.
mycars <- name_rows(mtcars)
mycars
mycarssort <- arrange(mycars, cyl, mpg)
mycarssort
# name_rows() can also remove that column and create implicit rownames again.
mycarssort <-name_rows(mycarssort)

# sort in partly descending order
arrange(mycars, desc(cyl), mpg)


#### summarise ####
# summarise() is similar to mutate(), but creates a new dataframe.
summarise(dta, max(x), max(y))

# find the maximum mpg and hp values for each cyl group
ddply(mtcars, .(cyl), summarize, max_mpg=max(mpg), max_hp=max(hp))

# This uses data from plyR_intro1.R!
# calculate the mean x value per experiment
ddply(d, .(experiment), summarise, meanx = mean(x))