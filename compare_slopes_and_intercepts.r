#######################################################################################
##### Comparison of Slope and Intercept Terms for Multi-Level Model
#######################################################################################

library(lattice)

# make some fake data:
x <- rnorm(100, mean=3, sd=6)
y <- x * runif(100, min=1, max=7) + runif(100, min=1.8, max=5)
d <- data.frame(x, y, f=rep(letters[1:10], each=10))

xyplot(y ~ x | f, data=d, type=c('p','r'))

# split by factor
d.l <- split(d, d$f)
# fit model for each level of factor
fits <- lapply(d.l, function(d_i) {lm(y ~ x, data=d_i)})

# extract coefs
est <- lapply(fits, coef)

# compute confints
ci <- lapply(fits, confint)

ci.mat <- do.call('rbind', ci)
est.mat <- do.call('rbind', est)
ci.df <- data.frame(f=rep(colnames(sapply(ci, '[')), each=2))
ci.df$lower <- ci.mat[,1] 
ci.df$upper <- ci.mat[,2]

# re-attach estimate label
ci.df$which <- row.names(ci.mat)

# add dummy column for estimate
ci.df$estimate <- NA

# make a data frame for the estimates
est.df <- data.frame(which=rep(colnames(est.mat), each=nrow(est.mat)))
est.df$estimate <- as.vector(c(est.mat[,1], est.mat[,2]))
est.df$f <- rep(row.names(est.mat), 2)

# add dummy columns for upper and lower conf ints
est.df$upper <- NA
est.df$lower <- NA

# combine estimate with confints
combined <- rbind(est.df, ci.df)

# combined plot of estimate +/- confint
dotplot(f ~ estimate + lower + upper | which, data=combined, scales=list(relation='free'), xlab="Estimate", ylab="Group", auto.key=list(columns=3),
        par.settings=list(superpose.symbol=list(col=c(1), pch=c(16,1,1), cex=c(1,0.75,0.75))))

#######################################################################################
##### Comparison of Slope and Intercept Terms for Multi-Level Model II: Using Contrasts
#######################################################################################

# need these
library(lattice)

##### example of multi-level data

# replicate an important experimental dataset
set.seed(10101010)
x <- rnorm(100)
y1 <- x[1:25] * 2 + rnorm(25, mean=1)
y2 <- x[26:50] * 2.6 + rnorm(25, mean=1.5)
y3 <- x[51:75] * 2.9 + rnorm(25, mean=5)
y4 <- x[76:100] * 3.5 + rnorm(25, mean=5.5)
d <- data.frame(x=x, y=c(y1,y2,y3,y4), f=factor(rep(letters[1:4], each=25)))

# plot
xyplot(y ~ x, groups=f, data=d, 
       auto.key=list(columns=4, title='Beard Type', lines=TRUE, points=FALSE, cex=0.75), 
       type=c('p','r'), ylab='Number of Pirates', xlab='Distance from Land')

# standard comparison to base level of f
summary(lm(y ~ x * f, data=d))

# compare to level 4 of f
summary(lm(y ~ x * C(f, base=4), data=d))

# need these
library(multcomp)
library(sandwich)

# open this vignette, lots of good information
vignette("generalsiminf", package = "multcomp")

# fit two models
l.1 <- lm(y ~ x + f, data=d)
l.2 <- lm(y ~ x * f, data=d)

# note that: tests are AGAINST the null hypothesis
summary(glht(l.1))

# see the plotting methods:
plot(glht(l.1))
plot(glht(l.2))

# pair-wise comparisons
summary(glht(l.1, linfct=mcp(f='Tukey')))

# pair-wise comparisons
# may not be appropriate for model with interaction
summary(glht(l.2, linfct=mcp(f='Tukey')))

# when variance is not homogenous between groups:
summary(glht(l.1, linfct=mcp(f='Tukey'), vcov=sandwich))