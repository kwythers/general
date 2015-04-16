## Load some libraries
library(data.table)
library(ggplot2)
library(AICcmodavg)
library(plyr)
library(stringr)
library(rgp)

## Read in the data as data.tables
# dt.m <- fread("~/Desktop/2772/measured_ldc_s002-320_south_branch.csv")
# dt.t <- fread("~/Desktop/2772/target_ldc_s002-320_south_branch.csv")

## Working with the target data first, curve fitting
# ggplot(dt.t, aes(x = Flow, y = Load)) +
#   geom_point(shape = 1)      # Use hollow circle

# read in the data old school 
df <- read.table(text= "x y 
0.0003  4332.41 
0.001  3119.21 
0.0027	2451.7 
0.01	1588.18 
0.05	666.25 
0.1	398.42 
0.15	262.21  
0.2	176.66 
0.25  121.81  
0.3	93.45 
0.35	73.92 
0.4	58.47 
0.45	42.41 
0.5	31.37 
0.55	22.35 
0.6	16.09 
0.65	10.26 
0.7	6.47 
0.75	3.48 
0.8	1.14 
0.85	0.39 
0.9	0.21 
0.95	0.12 
0.99	0.02", header = TRUE)

## Quick look at the data
ggplot(df, aes(x, y)) +
  geom_point(shape = 1) +
  xlab("Flow") +
  ylab("Load")

# a list of possible models...
models <- list(lm(y ~ x, data = df), 
               lm(y ~ I(1/x), data = df),
               lm(y ~ log(x), data = df),
               nls(y ~ I(1/x*a) + b*x, data = df, start = list(a = 1, b = 1)), 
               nls(y ~ (a + b*log(x)), data = df, start = setNames(coef(lm(y ~ log(x), data = df)), c("a", "b"))),
#                nls(y ~ I(exp(1)^(a + b * x)), data = df, start = list(a = 0, b = 0)), # producing an infinity???
               nls(y ~ I(1/x*a)+b, data = df, start = list(a = 1,b = 1))
)

# have a quick look at the visual fit of these models 
ggplot(df, aes(x, y)) + geom_point(shape = 1) +
  stat_smooth(method = "lm", formula = as.formula(models[[1]]), size = 1, se = FALSE, colour = "black") + 
  stat_smooth(method = "lm", formula = as.formula(models[[2]]), size = 1, se = FALSE, colour = "blue") + 
  stat_smooth(method = "lm", formula = as.formula(models[[3]]), size = 1, se = FALSE, colour = "yellow") + 
  stat_smooth(method = "nls", formula = as.formula(models[[4]]), data=df, start = list(a=0,b=0), size = 1, se = FALSE, colour = "red") + 
  stat_smooth(method = "nls", formula = as.formula(models[[5]]), data=df, start = setNames(coef(lm(y ~ log(x), data=df)), c("a", "b")), size = 1, se = FALSE, colour = "green") +
  stat_smooth(method = "nls", formula = as.formula(models[[6]]), data=df, start = list(a=0,b=0), size = 1, se = FALSE, colour = "violet") 
#   stat_smooth(method = "nls", formula = as.formula(models[[7]]), data=df, start = list(a=0,b=0), size = 1, se = FALSE, colour = "orange")

# calculate the AIC and AICc (for small samples) for each 
# model to see which one is best, ie has the lowest AIC
ldply(models, function(mod){ data.frame(AICc = AICc(mod), AIC = AIC(mod), model = deparse(formula(mod))) })

# # symbolic regression using Genetic Programming
# # http://rsymbolic.org/projects/rgp/wiki/Symbolic_Regression
# result1 <- symbolicRegression(y ~ x, 
#                               data=df, functionSet=mathFunctionSet,
#                               stopCondition=makeStepsStopCondition(2000))
# # inspect results, they'll be different every time...
# (symbreg <- result1$population[[which.min(sapply(result1$population, result1$fitnessFunction))]])
# 
# function (x) 
#   tan((x - x + tan(x)) * x) 
# # quite bizarre...
# 
# # inspect visual fit
# ggplot() + geom_point(data=dat, aes(x,y), size = 3) +
#   geom_line(data=data.frame(symbx=dat$x, symby=sapply(dat$x, symbreg)), aes(symbx, symby), colour = "red")
