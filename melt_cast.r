###############################
##### Melt and Cast data ######
###############################

require(reshape2)

# load data
# dt <- data.table(id = c("a", "a", "b", "b"), time = c(1, 2, 1, 2), var1 = c(5, 3, 6, 2), var2 = c(600, 500, 100, 400))
# dt <- read.csv("PATH_TO_DATA")

# look at the data structure
dim(dt)
dt
sum(is.na(dt))

# melt example
key <- c("id", "time") # columns not to stack
mdt <- melt(dt, key)

# cast back to wide format
cdt <- dcast(mdt, id + time ~ variable)

# cast back to wide format calculating means for particualr columns
idmeans <- dcast(mdt, id ~ variable, mean) # mean of ids
timemeans <- dcast(mdt, time ~ variable, mean)

