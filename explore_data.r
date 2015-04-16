##### get information about the data
str(dt)

##### add a column to data table
dt$column2add <- NA

##### delete column from data table
dt$column2remove <- NULL

##### rename columns
names(dt) <- c("col1", "col2", "col3")

##### renmae by column name
names(dt)[names(dt) == "col1"] <- c("column1")

##### rename by position
names(dt)[1] <- "column1"

##### reorder columns by numeric position
dt <- dt[c(1, 3, 2)]

##### reorder columns by name
dt <- dt[c("col1", "col3", "col2")]

##### subsetting a data frame
# > head(climate, 5)
# Source Year Anomaly1y Anomaly5y Anomaly10y Unc10y
# 1 Berkeley 1800        NA        NA     -0.435  0.505
# 2 Berkeley 1801        NA        NA     -0.453  0.493
# 3 Berkeley 1802        NA        NA     -0.460  0.486
# 4 Berkeley 1803        NA        NA     -0.493  0.489
# 5 Berkeley 1804        NA        NA     -0.536  0.483

# grab all rows with Berkeley and the columns Year and Anomaly10y
subset(climate, Source == "Berkeley", select = c(Year, Anomaly10y))

# grab Berkeley rows from years between 1900 and 2000
subset(climate, Source == "Berkeley" & Year >= 1900 & Year <= 2000, select = c(Year, Anomaly10y))

##### change the order of factors (usefull to change graphing order)
# str(iss)
# 'data.frame':  72 obs. of  2 variables:
#   $ count: num  10 7 20 14 14 12 10 23 17 20 ...
# $ spray: Factor w/ 6 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...

iss2 <- reorder(iss$spray, iss$cout, FUN = mean) # new order by mean

# str(iss2)
# Factor w/ 6 levels "C","E","D","A",..: 4 4 4 4 4 4 4 4 4 4 ...
# - attr(*, "scores")= num [1:6(1d)] 14.5 15.33 2.08 4.92 3.5 ...
# ..- attr(*, "dimnames")=List of 1
# .. ..$ : chr [1:6] "A" "B" "C" "D" ...

##### rename factors, use revalue() or mapvalues() from plyr
sizes <- factor(c("small", "medium", "large", "large", "small"))
sizes1 <- revalue(sizes, c(small = "s", medium = "m", large = "l"))

##### summerizing and transforming data by groups, use ddply() from plyr
# head(cabbages)
# Cult Date HeadWt VitC
# 1  c39  d16    2.5   51
# 2  c39  d16    2.2   55
# 3  c39  d16    3.1   45
# 4  c39  d16    4.3   42
# 5  c39  d16    2.5   53
# 6  c39  d16    4.3   50

# summary(cabbages)
# Cult     Date        HeadWt           VitC      
# c39:30   d16:20   Min.   :1.000   Min.   :41.00  
# c52:30   d20:20   1st Qu.:1.875   1st Qu.:50.75  
# d21:20   Median :2.550   Median :56.00  
# Mean   :2.593   Mean   :57.95  
# 3rd Qu.:3.125   3rd Qu.:66.25  
# Max.   :4.300   Max.   :84.00  

# summary by date
ddply(cabbages, c("Cult", "Date"), summarize, Weight = mean(HeadWt), VitC = mean(VitC))
# Cult Date Weight VitC
# 1  c39  d16   3.18 50.3
# 2  c39  d20   2.80 49.4
# 3  c39  d21   2.74 54.8
# 4  c52  d16   2.26 62.5
# 5  c52  d20   3.11 58.9
# 6  c52  d21   1.47 71.8

# summary by cultivar
ddply(cabbages, "Cult", summarize, Wieght = mean(HeadWt))
# Cult   Wieght
# 1  c39 2.906667
# 2  c52 2.280000

# summery with standard errors and confidence intervals, add na.rm = TRUE to deal with NAs
ddply(cabbages, c("Cult", "Date"), 
      summarize, Weight = mean(HeadWt, na.rm = T), 
      sd = sd(HeadWt, na.rm = T), 
      n = sum(!is.na(HeadWt)), 
      se = sd / sqrt(n))
# Cult Date Weight        sd  n         se
# 1  c39  d16   3.18 0.9566144 10 0.30250803
# 2  c39  d20   2.80 0.2788867 10 0.08819171
# 3  c39  d21   2.74 0.9834181 10 0.31098410
# 4  c52  d16   2.26 0.4452215 10 0.14079141
# 5  c52  d20   3.11 0.7908505 10 0.25008887
# 6  c52  d21   1.47 0.2110819 10 0.06674995

# find head weight difference from the mean of head weight
transform(cabbages, DevWt = HeadWt - mean(HeadWt))
# Cult Date HeadWt VitC        DevWt
# 1   c39  d16    2.5   51 -0.093333333
# 2   c39  d16    2.2   55 -0.393333333
# 3   c39  d16    3.1   45  0.506666667
# 4   c39  d16    4.3   42  1.706666667
# 5   c39  d16    2.5   53 -0.093333333

# find head weight diffence from the mean by group (in this case caltivar)
ddply(cabbages, "Cult", transform, DevWt = HeadWt - mean(HeadWt))

