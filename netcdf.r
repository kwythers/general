############################################################
#### Reading and working with a NetCDF data set         ####
#### Kirk R. Wythers, August, 23 2014                   ####
############################################################

# load libraries
library(chron)
library(data.table)
library(lattice)
library(ncdf)
library(ncdf.tools)
library(ncdf4)
library(ncdf4.helpers)
library(RColorBrewer)
library(raster)
library(reshape2)
library(rgdal)
library(sp)

# set working directory
getwd()
workdir <- "~/r/"
setwd(workdir)

# make sure needed data files are in your working directory
list.files(workdir)

# read in netcdf files and vairiable names
ncname <- "cru_ts3.22.1901.2013.tmp.dat.nc"
#ncfinalname <- paste(ncname, ".nc", sep = "") # comment out if file name incules .nc
ncfinalname <- ncname # comment out if file name does not include .nc
dname <- "tmp"  # note: tmp means temperature (not temporary)

# open a NetCDF file and print information about the file
ncin <- open.ncdf(ncfinalname)
print(ncin)

# get the longtiudes with the get.var.ncdf() function
lon <- get.var.ncdf(ncin, "lon")
nlon <- dim(lon)
head(lon, 10) # peak at the variable
lat <- get.var.ncdf(ncin, "lat", verbose = F)
nlat <- dim(lat)
head(lat, 10) # peak at the variable

# print the dimensions of lon lat
print(c(nlon, nlat))

# get the time variable and attributes using get.var.ncdf() 
# and att.get.ncdf(), also the number of times with dim()
t <- get.var.ncdf(ncin, "time")
tunits <- att.get.ncdf(ncin, "time", "units")
tunits # print the time units string
t  # list the values
nt <- dim(t)  # get the number of values
nt # list the values

# read into array, check the variable, it's attributes, and verify the size of the array
tmp.array <- get.var.ncdf(ncin, dname)
dlname <- att.get.ncdf(ncin, dname, "long_name")
dunits <- att.get.ncdf(ncin, dname, "units")
fillvalue <- att.get.ncdf(ncin, dname, "_FillValue")
missingvalue <- att.get.ncdf(ncin, dname, "missing_value") # get the missing values (in case they are different from fills)
dim(tmp.array)

# create a raster object
tmp.raster <- raster(ncfinalname)
dim(tmp.raster)

# get the global attributes
title <- att.get.ncdf(ncin, 0, "title")
institution <- att.get.ncdf(ncin, 0, "institution")
datasource <- att.get.ncdf(ncin, 0, "source")
references <- att.get.ncdf(ncin, 0, "references")
history <- att.get.ncdf(ncin, 0, "history")
Conventions <- att.get.ncdf(ncin, 0, "Conventions")

# check attribute values 
title$value 
institution$value
datasource$value
references$value
history$value
Conventions$value

# close the NetCDF file
close.ncdf(ncin)

# split the time units string into fields, convert to "real" and use chron() to get abs values
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(t, origin = c(tmonth, tday, tyear))

# check fill and missingvalues then replace netcdf fillvalues with NAs
length(which(tmp.array == fillvalue)) # count fillvalues
length(which(tmp.array == missingvalue)) # count missingvalues

tmp.array[tmp.array == fillvalue] <- NA
tmp.array[tmp.array == missingvalue] <- NA
length(which(tmp.array == fillvalue$value)) #  # confirm NA replacement, count fillvalues
length(which(tmp.array == missingvalue$value)) # confirm NA replacement, count missingvalues 

# determin length of a single "data slice" 
length(na.omit(as.vector(tmp.array[, , 1])))

# index into the 3rd dimension to get a single temporal slice of the data, 
# create an R data frame, and write a .csv file for further anaysis and visualization
# m is "month" 1 == first month of data, 1356 == last month
# confirm 720 x 360 with dim()
m <- 1
tmp.slice <- tmp.array[, , m]
dim(tmp.slice)

# quick map of the extracted slice with image()
image(lon, lat, tmp.slice, col = rev(brewer.pal(11, "RdBu")))

# make better looking image using levelplot() and defining cutpts... 
# need to find min max of the data to make a good guess for cutpts
range(tmp.slice, na.rm = T)
# set cutpts
grid <- expand.grid(lon = lon, lat = lat)
#cutpts <- c(-60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40)
cutpts <- seq(-60, 40, by = 10)
levelplot(tmp.slice ~ lon * lat, data = grid, at = cutpts, cuts = 11, 
          pretty = T, col.regions = (rev(brewer.pal(11, "RdBu"))))

# slice and dice into time periods of interest
# for example if the array covers the time period 1900 - 2012,
# then grabbing slices 1345:1356 would cover all 12 months of
# the year 2013 globally, likewise slices 1:12 would cover Jan - Dec of
# the year 1900 
tmp2013.array <- tmp.array[,,1345:1356]
tmp1900.array <- tmp.array[,,1:12]

# create data frame with expand.grid() and "unstack" with as.vector()
lonlat <- expand.grid(lon, lat)
tmp.vec <- as.vector(tmp.slice)
length(tmp.vec)

# use data.frame() and cbind() functions to assemble the columns of the data frame 
# create column names with the names() function use the "m" variable to identy month
# use head() function with the na.omit() function to list the first rows of values without NAs
tmp.df01 <- data.frame(cbind(lonlat, tmp.vec))
names(tmp.df01) <- c("lon", "lat", paste(dname, as.character(m), sep = "_"))
head(na.omit(tmp.df01), 20)

# write the data frame to the working directory as a .csv file, using na.omit() to drop 
# the observations with missing data (i.e. ocean points and Antarctica)
csvfile <- "cru_tmp_1.csv"
write.table(na.omit(tmp.df01), csvfile, row.names = FALSE, sep = ",")

# convert the whole array to a data frame, in order to calculate MTWA, MTCO and MAT
# start by converting the array into a vector
tmp.vec.long <- as.vector(tmp.array)
length(tmp.vec.long)

# reshape that vector into a 259200 by number of months matrix using the matrix() function, 
# and verify its dimensions, which should be 259200 by nt months
tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
dim(tmp.mat)

# check data with the head() function
head(na.omit(tmp.mat))

# create the second data frame from the tmp.mat matrix, and use c(), rep() and names() 
# to name each years months
lonlat <- expand.grid(lon, lat)
tmp.df02 <- data.frame(cbind(lonlat, tmp.mat))
names(tmp.df02) <- c("lon", "lat", rep(c("tmpJan", "tmpFeb", "tmpMar", "tmpApr", "tmpMay", 
                     "tmpJun", "tmpJul", "tmpAug", "tmpSep", "tmpOct", "tmpNov", "tmpDec"), times = 113))

names(tmp.df02) <- c("lon", "lat", rep(c("tmpJan", "tmpFeb", "tmpMar", "tmpApr", "tmpMay", 
                                         "tmpJun", "tmpJul", "tmpAug", "tmpSep", "tmpOct", "tmpNov", "tmpDec"), times = 113))



options(width = 110)
head(na.omit(tmp.df02, 24))

####### think about using a data.table here ######

# tmp.dt01 <- as.data.table(tmp.df02)

# get annual mean, mtwa and mtco values and add them the second data frame
tmp.df02$mtwa <- apply(tmp.df02[3:14], 1, max)  # mean temperature warmest month
tmp.df02$mtco <- apply(tmp.df02[3:14], 1, min)  # mean temperatue coldest month
tmp.df02$mat <- apply(tmp.df02[3:14], 1, mean)  # annual (i.e. row) means
head(na.omit(tmp.df02))

dim(na.omit(tmp.df02))

# write out the second data frame as a .csv file, dropping NAs
csvfile <- "cru_tmp_2.csv"
write.table(na.omit(tmp.df02), csvfile, row.names = FALSE, sep = ",")

# create a third data frame, with non-missing values only 
# to be used later to demonstrate how to convert a “short” data frame into 
# full matrix or array for writing out as a NetCDF file