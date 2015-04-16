##### read test #####

system.time(
  read.csv("~/Desktop/user_2012-2013_hourly.csv")
)
# user  system elapsed 
# 212.205   3.967 217.270 


system.time(
  fread("~/Desktop/user_2012-2013_hourly.csv")
)
# Read 1919424 rows and 62 (of 62) columns from 0.864 GB file in 00:00:11
# user  system elapsed 
# 9.332   0.805  11.392 