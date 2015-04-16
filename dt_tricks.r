
##### data table join
dt3 <- dt2[dt1] # join dt3 to dt3, data table in [] is the left side of left join

##### add new column based on conditon from another column
dt4$newcol <- ifelse(dt4$doy >= 161 & dt4$doy <= 279, "TRUE", "FALSE") # data frame way
dt4[,newcol:= as.integer(doy>150 & doy < 280)] # data table way