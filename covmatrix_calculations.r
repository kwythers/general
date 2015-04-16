# Emily Peters
# 5/6/2013
# Creates climate covariance matrices by month for 4 sites (Detroit Lakes, Kapuskasing, Mt. Pleasant, Souix Lookout)
# For climate variability project.

# Detroit Lakes
dl <- read.csv('/Volumes/modeling/pnet_dev/matlab/input/detroitlakes_monthly.dat')
for(i in 1:12) {
	dl_i = subset(dl,month==i,select=c('tmax','tmin','par','precip'))
	dlcovmatrix_i = cov(dl_i,dl_i)
	file_i = paste("/Volumes/modeling/pnet_dev/matlab/input/dlcovmatrix_",i,".txt",sep="")
	write.table(dlcovmatrix_i,file=file_i,row.names=FALSE,col.names=FALSE)	
}


# Kapuskasing
kapuskasing <- read.csv('/Volumes/modeling/pnet_dev/matlab/input/kapuskasing_monthly.dat')
names(kapuskasing) <- c("year","month","rows","tmax","tmin","par","precip")
for(i in 1:12) {
	kapuskasing_i = subset(kapuskasing,month==i,select=c('tmax','tmin','par','precip'))
	kapuskasingcovmatrix_i = cov(kapuskasing_i,kapuskasing_i)
	file_i = paste("/Volumes/modeling/pnet_dev/matlab/input/kapuskasingcovmatrix_",i,".txt",sep="")
	write.table(kapuskasingcovmatrix_i,file=file_i,row.names=FALSE,col.names=FALSE)	
}


# mtpleasant
mtpleasant <- read.csv('/Volumes/modeling/pnet_dev/matlab/input/mtpleasant_monthly.dat')
for(i in 1:12) {
	mtpleasant_i = subset(mtpleasant,month==i,select=c('tmax','tmin','par','precip'))
	mtpleasantcovmatrix_i = cov(mtpleasant_i,mtpleasant_i)
	file_i = paste("/Volumes/modeling/pnet_dev/matlab/input/mtpleasantcovmatrix_",i,".txt",sep="")
	write.table(mtpleasantcovmatrix_i,file=file_i,row.names=FALSE,col.names=FALSE)	
}

# siouxlookout
siouxlookout <- read.csv('/Volumes/modeling/pnet_dev/matlab/input/siouxlookout_monthly.dat')
names(siouxlookout) <- c("year","month","rows","tmax","tmin","par","precip")
for(i in 1:12) {
	siouxlookout_i = subset(siouxlookout,month==i,select=c('tmax','tmin','par','precip'))
	siouxlookoutcovmatrix_i = cov(siouxlookout_i,siouxlookout_i)
	file_i = paste("/Volumes/modeling/pnet_dev/matlab/input/siouxlookoutcovmatrix_",i,".txt",sep="")
	write.table(siouxlookoutcovmatrix_i,file=file_i,row.names=FALSE,col.names=FALSE)	
}
