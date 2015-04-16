# get a list of files
theFiles <- dir("~/r/data", pattern = "\\.csv")

# loop through the files
for(a in theFiles) 
  {
    # build name and assign it to the data
    nameToUse <- str_sub(string = a, start = 12, end = 18)
    # read in the csv using read.table
    # file.path is a handy way to specity a folder or file
    temp <- read.table(file = file.path("~/r/data", a), header = TRUE, sep = ",", stringsAsFactors = FALSE)
    # assign them the workspace
    assign(x = nameToUse, value = temp)
  }

# figure out the names of the data.frames
frameNames <- str_sub(string = theFiles, start = 12, end = 18)
# build an empty list
frameList <- vector("list", length(frameNames))
names(frameList) <- frameNames
# add each data.frame into the list
for (a in frameNames)
{
  frameList[[a]] <- eval(parse(text = a))
}