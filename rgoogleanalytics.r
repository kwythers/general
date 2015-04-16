##### RGoogleAnalytics example #####

require("RCurl")
require("rjson")
require("ggplot2")
require("plyr")
require("gridExtra")
require("reshape")
require("RGoogleAnalytics")

##### Authorize your account and paste the accesstoken 
query <- QueryBuilder()
access_token <- query$authorize()

##### Initialize the configuration object - execute one line at a time
conf <- Configuration()

ga.account <- conf$GetAccounts() 
ga.account

## If you have many accounts, you might want to add "ga.account$id[index]"
## without the "" inside the ( ) below to list only the web properties inside 
## a specific account. 

ga.webProperty <- conf$GetWebProperty(ga.account$id[index])
ga.webProperty