
####START WITH PRIMARY15 and PRIMARY60 edited 9/2014
#Generate 15 min derived data

#1. create mean based filter
#2. calculate deltas
#3. refold db into stack

#Functions needed for scripts 

### creates standard deviation of trimmed mean
sd.trim <- function(x, trim, na.rm)
{
  if(!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
    warning("argument is not numeric or logical: returning NA")
    return(NA_real_)
  }
  if(na.rm) x <- x[!is.na(x)]
  if(!is.numeric(trim) || length(trim) != 1)
    stop("'trim' must be numeric of length one")
  n <- length(x)
  if(trim > 0 && n > 0) {
     if(is.complex(x)) stop("trimmed sd are not defined for complex data")
     if(trim >= 0.5) return(0)
     lo <- floor(n * trim) + 1
     hi <- n + 1 - lo
     x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]

x<-sd(x)
}}

###trimmed mean to use in data.table

me_trim<-function(x){ mean(x, na.rm=TRUE, trim =.13)}##create meantrim average
me_trim2<-function(x){ mean(x, na.rm=TRUE, trim =.35)}##create meantrim average
mean_rm<-function(x){mean(x, na.rm=TRUE)}##create na.rm max

#packages needed
library(data.table)
library(lubridate)

###Primary 15data
primary_dir<-"/Users/rich0475/Documents/2013_B4W_Test/primary15_2013.csv"
design_table<-"/Users/rich0475/Desktop/2013_test/headers/2013_design_role.csv" ###datatable of experimental design
timedata<-"/Users/rich0475/Documents/2013_B4W_Test/b4w_15_timedata2.csv"
startstop<-"/Users/rich0475/Documents/2013_B4W_Test/B4Wstart_stop.csv"
sec<-"/Users/rich0475/Documents/2013_B4W_Test/primary15c_2013.csv"

###Primary 15data 2013 only
primary_dir<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15_2013.csv"
design_table<-"/Volumes/disk7/b4warmed3/2013_design_role.csv" ###datatable of experimental design
timedata<-"/Volumes/disk7/b4warmed3/b4w_15_timedata2.csv"
startstop<-"/Volumes/disk7/b4warmed3/B4Wstart_stop.csv"
sec1<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15a_2013.csv"
sec2<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15b_2013.csv"
sec3<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15c_2013.csv"
sec4<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15d_2013.csv"
sec5<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15e_2013.csv"
sec6<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15f_2013.csv"
sec7<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15g_2013.csv"
sec8<-"/Volumes/disk7/b4warmed3/export/primary2013/derived60_2013.csv"
sec9<-"/Volumes/disk7/b4warmed3/export/primary2013/derived60_2013_complete.csv"

###Primary 15data combined with 2012 data
primary_dir<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15_2013vcombine.csv"
design_table<-"/Volumes/disk7/b4warmed3/2013_design_role.csv" ###datatable of experimental design
timedata<-"/Volumes/disk7/b4warmed3/b4w_15_timedata2.csv"
startstop<-"/Volumes/disk7/b4warmed3/B4Wstart_stop.csv"
sec<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15v_2013.csv"

sec1<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15av_2013.csv"
sec2<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15bv_2013.csv"
sec3<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15cv_2013.csv"
sec4<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15dv_2013.csv"
sec5<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15ev_2013.csv"
sec6<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15fv_2013.csv"
sec7<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15gv_2013.csv"
sec8<-"/Volumes/disk7/b4warmed3/export/primary2013/derived60v_2013.csv"
sec9<-"/Volumes/disk7/b4warmed3/export/primary2013/derived60v_2013_complete.csv"

sec1<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15av2_2013.csv"
sec2<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15bv2_2013.csv"
sec3<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15cv2_2013.csv"
sec4<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15dv2_2013.csv"
sec5<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15ev2_2013.csv"
sec6<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15fv2_2013.csv"
sec7<-"/Volumes/disk7/b4warmed3/export/primary2013/primary15gv2_2013.csv"
sec8<-"/Volumes/disk7/b4warmed3/export/primary2013/derived60v2_2013.csv"
sec9<-"/Volumes/disk7/b4warmed3/export/primary2013/derived60v2_2013_complete.csv"



##sec= substeps where table is written for ease of script use.

primary60_dir<-"/Volumes/disk7/b4warmed3/export/primary2013/primary60_2013vcombine.csv"
texture<-"/Volumes/disk7/b4warmed3/derived_data/texture.csv"

dt<-fread(primary_dir)
ddd<-function(x){gsub(-7999.000, NA ,x)}
dt2 <- dt[,lapply(.SD,ddd)]
write.table(dt2,sec, append = FALSE, quote = FALSE, sep = ",",
           , na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))
rm(dt2)
dt<-fread(sec) 
dt$plot<-gsub(" ","", dt$plot)
dt$block<-gsub(" ","", dt$block)

###reopen sec and fread again because apply function changes datatable classes to character data.

### Add treatment labels
des<-fread(design_table)
sub1<-c( "site", "canopy", "plot", "warming_treatment" , "treatment_code" ,"treatment_abbr", "water_treatment" )
des1<- unique(subset(des, treatment_abbr=="ac"|treatment_abbr=="dc"| treatment_abbr=="h1"|treatment_abbr=="h2",  select = sub1))
setkey(dt, plot)
setkey(des1, plot)
dt2<-des1[dt]
rm(dt)
rm(des1)
rm(des)

#sub2<-c("site","year")

###add time variables
dtime<-fread(timedata)
sub3<-c("site","time2")
setkeyv(dtime, sub3)
setkeyv(dt2, sub3)
dt3<-dtime[dt2]
dt3[,year:=year(time2)]
rm(dtime)
rm(dt2)

###create power
dst<-fread(startstop)
key<-c("site","year")
setkeyv(dst, key)
setkeyv(dt3, key)
dt2<-dst[dt3]
dt2[, power:= 1]
dt2[doy<start_doy | doy> stop_doy, power:= 0]
rm(dt3)
rm(dst)

#names(dt)
# names(dt)
# [1] "rowid"           "time2"           "timestamp"       "block"           "plot"            "batt_volt_min"  
# [7] "flag1"           "flag2"           "flag3"           "flag4"           "flag5"           "flag6"          
# [13] "flag7"           "flag8"           "flag9"           "flag10"          "flag11"          "flag12"         
# [19] "flag13"          "amb_a_avg"       "ptemp_avg"       "airtemp_avg"     "airtemp_max"     "airtemp_min"    
# [25] "airtc_avg"       "rh"              "vp_avg"          "ws_ms_s_wvt"     "winddir_d1_wvt"  "winddir_sd1_wvt"
# [31] "ws_ms_avg"       "s_all_avgt_avg"  "a_dc"            "b_dif2"          "b_dif3"          "b_dif"          
# [37] "dif_a"           "heat_a_avg"      "meanpin"         "meanpinr"        "mnpin"           "mxpin"          
# [43] "pid_lmt"         "runavga"         "s_dc"            "s_pid_lmt"       "s_scldout"       "samb"           
# [49] "sbtemp"          "scldout"         "tabove"          "targettemp_adj"  "targettemp"      "tmv"            
# [55] "tsoil"           "tsoilr"          "scldsq"          "sscldsq"        

#names(des)
# 1] "variable_name"        "variable_channel"     "cr1000_name_2013"     "var_type"             "cr1000newname2013"   
# [6] "2013 data"            "site"                 "canopy"               "block"                "plot"                
# [11] "measurement_interval" "warming_treatment"    "treatment_code"       "treatment_abbr"       "water_treatment"     
# [16] "block_name"           "variable_id"         

###add full treatment labels to primary



# names(dt3)
 # [1] "site"              "year"              "start_doy"         "stop_doy"          "time2"            
 # [6] "doy"               "hour"              "week"              "month"             "long"             
# [11] "lat"               "s_angle"           "s_elevation"       "daylight"          "plot"             
# [16] "rowid"             "timestamp"         "block"             "batt_volt_min"     "flag1"            
# [21] "flag2"             "flag3"             "flag4"             "flag5"             "flag6"            
# [26] "flag7"             "flag8"             "flag9"             "flag10"            "flag11"           
# [31] "flag12"            "flag13"            "amb_a_avg"         "ptemp_avg"         "airtemp_avg"      
# [36] "airtemp_max"       "airtemp_min"       "airtc_avg"         "rh"                "vp_avg"           
# [41] "ws_ms_s_wvt"       "winddir_d1_wvt"    "winddir_sd1_wvt"   "ws_ms_avg"         "s_all_avgt_avg"   
# [46] "a_dc"              "b_dif2"            "b_dif3"            "b_dif"             "dif_a"            
# [51] "heat_a_avg"        "meanpin"           "meanpinr"          "mnpin"             "mxpin"            
# [56] "pid_lmt"           "runavga"           "s_dc"              "s_pid_lmt"         "s_scldout"        
# [61] "samb"              "sbtemp"            "scldout"           "tabove"            "targettemp_adj"   
# [66] "targettemp"        "tmv"               "tsoil"             "tsoilr"            "scldsq"           
# [71] "sscldsq"           "canopy"            "warming_treatment" "treatment_code"    "treatment_abbr"   
# [76] "water_treatment"  

#sub1<-c( "site", "canopy", "plot", "warming_treatment" , "treatment_code" ,"treatment_abbr", "water_treatment" )
#setkeyv(dt3,sub1)


###variables collected on individual blocks to site level
drh<-dt2[block=="b"|block=="d"|block=="i"| block=="k",]
setkey(drh, site, canopy,time2)
key2<- c("time2","rh","vp_avg","airtc_avg","airtemp_max","airtemp_min","site","canopy")
drh<-subset(drh, select=key2)
setnames(drh, "rh", "d_rh")
setnames(drh, "airtc_avg", "d_airtc")
setnames(drh, "airtemp_min", "d_airtcmin")
setnames(drh, "airtemp_max", "d_airtcmax")
setnames(drh, "vp_avg", "d_vp")

drh<- unique(drh) ##needed to remove double rows
setkey(dt2, site,canopy,time2)
setkey(drh, site,canopy,time2)
dt2<-drh[dt2]
rm(drh)

#WIND
dwind<-dt2[block=="e"|block=="l",]
setkey(dwind, site, time2)
key2<- c("time2", "ws_ms_s_wvt", "winddir_d1_wvt","winddir_sd1_wvt","ws_ms_avg","site")
dwind<-subset(dwind, select=key2)
setnames(dwind, "ws_ms_s_wvt", "d_ws_ms_s_wvt")
setnames(dwind, "winddir_d1_wvt", "d_winddir_d1_wvt")
setnames(dwind, "winddir_sd1_wvt", "d_winddir_sd1_wvt")
setnames(dwind, "ws_ms_avg", "d_ws_ms_avg")

dwind<- unique(dwind) ##needed to remove double rows
setkey(dt2, site,time2)
setkey(dwind, site,time2)
dt2<-dwind[dt2]
rm(dwind)


write.table(dt2,sec1, append = FALSE, quote = FALSE, sep = ",",
           , na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))

# dt2<-dt4
# rm(dt4)
dt3<-dt2
###Filter for 15_minute variables
key<- c("time2","plot","year","month","week","hour","doy","pdy","tsoil","tabove", "targettemp",  "site","canopy","treatment_abbr","water_treatment","daylight","power")
dt2<-subset(dt3, select=key)

# ##change null to NA's
dt2$targettemp<-gsub("NULL",NA ,dt2$targettemp)
dt2$targettemp<-gsub(NaN,NA ,dt2$targettemp)
dt2$targettemp<-as.numeric(dt2$targettemp)

ky=c("time2","site","canopy","water_treatment","treatment_abbr") ####2013 revisions changed for water treatment, now by site

dt2[,d_tsoil_meantrim:= me_trim(tsoil),by = ky]
dt2[,d_tabove_meantrim:= me_trim(tabove),by = ky]
dt2[,d_tsoil_sdtrim:= sd.trim(tsoil,0.13, na.rm = TRUE),by = ky]
dt2[,d_tabove_sdtrim:= sd.trim(tabove,0.13, na.rm = TRUE),by = ky]
dt2[,d_tabove_sdlower:=(-2)*d_tabove_sdtrim+d_tabove_meantrim]
dt2[,d_tabove_sdupper:=(2)*d_tabove_sdtrim+d_tabove_meantrim]
dt2[,d_tsoil_sdlower:=(-2)*d_tsoil_sdtrim+d_tsoil_meantrim]
dt2[,d_tsoil_sdupper:=(2)*d_tsoil_sdtrim+d_tsoil_meantrim]

### addition for IRPS
dt2[,d_targettemp_meantrim:= me_trim(targettemp),by = ky]
dt2[,d_targettemp_sdtrim:= sd.trim(targettemp,0.13, na.rm = TRUE),by = ky]
dt2[,d_targettemp_sdlower:=(-2)*d_targettemp_sdtrim+d_targettemp_meantrim]
dt2[,d_targettemp_sdupper:=(2)*d_targettemp_sdtrim+d_targettemp_meantrim]

dt2$d_targettemp_sc<-dt2$targettemp
is.na(dt2$d_targettemp_sc)<-is.na(dt2$d_targettemp_sc)
dt2$d_targettemp_sc<-ifelse(dt2$d_targettemp_sc<dt2$d_targettemp_sdlower | dt2$d_targettemp_sc>dt2$d_targettemp_sdupper, NA, dt2$d_targettemp_sc)
dt2$d_targettemp_sc<-ifelse(dt2$d_targettemp_sc< -50 | dt2$d_targettemp_sc> 50, NA,dt2$d_targettemp_sc)

dt2$d_tabove_sc<-dt2$tabove
dt2$d_tabove_sc<-ifelse(dt2$d_tabove_sc<dt2$d_tabove_sdlower | dt2$d_tabove_sc>dt2$d_tabove_sdupper, NA, dt2$d_tabove_sc)
#dt2$d_tabove_sc<-ifelse(dt2$d_tabove_sc< -15 | dt2$d_tabove_sc> 46, NA,dt2$d_tabove_sc)

dt2$d_tsoil_sc<-dt2$tsoil
dt2$d_tsoil_sc<-ifelse(dt2$d_tsoil_sc<dt2$d_tsoil_sdlower | dt2$d_tsoil_sc>dt2$d_tsoil_sdupper, NA, dt2$d_tsoil_sc)
#dt2$d_tsoil_sc<-ifelse(dt2$d_tsoil_sc< -11 | dt2$d_tsoil_sc> 46, NA, dt2$d_tsoil_sc)

##advanced ommision- FT HAS ADDIONTIONAL PLOTS MANUALLY ADDED BASED ON CODE BELOW... USE OMISSION FILE
#kpdy<-c("year","pdy","treatment_abbr","plot","block","site","canopy","power","treatment_code","daylight","month")
kpdy2<-c("year","treatment_abbr","water_treatment","plot","site","canopy","power","daylight","month")

dt2[,d_tabove_sc_pdm:=mean_rm(d_tabove_sc),by=kpdy2]
dt2[,d_tsoil_sc_pdm:=mean_rm(d_tsoil_sc),by=kpdy2]
dt2[,d_targettemp_sc_pdm:=mean_rm(d_targettemp_sc),by=kpdy2]

km<-c("year","water_treatment","treatment_abbr","site","canopy","power","daylight","month")
km2<-c("year","water_treatment","plot","treatment_abbr","site","canopy","power","daylight","month")

dt2[,d_tsoil_sc_dm:= mean_rm(d_tsoil_sc_pdm),by = km2]
dt2[,d_tabove_sc_dm:= mean_rm(d_tabove_sc_pdm),by = km2]
dt2[,d_targettemp_sc_dm:=mean_rm(d_targettemp_sc_pdm),by=km2]

#ft[,d_tsoil_sc_pdy_meantrim:= me_trim2(d_tsoil_sc_pdy),by = km]
#ft[,d_tabove_sc_pdy_meantrim:= me_trim2(d_tabove_sc_pdy),by = km]
dt2[,d_tsoil_sc_dm_median:= median(d_tsoil_sc_dm,na.rm=TRUE),by = km]
dt2[,d_tabove_sc_dm_median:= median(d_tabove_sc_dm, na.rm=TRUE),by = km]
dt2[,d_targettemp_sc_dm_median:=median(d_targettemp_sc_dm,na.rm=TRUE),by=km]

#tnames(fs,"V1","d_tsoil_sc_pdm") #plot daylight month
#setkey(ft,year,treatment_abbr,plot,block,site,canopy,power,treatment_code,daylight,month)
#ft<-ft[fs]

dt2[,d_tabove_sc_dm_lower:=(-.99)+d_tabove_sc_dm_median]
dt2[,d_tabove_sc_dm_upper:=(.99)+d_tabove_sc_dm_median]
dt2[,d_tsoil_sc_dm_lower:=(-.99)+d_tsoil_sc_dm_median]
dt2[,d_tsoil_sc_dm_upper:=(.99)+d_tsoil_sc_dm_median]
dt2[,d_targettemp_sc_dm_lower:=(-.99)+d_targettemp_sc_dm_median]
dt2[,d_targettemp_sc_dm_upper:=(.99)+d_targettemp_sc_dm_median]

####filter
dt2$d_ta_omit<-ifelse(dt2$d_tabove_sc_dm<dt2$d_tabove_sc_dm_lower | dt2$d_tabove_sc_dm>dt2$d_tabove_sc_dm_upper, 0,1)
dt2$d_ts_omit<-ifelse(dt2$d_tsoil_sc_dm<dt2$d_tsoil_sc_dm_lower | dt2$d_tsoil_sc_dm>dt2$d_tsoil_sc_dm_upper, 0,1)
dt2$d_tt_omit<-ifelse(dt2$d_targettemp_sc_dm<dt2$d_targettemp_sc_dm_lower | dt2$d_targettemp_sc_dm>dt2$d_targettemp_sc_dm_upper, 0,1)


dd<-c(names(dt2),names(dt3))
dd3<-dd[duplicated(dd)]
setkeyv(dt2,dd3)
setkeyv(dt3,dd3)
dt2<-unique(dt2)
dt3<-unique(dt3)
dt4<-dt2[dt3]

write.table(dt4,sec2, append = FALSE, quote = FALSE, sep = ",",
           , na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))

##dt4<-fread(sec2)
rm(dt2)
rm(dt3)
dt2<-dt4
rm(dt4)

##dt2<-fread(sec2)

###filter dt2 using omit data
##1. create tabove_sc omit
dt2$d_targettempf_sc<-dt2$d_targettemp_sc
dt2$d_tabovef_sc<-dt2$d_tabove_sc
dt2$d_tsoilf_sc<-dt2$d_tsoil_sc
dt2[d_ta_omit == 0, d_tabovef_sc:= NA] 
dt2[d_ts_omit == 0, d_tsoilf_sc:= NA] 
dt2[d_tt_omit == 0, d_targettempf_sc:= NA] 

###create fill values for missing sensors

ky3= c("time2","treatment_abbr", "water_treatment","site", "canopy")
dt2[,d_tabovefill2_sc:= mean_rm(d_tabovef_sc), by= ky3]
dt2[,d_tsoilfill2_sc:= mean_rm(d_tsoilf_sc), by= ky3]
dt2[,d_targettempfill2_sc:= mean_rm(d_targettempf_sc), by= ky3]

is.na(dt2$d_tabovefill2_sc)<- is.na(dt2$d_tabovefill2_sc)
is.na(dt2$d_targettempfill2_sc)<- is.na(dt2$d_targettempfill2_sc)
is.na(dt2$d_tsoilfill2_sc)<- is.na(dt2$d_tsoilfill2_sc)


# dt2$d_tabovefill2_sc<-gsub(NaN, NA,dt2$d_tabovefill2_sc)
# dt2$d_tabovefill2_sc<-gsub("Null", NA,dt2$d_tabovefill2_sc)
# dt2$d_tabovefill2_sc<-as.numeric (dt2$d_tabovefill2_sc)
# dt2$d_tsoilfill2_sc<-gsub(NaN, NA,dt2$d_tsoilfill2_sc)
# dt2$d_tsoilfill2_sc<-gsub("Null", NA,dt2$d_tsoilfill2_sc)
# dt2$d_tsoilfill2_sc<-as.numeric (dt2$d_tsoilfill2_sc)

##dc treatmentfill
#dt2[treatment_abbr=="dc", d_tabovef_sc:= ifelse(is.na(d_tabovef_sc), d_tabovefill_sc, d_tabovef_sc)]
#dt2[treatment_abbr=="dc", d_tabovef_sc:= ifelse(is.na(d_tabovef_sc), d_tabovefill2_sc, d_tabovef_sc)]
#dt2[treatment_abbr=="dc", d_tsoilf_sc:= ifelse(is.na(d_tsoilf_sc), d_tsoilfill_sc, d_tsoilf_sc)]
#dt2[treatment_abbr=="dc", d_tsoilf_sc:= ifelse(is.na(d_tsoilf_sc), d_tsoilfill2_sc, d_tsoilf_sc)]

dt2[, d_targettempf2_sc:= ifelse(is.na(d_targettempf_sc), d_targettempfill2_sc, d_targettempf_sc)]#sitexcanopyfill
dt2[, d_tabovef2_sc:= ifelse(is.na(d_tabovef_sc), d_tabovefill2_sc, d_tabovef_sc)]#sitexcanopyfill
dt2[, d_tsoilf2_sc:= ifelse(is.na(d_tsoilf_sc), d_tsoilfill2_sc, d_tsoilf_sc)]#sitexcanopyfill

###create mean dc values
dt4<-dt2[treatment_abbr=="dc"& water_treatment=="ambient",]
dt4[,d_soil_mean_dc := mean(d_tsoil_sc, na.rm=TRUE), by= ky3]
dt4[,d_soil_meanf_dc := mean(d_tsoilf2_sc, na.rm=TRUE), by= ky3]
dt4[,d_above_mean_dc := mean(d_tabove_sc, na.rm=TRUE), by= ky3]
dt4[,d_above_meanf_dc := mean(d_tabovef2_sc, na.rm=TRUE), by= ky3]
dt4[,d_targettemp_mean_dc := mean(d_targettemp_sc, na.rm=TRUE), by= ky3]
dt4[,d_targettemp_meanf_dc := mean(d_targettempf2_sc, na.rm=TRUE), by= ky3]

### create dc average (for computing deltas) and merge
key2<-c("time2","site","canopy","d_soil_mean_dc","d_soil_meanf_dc","d_above_mean_dc", "d_above_meanf_dc", "d_targettemp_mean_dc","d_targettemp_meanf_dc")

dt4<-subset(dt4, select= key2)
dt4<-unique(dt4)
setkey(dt2,site,canopy,time2)
setkey(dt4,site,canopy,time2)
dt2<-dt2[dt4]

####breakv5 code here!
write.table(dt2,sec3, append = FALSE, quote = FALSE, sep = ",",
           , na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))

##dt2<-fread(sec3)

###Energy derived variables
# keep<- c("time2","site","year","doy","plot","month","treatment_abbr","s_scldout","scldout","block","canopy","power","treatment_code","daylight") ###add elevation back in.
# #fread(dt)
# #fread(ft)
# dt2<-subset(dt,select = keep)
# dt2$plot<-gsub(" ","",dt2$plot)
# dt2$block<-gsub(" ","",dt2$block)

# setkey(fd,year,month,treatment_abbr,plot,block,site,canopy,power,treatment_code,daylight)
# setkey(dt2,year,month,treatment_abbr,plot,block,site,canopy,power,treatment_code,daylight)

# # dt2<-merge(dt2, fd, all=TRUE)

# dt2[,d_a_energy:= scldout]
# dt2[,d_a_energy_sc:= scldout]
# dt2[is.na(s_scldout),d_a_energy_sc:= NA]### remove bad energy values

# ###bridged sensors
# dt2[year =="2011" & plot== "d2    " & doy>104 & doy< 136, d_a_energy_sc := NA]
# dt2[year =="2011" & plot== "i8    " & doy>120 & doy< 144, d_a_energy_sc := NA]


# ky2= c("time2","treatment_abbr","block")
# dt2[,d_energyfill_sc:= mean(d_a_energy_sc, na.rm=TRUE), by= ky2]
# dt2[year =="2011" & plot== "d2    "& doy>104 & doy< 136, d_a_energy_sc := d_energyfill_sc]
# dt2[year =="2011" & plot== "i8    "& doy>120 & doy< 144, d_a_energy_sc := d_energyfill_sc]

# dt2[,d_a_energymodel_sc:= d_a_energy_sc]
# dt2[d_ta_omit2 == 0,d_a_energy_model_sc:= NA]

# filt<-c(0, 7999,10000,9900, 100,4000,5000,5980,7940,2060,9888.89,9944.44,6000)
# f1<-cbind(filt,0)
# f1<-data.table(f1)
# names(f1)<-c("d_a_energy", "d_energy_code")
# setkey(f1, d_a_energy)
# setkey(dt2, d_a_energy)
# dt2<-f1[dt2]
# dt2[d_energy_code == 0, d_a_energy_sc:= NA]
# rm(f1)
# dt2[,d_b_energy:= s_scldout]
# dt2[,d_b_energy_sc:= s_scldout]
# dt2[d_b_energy == 0| d_b_energy == 4000, d_b_energy_sc:= NA ]

### compute to kwh
dt2[treatment_abbr=="h1",d_above_kw:= ((scldout/10000)^2 *6)/7.06]
dt2[treatment_abbr=="h2",d_above_kw:= ((scldout/10000)^2 *8)/7.06]
dt2[,d_soil_kw:=((s_scldout/10000)^2 * 1.740 )/7.06]
dt2[,d_total_kw:= d_above_kw+d_soil_kw]

dt2[treatment_abbr=="h1",d_above_kw_alt:= (scldsq *6)/7.06]
dt2[treatment_abbr=="h2",d_above_kw_alt:= (scldsq *8)/7.06]
dt2[,d_soil_kw_alt:=(sscldsq * 1.740 )/7.06]
dt2[,d_total_kw_alt:= d_above_kw_alt+d_soil_kw_alt]

# dt2[treatment_abbr=="h1",d_abovemodel_kw_sc:= ((d_a_energymodel_sc/10000)^2 *6)/7.06]
# dt2[treatment_abbr=="h2",d_abovemodel_kw_sc:= ((d_a_energymodel_sc/10000)^2 *8)/7.06]
# dt2[,d_soilmodel_kw_sc:=((d_b_energy_sc/10000)^2 * 1.740 )/7.06]
# dt2[,d_totalmodel_kw_sc:= d_abovemodel_kw_sc+d_soil_kw_sc]

# denergy<-dt2
# rm(dt2)
# key6<-c("d_a_energy","d_energy_code","year", "time2","plot","s_scldout","scldout","d_a_energy_sc","d_energyfill_sc","d_a_energymodel_sc","d_a_energy_model_sc","d_b_energy", "d_b_energy_sc","d_above_kw","d_soil_kw","d_total_kw","d_above_kw_sc","d_soil_kw_sc","d_total_kw_sc", "d_abovemodel_kw_sc","d_soilmodel_kw_sc","d_totalmodel_kw_sc")
# denergy<-subset(denergy, select=key6)

# write.table(denergy,path4, append = FALSE, quote = FALSE, sep = ",",
           # , na = "NA", dec = ".", row.names = FALSE,
          # col.names = TRUE, qmethod = c("escape", "double"))

# dt6<-fread(path3)

# setkey(dt6, plot, year,time2)
# setkey(denergy, plot,year, time2)
# dt7<-merge(denergy, dt6, all=TRUE)

# write.table(dt7,path4, append = FALSE, quote = FALSE, sep = ",",
           # , na = "NA", dec = ".", row.names = FALSE,
          # col.names = TRUE, qmethod = c("escape", "double"))

#create hourly derived data
# names(dt7)
#[1] "plot"              "year"              "time2"
# [7] "s_scldout"         "scldout"           "d_a_energy_sc"
#[10] "d_b_energy"        "d_b_energy_sc"     "d_above_kw"
#[13] "d_soil_kw"         "d_total_kw"        "d_above_kw_sc"
#[16] "d_soil_kw_sc"      "d_total_kw_sc"     "block"
#[19] "site"              "canopy"            "month"
#[22] "week"              "hour"              "doy"
#[25] "pdy"               "tsoil"             "tabove"
#[28] "rh"                "vp_avg"            "airtc_avg"
#[31] "treatment_code"    "treatment_abbr.y"  "daylight"
#[34] "d_rh"              "d_vp"              "d_airtc"
#[37] "d_tsoil_meantrim"  "d_tabove_meantrim" "d_tsoil_sdtrim"
#[40] "d_tabove_sdtrim"   "d_tabove_sdlower"  "d_tabove_sdupper"
#[43] "d_tsoil_sdlower"   "d_tsoil_sdupper"   "d_tabove_sc"
#[46] "d_tsoil_sc"        "d_tsoilfill_sc"    "d_tabovefill_sc"
#[49] "d_soil_mean_dc"    "d_above_mean_dc"   "d_tsoil_delta"
#[52] "d_tabove_delta"    "d_tabovef_sc"      "d_tsoilf_sc"
#[55] "d_tsoilf_delta"    "d_tabovef_delta"
###start here



### aggregate to hourly
# function will aggregate data
col_key<-c("year","doy","hour","plot")

col_keep<-c("site","canopy","year","doy","hour","week","month","daylight","pdy","plot","treatment_abbr","water_treatment", "start_doy","stop_doy","warming_treatment","power")

col_avg<-c("s_angle","s_elevation","tabove" ,"targettemp","tsoil","d_tsoil_meantrim" ,"d_tabove_meantrim","d_tsoil_sdtrim","d_tabove_sdtrim","d_tabove_sdlower","d_tabove_sdupper","d_tsoil_sdlower","d_tsoil_sdupper","d_targettemp_meantrim","d_targettemp_sdtrim","d_targettemp_sdlower","d_targettemp_sdupper","d_targettemp_sc","d_tabove_sc","d_tsoil_sc","d_tabove_sc_pdm","d_tsoil_sc_pdm","d_targettemp_sc_pdm","d_tsoil_sc_dm","d_tabove_sc_dm","d_targettemp_sc_dm","d_tsoil_sc_dm_median","d_tabove_sc_dm_median","d_targettemp_sc_dm_median","d_tabove_sc_dm_lower","d_tabove_sc_dm_upper","d_tsoil_sc_dm_lower","d_tsoil_sc_dm_upper","d_targettemp_sc_dm_lower","d_targettemp_sc_dm_upper","d_ta_omit","d_ts_omit","d_tt_omit","d_ws_ms_s_wvt","d_winddir_d1_wvt","d_winddir_sd1_wvt","d_ws_ms_avg","d_rh","d_vp","d_airtc","s_all_avgt_avg","a_dc","b_dif2","b_dif3","b_dif","dif_a","heat_a_avg","meanpin" ,"meanpinr","mnpin","mxpin" ,"pid_lmt","runavga" ,"s_dc" ,"s_pid_lmt" ,"s_scldout","samb","sbtemp","scldout" ,"targettemp_adj","tmv","tsoilr","scldsq","sscldsq" ,"d_targettempf_sc","d_tabovef_sc","d_tsoilf_sc","d_tabovefill2_sc","d_tsoilfill2_sc" ,"d_targettempfill2_sc","d_targettempf2_sc","d_tabovef2_sc","d_tsoilf2_sc" ,"d_soil_mean_dc" ,"d_soil_meanf_dc","d_above_mean_dc","d_above_meanf_dc","d_targettemp_mean_dc","d_targettemp_meanf_dc","d_above_kw","d_soil_kw","d_total_kw","d_above_kw_alt","d_soil_kw_alt","d_total_kw_alt","batt_volt_min","amb_a_avg","ptemp_avg", "airtemp_avg")

dt5<-subset(dt2, select=col_keep) ### not sure why I wrote this originally - to keep extra variables
dt5<-unique(dt5)
setkey(dt5, plot, hour, doy, year)

sub<-c(col_key,col_avg)
dt3<-subset(dt2, select=sub)
dt4<-dt3[,lapply(.SD,mean_rm), by=col_key]

setkey(dt4, plot, hour, doy, year)
setkey(dt2, plot, hour, doy, year)

ky<-c("plot","hour","doy","year")
dh1<-dt2[,max(d_airtcmax, na.rm=FALSE),by = ky]
setnames(dh1,"V1","d_airtcmax")
setkey(dh1, plot, hour, doy, year)

dt4<-dh1[dt4]

dh1<-dt2[,min(d_airtcmin, na.rm=FALSE),by = ky]
setnames(dh1,"V1","d_airtcmin")
setkey(dh1, plot, hour, doy, year)

dt4<-dh1[dt4]

dt6<-dt4[dt5] #Not sure why I worte this originally- join variables back in.

write.table(dt6,sec4, append = FALSE, quote = FALSE, sep = ",",
           , na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))


  [1] "site"                      "canopy"                    "time2"                     "year"                     
  [5] "doy"                       "hour"                      "week"                      "month"                    
  [9] "daylight"                  "pdy"                       "plot"                      "treatment_abbr"           
 [13] "water_treatment"           "tabove"                    "targettemp"                "tsoil"                    
 [17] "power"                     "d_tsoil_meantrim"          "d_tabove_meantrim"         "d_tsoil_sdtrim"           
 [21] "d_tabove_sdtrim"           "d_tabove_sdlower"          "d_tabove_sdupper"          "d_tsoil_sdlower"          
 [25] "d_tsoil_sdupper"           "d_targettemp_meantrim"     "d_targettemp_sdtrim"       "d_targettemp_sdlower"     
 [29] "d_targettemp_sdupper"      "d_targettemp_sc"           "d_tabove_sc"               "d_tsoil_sc"               
 [33] "d_tabove_sc_pdm"           "d_tsoil_sc_pdm"            "d_targettemp_sc_pdm"       "d_tsoil_sc_dm"            
 [37] "d_tabove_sc_dm"            "d_targettemp_sc_dm"        "d_tsoil_sc_dm_median"      "d_tabove_sc_dm_median"    
 [41] "d_targettemp_sc_dm_median" "d_tabove_sc_dm_lower"      "d_tabove_sc_dm_upper"      "d_tsoil_sc_dm_lower"      
 [45] "d_tsoil_sc_dm_upper"       "d_targettemp_sc_dm_lower"  "d_targettemp_sc_dm_upper"  "d_ta_omit"                
 [49] "d_ts_omit"                 "d_tt_omit"                 "d_ws_ms_s_wvt"             "d_winddir_d1_wvt"         
 [53] "d_winddir_sd1_wvt"         "d_ws_ms_avg"               "d_rh"                      "d_vp"                     
 [57] "d_airtc"                   "d_airtcmax"                "d_airtcmin"                "start_doy"                
 [61] "stop_doy"                  "long"                      "lat"                       "s_angle"                  
 [65] "s_elevation"               "warming_treatment"         "treatment_code"            "rowid"                    
 [69] "timestamp"                 "block"                     "batt_volt_min"             "flag1"                    
 [73] "flag2"                     "flag3"                     "flag4"                     "flag5"                    
 [77] "flag6"                     "flag7"                     "flag8"                     "flag9"                    
 [81] "flag10"                    "flag11"                    "flag12"                    "flag13"                   
 [85] "amb_a_avg"                 "ptemp_avg"                 "airtemp_avg"               "airtemp_max"              
 [89] "airtemp_min"               "airtc_avg"                 "rh"                        "vp_avg"                   
 [93] "ws_ms_s_wvt"               "winddir_d1_wvt"            "winddir_sd1_wvt"           "ws_ms_avg"                
 [97] "s_all_avgt_avg"            "a_dc"                      "b_dif2"                    "b_dif3"                   
[101] "b_dif"                     "dif_a"                     "heat_a_avg"                "meanpin"                  
[105] "meanpinr"                  "mnpin"                     "mxpin"                     "pid_lmt"                  
[109] "runavga"                   "s_dc"                      "s_pid_lmt"                 "s_scldout"                
[113] "samb"                      "sbtemp"                    "scldout"                   "targettemp_adj"           
[117] "tmv"                       "tsoilr"                    "scldsq"                    "sscldsq"                  
[121] "d_targettempf_sc"          "d_tabovef_sc"              "d_tsoilf_sc"               "d_tabovefill2_sc"         
[125] "d_tsoilfill2_sc"           "d_targettempfill2_sc"      "d_targettempf2_sc"         "d_tabovef2_sc"            
[129] "d_tsoilf2_sc"              "d_soil_mean_dc"            "d_soil_meanf_dc"           "d_above_mean_dc"          
[133] "d_above_meanf_dc"          "d_targettemp_mean_dc"      "d_targettemp_meanf_dc"     "d_above_kw"               
[137] "d_soil_kw"                 "d_total_kw"                "d_above_kw_alt"            "d_soil_kw_alt"            
[141] "d_total_kw_alt"   

rm(dh1)
rm(dt2)
rm(dt3)
rm(dt5)
rm(dt4)
dt6<-fread(sec4)
dvwc<-fread(primary60_dir)
dvwc$plot<-gsub(" ","", dvwc$plot)
dvwc$block<-gsub(" ","", dvwc$block)
tex<-fread(texture)

dvwc<-dvwc[,hour:=hour(time2)]
dvwc<-dvwc[,year:=year(time2)]
dvwc<-dvwc[,doy:=yday(time2)]

setkey(dvwc, plot,hour,doy,year)
setkey(dt6, plot, hour, doy, year)
ky<-c("plot","hour","doy","year")
dt6<-unique(dt6)
d60<-dt6[dvwc]

write.table(d60,sec5, append = FALSE, quote = FALSE, sep = ",",
           , na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))

#d60<-fread(sec5)
####NOW
# ####Model to fill missing values of tabove:
# ###create variables for models
# ###create modelling dataset for filling data
# #### d60<-dt
# dm<-d60[treatment_abbr=="h1" | treatment_abbr=="h2" | treatment_abbr=="dc", ]
# dm<-dm[year > 2008, ]
# dm<-dm[power==1, ]
# key4<-c("canopy","month","year","hour","site","daylight","treatment_abbr")
# key5<-c("month","year","hour","doy","site","daylight","treatment_abbr")

# dm2<-dm[canopy=="closed",mean(d_tabovef2_sc, na.rm=TRUE),by=key5]
# setnames(dm2, "V1", "d_tabovef2_sc_canopyclosed")
# setkey(dm2, month,doy,year,hour,site,daylight,treatment_abbr)

# dm4<-dm[canopy=="open",mean(d_tabovef2_sc, na.rm=TRUE),by=key5]
# setnames(dm4, "V1", "d_tabovef2_sc_canopyopen")
# setkey(dm4, month,doy,year,hour,site,daylight,treatment_abbr)

# dm3<-dm2[dm4]

# dm3$treatment_abbr<-as.factor(dm3$treatment_abbr)
# dm3$year<-as.factor(dm3$year)
# dm3$daylight<-as.factor(dm3$daylight)
# dm3$hour<-as.factor(dm3$hour)
# dm3$month<-as.factor(dm3$month)

# modelopen<-lm(d_tabovef2_sc_canopyopen~treatment_abbr+site+year+d_tabovef2_sc_canopyclosed*hour+month,dm3)
# modelclosed<-lm(d_tabovef2_sc_canopyclosed~treatment_abbr+site+year+d_tabovef2_sc_canopyopen*hour+month,dm3)

# dm2<-dm[canopy=="closed",d_tabovefill2_sc,by=key5]
# setnames(dm2, "d_tabovefill2_sc", "d_tabovefill2_canopyclosed")
# setkey(dm2, month,year,doy,hour,site,daylight,treatment_abbr)

# dm4<-dm[canopy=="open",d_tabovefill2_sc,by=key5]
# setnames(dm4, "d_tabovefill2_sc", "d_tabovefill2_canopyopen")
# setkey(dm4, month,year,hour,doy,site,daylight,treatment_abbr)

# dm2<-unique(dm2)
# dm4<-unique(dm4)
# dm6<-merge(dm2,dm4, all=TRUE)

# ###merge with d60?
# dm<-d60[treatment_abbr=="h1" | treatment_abbr=="h2" | treatment_abbr=="dc", ]
# dm<-dm[year > 2008, ]
# dm<-dm[month > 2, ]
# dm<-dm[month < 12, ]

# dm$treatment_abbr<-as.factor(dm$treatment_abbr)
# dm$year<-as.factor(dm$year)
# dm$daylight<-as.factor(dm$daylight)
# dm$hour<-as.factor(dm$hour)
# dm$month<-as.factor(dm$month)
# setkey(dm, month,year,hour,doy,site,daylight,treatment_abbr)

# dm6$treatment_abbr<-as.factor(dm6$treatment_abbr)
# dm6$year<-as.factor(dm6$year)
# dm6$daylight<-as.factor(dm6$daylight)
# dm6$hour<-as.factor(dm6$hour)
# dm6$month<-as.factor(dm6$month)
# setkey(dm6, month,year,hour,doy,site,daylight,treatment_abbr)

# dm7<-merge(dm6,dm, all=TRUE)
# dm7$treatment_abbr<-as.factor(dm7$treatment_abbr)
# dm7$year<-as.factor(dm7$year)
# dm7$daylight<-as.factor(dm7$daylight)
# dm7$hour<-as.factor(dm7$hour)
# dm7$month<-as.factor(dm7$month)

# setnames(dm7, "d_tabovef2_sc_canopyopen","d_tabovef2_sc_canopyopen_old")
# setnames(dm7, "d_tabovef2_sc_canopyclosed","d_tabovef2_sc_canopyclosed_old")


# setnames(dm7, "d_tabovefill2_canopyopen", "d_tabovef2_sc_canopyopen")
# setnames(dm7, "d_tabovefill2_canopyclosed", "d_tabovef2_sc_canopyclosed")

# dm7[,p_tabovef2_closedfill_new:=predict.lm(modelclosed,dm7),]
# dm7[,p_tabovef2_openfill_new:=predict.lm(modelopen,dm7),]

# dm7[canopy=="open", p_tabovef2_sc_new:= ifelse(is.na(d_tabovef2_sc), p_tabovef2_openfill_new, d_tabovef2_sc)]
# dm7[canopy=="closed",p_tabovef2_sc_new:= ifelse(is.na(d_tabovef2_sc), p_tabovef2_closedfill_new, d_tabovef2_sc)]
# ###reset keys to null here

# col7= c("year","hour", "week", "site", "canopy", "plot" )
 # d60[, above_w_fill:=mean_rm(d_tabovef2_sc), by= col7]
 # d60[, soil_w_fill:=mean_rm(d_tsoilf2_sc), by= col7]

library(zoo)
setkey(d60, "year","doy","hour","plot")
col9= c( "hour","plot" )
d60[,tsoilrollfill_5:=rollapply(d_tsoilf2_sc, 5, mean, na.rm = TRUE, fill = NA, align="center"),by =col9]
d60[,taboverollfill_5:=rollapply(d_tabovef2_sc, 5, mean, na.rm = TRUE, fill = NA, align="center"),by =col9]
d60[,targettemprollfill_5:=rollapply(d_targettempf2_sc, 5, mean, na.rm = TRUE, fill = NA, align="center"),by =col9] #new for 2014

#setkey(dm7, "year","doy","plot","hour")

#### filling step using rolling averages.

#d60[, p_tabovef2_sc_new2:= ifelse(is.na(d_tabovef2_sc), above_w_fill, d_tabovef2_sc)]
d60[, p_tabovef2_sc_new3:= ifelse(is.na(d_tabovef2_sc), taboverollfill_5, d_tabovef2_sc)]
#d60[, d_tsoilf2_sc_new2:= ifelse(is.na(d_tsoilf2_sc), soil_w_fill, d_tsoilf2_sc)]
d60[, d_tsoilf2_sc_new3:= ifelse(is.na(d_tsoilf2_sc), tsoilrollfill_5, d_tsoilf2_sc)]
d60[, d_targettempf2_sc_new3:= ifelse(is.na(d_targettempf2_sc), targettemprollfill_5, d_targettempf2_sc)]##new for 2013

#setnames(dm7, "d_tabovef2_sc_canopyopen","d_tabovef2_sc_canopyopen_new")
#setnames(dm7, "d_tabovef2_sc_canopyclosed","d_tabovef2_sc_canopyclosed_new")
# setnames(dm7, "p_tabovef2_sc","p_tabovef2_sc_new")
# setnames(dm7, "p_tabovef2_closedfill","p_tabovef2_closedfillnew")
# setnames(dm7, "p_tabovef2_openfill","p_tabovef2_openfillnew")


# cols3<- c("year","hour","doy","d_tabovef2_sc_canopyclosed_new","d_tabovef2_sc_canopyopen_new","plot","p_tabovef2_closedfill","p_tabovef2_openfill","p_tabovef2_sc")
#cols3<- c("year","hour","doy","d_tabovef2_sc_canopyclosed_new","d_tabovef2_sc_canopyopen_new","plot","p_tabovef2_closedfill_new","p_tabovef2_openfill_new","p_tabovef2_sc_new","p_tabovef2_sc_new2","p_tabovef2_sc_new3","d_tsoilf2_sc_new2","d_tsoilf2_sc_new3","taboverollfill_5", "tsoilrollfill_5","above_w_fill","soil_w_fill")


###RECREATE MEAN DC VALUES
# dt6<-dm7
# dt6<-subset(dt6,select=cols3)
# setkey(dt6, plot,hour,doy,year)
# d60$year<-as.factor(d60$year)
# d60$hour<-as.factor(d60$hour)
# d60$month<-as.factor(d60$month)
# setkey(d60, plot,hour,doy,year)

# dt9<-dt6[d60]
# dt6<-dt9
# setkey(dt6, plot,hour,doy,year)
# setkey(dt9, year,hour, doy, site, canopy)

# ### Add treatment labels for hourly
# des<-fread(design_table)
# sub1<-c( "site", "canopy", "plot", "warming_treatment" , "treatment_code" ,"treatment_abbr", "water_treatment" )
# des1<- unique(subset(des, treatment_abbr=="ac"|treatment_abbr=="dc"| treatment_abbr=="h1"|treatment_abbr=="h2",  select = sub1))
# setkey(d60, plot)
# setkey(des1, plot)
# d60<-des1[d60]
# rm(dt)
# rm(des1)
# rm(des)

###For making delta values several filling alternatives

ky3= c("year","hour", "doy", "site", "canopy")
dt6<-d60[treatment_abbr=="dc" & water_treatment=="ambient",]
#dt6[,p_above_meanf2_dc_new := mean(p_tabovef2_sc_new, na.rm=TRUE), by= ky3] #use for delta
#dt6[,p_above_meanf2_dc_new2 := mean_rm(p_tabovef2_sc_new2), by= ky3] #use for delta
dt6[,p_above_meanf2_dc_new3 := mean_rm(p_tabovef2_sc_new3), by= ky3] #use for delta
dt6[,d_targettempf2_dc_new3:= mean_rm(d_targettempf2_sc_new3), by= ky3] #use for delta new for 2013
dt6[,d_tsoilf2_dc_new3:= mean_rm(d_tsoilf2_sc_new3), by= ky3] #use for delta
# dt6[,d_tsoilf2_dc_new2:= mean_rm(d_tsoilf2_sc_new2), by= ky3] #use for delta

#dt6[,p_above_meanf2_dc2_new:= ifelse(is.na(d_above_meanf_dc),p_above_meanf2_dc_new,d_above_meanf_dc)]
# dt6[,p_above_meanf2_dc2_new2:= ifelse(is.na(d_above_meanf_dc),p_above_meanf2_dc_new2,d_above_meanf_dc)]
dt6[,p_above_meanf2_dc2_new3:= ifelse(is.na(d_above_meanf_dc),p_above_meanf2_dc_new3,d_above_meanf_dc)]
dt6[,d_targettempf2_dc2_new3:= ifelse(is.na(d_targettemp_meanf_dc),d_targettempf2_dc_new3,d_targettemp_meanf_dc)]

# dt6[,d_tsoilf2_dc2_new2:= ifelse(is.na(d_soil_meanf_dc),d_tsoilf2_dc_new2,d_soil_meanf_dc)]
dt6[,d_tsoilf2_dc2_new3:= ifelse(is.na(d_soil_meanf_dc),d_tsoilf2_dc_new3,d_soil_meanf_dc)]

# cols4<- c("year","hour","doy","site", "canopy","p_above_meanf2_dc_new","p_above_meanf2_dc2_new", "p_above_meanf2_dc_new2","p_above_meanf2_dc2_new2", "p_above_meanf2_dc_new3","p_above_meanf2_dc2_new3", "d_tsoilf2_dc_new3","d_tsoilf2_dc2_new3","d_tsoilf2_dc_new2","d_tsoilf2_dc2_new2")

cols4<- c("year","hour","doy","site", "canopy","p_above_meanf2_dc_new3","p_above_meanf2_dc2_new3","d_tsoilf2_dc_new3","d_tsoilf2_dc2_new3","d_targettempf2_dc_new3","d_targettempf2_dc2_new3")

dt6<-subset(dt6, select=cols4)
setkey(dt6, year,hour, doy, site, canopy)
setkey(d60, year,hour, doy, site, canopy)
dt6<-unique(dt6)
dt6<-dt6[d60]  ## merge ambient values with hourly data


### delta calculations

# dt10<-merge(dt6,dt9, all=TRUE)
# dt6<-dt10
###b= block f= filtered
# dt6[,d_tsoil_delta:= d_tsoil_sc_hr-d_soil_mean_dc]
dt6[,d_tsoilf_delta:= d_tsoilf_sc-d_soil_meanf_dc]#use
# dt6[,d_tsoilf1_delta:= d_tsoilf1_sc-d_soil_meanf_dc]
dt6[,d_tsoilf2_delta:= d_tsoilf2_sc-d_soil_meanf_dc]#use filled
# dt6[,d_tsoilb_delta:= d_tsoil_sc_hr-d_soil_meanb_dc]
# dt6[,d_tsoilfb_delta:= d_tsoilf_sc-d_soil_meanfb_dc]
# dt6[,d_tsoilfb1_delta:= d_tsoilf1_sc-d_soil_meanfb_dc]
# dt6[,d_tsoilfb2_delta:= d_tsoilf2_sc-d_soil_meanfb_dc]

# dt6[,d_tabove_delta:= d_tabove_sc_hr-d_above_mean_dc]
dt6[,d_tabovef_delta:= d_tabovef_sc-d_above_meanf_dc]#use
# dt6[,d_tabovef1_delta:= d_tabovef1_sc-d_above_meanf_dc]
dt6[,d_tabovef2_delta:= d_tabovef2_sc-d_above_meanf_dc]
# dt6[,d_taboveb_delta:= d_tabove_sc_hr-d_above_meanb_dc]
# dt6[,d_tabovefb_delta:= d_tabovef_sc-d_above_meanfb_dc]#use filled
# dt6[,d_tabovefb1_delta:= d_tabovef1_sc-d_above_meanfb_dc]
# dt6[,d_tabovefb2_delta:= d_tabovef2_sc-d_above_meanfb_dc]

# dt6[,d_tabovef2_delta_base:= d_tabovef2_sc-d_above_meanf_dc]
# dt6[,p_tabovef2_delta_new:= p_tabovef2_sc_new-p_above_meanf2_dc2_new]
# dt6[,p_tabovef2_delta_new2:= p_tabovef2_sc_new2-p_above_meanf2_dc2_new2]
dt6[,p_tabovef2_delta_new3:= p_tabovef2_sc_new3-p_above_meanf2_dc2_new3]#use filled
# dt6[,d_tsoilf2_delta_new2:= d_tsoilf2_sc_new2-d_tsoilf2_dc2_new2]
dt6[,d_tsoilf2_delta_new3:= d_tsoilf2_sc_new3-d_tsoilf2_dc2_new3]#use filled

### new for 2013 targettemp
dt6[,d_targettempf_delta:= d_targettempf_sc-d_targettemp_meanf_dc]#use
dt6[,d_targettempf2_delta:= d_targettemp_sc-d_targettemp_meanf_dc]#use filled
dt6[,d_targettempf2_delta_new3:= d_targettempf2_sc_new3-d_targettempf2_dc2_new3]#use filled


# col5<-c("year", "doy", "hour", "site", "canopy", "plot","treatment_abbr","d_tabovef2_delta_base","p_tabovef2_delta_new", "p_tabovef2_sc_new","p_tabovef2_delta_new2", "p_tabovef2_sc_new2","p_tabovef2_delta_new3", "p_tabovef2_sc_new3", "d_tabovef2_delta","d_tsoilf2_delta_new2","d_tsoilf2_delta_new3", "d_tsoilf2_sc_new2","d_tsoilf2_sc_new2")

# dt10<-subset(dt6, select=col5)

write.table(dt6, sec6, append = FALSE, quote = FALSE, sep = ",",
           , na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))



###still need to construct merge
texture<-"/Volumes/disk7/b4warmed3/derived_data/texture.csv"
tex<-fread(texture)

dvwc2<-dt6
# dvwc2<-dvwc2[,week:=week(time2)]
# dvwc2$month<-month(dvwc2$time2)
# dvwc2$week<-week(dvwc2$time2)

###monthly data for filling data
key<-c("plot","month","hour","year")
dvwc2[,d_tsoil_sc_mm:= mean_rm(d_tsoilf2_sc_new3),by = key]
# dvwc2[,d_tsoil_delta_sc_mm:= mean(d_tsoilf2_delta, na.rm=TRUE),by = key]
dvwc2[,d_tabove_sc_mm:= mean_rm(p_tabovef2_sc_new3),by = key]
# dvwc2[,d_tabove_delta_sc_mm:= mean(p_tabovef2_delta, na.rm=TRUE),by = key]
dvwc2[,d_targettemp_sc_mm:= mean_rm(d_targettempf2_sc_new3),by = key]

##advanced ommision-for VWC
kpdy2<-c("year","treatment_abbr","water_treatment","plot","site","canopy","week")

dvwc2[,period_sc1:=mean_rm(period),by=kpdy2]

km<-c("year","water_treatment","treatment_abbr","site","canopy","week")
km2<-c("year","water_treatment","plot","treatment_abbr","site","canopy","week")

dvwc2[,d_period_sc_dw:=mean_rm(period_sc1),by=km2]
dvwc2[,d_period_sc_dw_median:=median(period_sc1,na.rm=TRUE),by=km]

dvwc2[,d_period_sc_dw_lower:=(-.99)+d_period_sc_dw_median]
dvwc2[,d_period_sc_dw_upper:=(.99)+d_period_sc_dw_median]

####filter
dvwc2$d_period_omit<-ifelse(dvwc2$d_period_sc_dw<dvwc2$d_period_sc_dw_lower |dvwc2$d_period_sc_dw>dvwc2$d_period_sc_dw_upper, 0,1)

###filter dvwc2 using omit data

dvwc2$d_periodf<-dvwc2$period
dvwc2[d_period_omit == 0, d_periodf:= NA] 

###create fill values for missing sensors

ky3= c("time2","treatment_abbr", "water_treatment","site", "canopy")
dvwc2[,d_periodfill:= mean_rm(d_periodf), by= ky3]
is.na(dvwc2$d_periodfill)<- is.na(dvwc2$d_periodfill)

dvwc2[, d_periodf2:= ifelse(is.na(d_periodf), d_periodfill, d_periodf)]#sitexcanopyfill

###join texture data
dvwc2[,d_plot:= gsub(" ","" ,plot)]
setkey(dvwc2,d_plot)
setkey(tex,d_plot)
key2<-c("bulk_density","sand_percent", "clay_percent","d_plot")
tex<-subset(tex, select=key2)
dvwc2<-merge(tex,dvwc2,all= TRUE)

#William Eddy:
#Temperature correct per Campbell's equation,
#period+(20-soil temp)*(0.526-0.052*period+0.00136*period^2)

###replaces missing tsoil data with _mm data (and temperature correction)
dvwc2[,d_period_tscor2:= d_periodf2+(20-d_tsoilf2_sc)*(0.526-0.052*d_periodf2+0.00136*d_periodf2^2)]
dvwc2[is.na(d_period_tscor2), d_period_tscor2:= d_periodf2+(20-d_tsoil_sc_mm)*(0.526-0.052*d_periodf2+0.00136*d_periodf2^2)]

dvwc2[,d_period_tscor:= period+(20-d_tsoilf2_sc)*(0.526-0.052*period+0.00136*period^2)]
dvwc2[is.na(d_period_tscor), d_period_tscor:= period+(20-d_tsoil_sc_mm)*(0.526-0.052*period+0.00136*period^2)]

###Peter equation:
 #    Parameter Estimates
#Term	 	Estimate	Std Error	t Ratio	Prob>|t|
#Intercept		 -0.207062	0.048452	 -4.27	<.0001*
#Period Temp Corr		0.0209658	0.001299	16.14	<.0001*
#% CLAY (1-20CM; constant BD)		 -0.000542	0.001371	 -0.40	0.6937
#% SAND (1-20CM; constant BD)		 -0.00137	0.000465	 -2.95	0.0041*
#(% CLAY (1-20CM; constant BD)-12.5622)*(% SAND (1-20CM; constant BD)-60.4412)		 -0.000477	0.000161	 -2.96	0.0039*

dvwc2[,d_vwc_corr:= -0.207062 + (0.0209658*d_period_tscor) -(0.000542*clay_percent) -(0.00137*sand_percent) - 0.000477*((clay_percent-12.5622)*(sand_percent -60.4412))]

dvwc2[,d_vwc_corr2:= -0.207062 + (0.0209658*d_period_tscor2) -(0.000542*clay_percent) -(0.00137*sand_percent) - 0.000477*((clay_percent-12.5622)*(sand_percent -60.4412))]


#Parameter Estimates
#Term	 	Estimate	Std Error	t Ratio	Prob>|t|
#Intercept		 -0.289268	0.041394	 -6.99	<.0001*
#% SAND (1-20CM; constant BD)		 -0.000549	0.000389	 -1.41	0.1619
#% CLAY (1-20CM; constant BD)		0.0025543	0.000925	2.76	0.0070*
#Period Temp Corr		0.0210626	0.001353	15.57	<.0001*

dvwc2[,d_vwc_corr_sc:= ifelse(d_vwc_corr < .60, d_vwc_corr, NA),]
dvwc2[,d_vwc_corr_sc:= ifelse(d_vwc_corr_sc > 0, d_vwc_corr_sc, NA),]

dvwc2[,d_vwc_corr_sc2:= ifelse(d_vwc_corr2 < .60, d_vwc_corr2, NA),]
dvwc2[,d_vwc_corr_sc2:= ifelse(d_vwc_corr_sc2 > 0, d_vwc_corr_sc2, NA),]

write.table(dvwc2,sec7, append = FALSE, quote = FALSE, sep = ",",
           , na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))


dvwc2<-fread(sec7)
###variables collected on individual blocks to site level

###Humidity
drh<-dvwc2[block=="b"|block=="d"|block=="i"| block=="k",]
setkey(drh, site,canopy,time2)
key2<- c("time2","rh","vp_avg","airtc_avg","site","canopy")
drh<-subset(drh, select=key2)
setnames(drh, "rh", "dd_rh")
setnames(drh, "airtc_avg", "dd_airtc")
setnames(drh, "vp_avg", "dd_vp")
drh<- unique(drh) ##needed to remove double rows
setkey(dvwc2, site,canopy,time2)
setkey(drh, site,canopy,time2)
dvwc2<-merge(dvwc2, drh, all=TRUE)

###PAR
###variables collected on individual blocks to site level
dpar<-dvwc2[block=="c"|block=="f"|block=="g"| block=="j",]
setkey(dpar, site, canopy,time2)
key2<- c("time2","par_tot_tot","par_den_avg","site","canopy")
dpar<-subset(dpar, select=key2)
setnames(dpar, "par_tot_tot", "dd_par_tot")
setnames(dpar, "par_den_avg", "dd_par_den")
dpar<- unique(dpar) ##needed to remove double rows
setkey(dvwc2, site,canopy,time2)
setkey(dpar, site,canopy,time2)
dvwc2<-merge(dvwc2, dpar,all=TRUE)

###SLR
###variables collected on individual blocks to site level
dslr<-dvwc2[block=="f"|block=="j",]
setkey(dslr, site,time2)
key2<- c("time2","slr_mj_tot","slr_w_avg","site")
dslr<-subset(dslr, select=key2)
setnames(dslr, "slr_mj_tot", "dd_slr_tot")
setnames(dslr, "slr_w_avg", "dd_slr_w_avg")
dslr<- unique(dslr) ##needed to remove double rows
setkey(dvwc2, site,time2)
setkey(dslr, site,time2)
dvwc2<-merge(dvwc2, dslr,all=TRUE)

###RAIN
###variables collected on individual blocks to site level
drain<-dvwc2[block=="d"|block=="j",]
setkey(drain, site, time2)
key2<- c("time2","rain_in_tot","site")
drain<-subset(drain, select=key2)
setnames(drain, "rain_in_tot", "dd_rain")
drain<- unique(drain) ##needed to remove double rows
drain[,dd_rain:= dd_rain/2]
setkey(drain,site,time2)
setkey(dvwc2,site,time2)
dvwc2<-merge(drain, dvwc2,all=TRUE)

# #WIND
# dwind<-dvwc2[block=="e"|block=="l",]
# setkey(dwind, site, time2)
# key2<- c("time2", "ws_ms_s_wvt", "winddir_d1_wvt","winddir_sd1_wvt","ws_ms_avg","site")
# dwind<-subset(dwind, select=key2)
# setnames(dwind, "ws_ms_s_wvt", "dd_ws_ms_s_wvt")
# setnames(dwind, "winddir_d1_wvt", "dd_winddir_d1_wvt")
# setnames(dwind, "winddir_sd1_wvt", "dd_winddir_sd1_wvt")
# setnames(dwind, "ws_ms_avg", "dd_ws_ms_avg")

# dwind<- unique(dwind) ##needed to remove double rows
# setkey(dvwc2, site,time2)
# setkey(dwind, site,time2)
# dt2<-dwind[dt2]
# rm(dwind)


write.table(dvwc2,sec8, append = FALSE, quote = FALSE, sep = ",",
           , na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))

dvwc2<-dvwc2[is.na(time2)==FALSE,] ### what does this do?

###Calculate VPD hourly?
###################################################
#####     Kirk R. Wythers 2014.09.12          #####
##### R script to calculate vapor pressure    #####
##### deficits from vapor pressure air        #####
##### and rel humidity                        #####
###################################################

# saturation pressure (es1) function, from Dennis Hartman "Global Physical Climatology" (p 350)
# get.es1 <- function(temp){
#   es1 <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + temp))) ## millibars (mb)
#   return(es1)
# }
# 
# get.vpd1 <- function(rh, temp){
#   ## calculate saturation vapor pressure
#   es1 <- get.es1(temp)
#   ## calculate vapor pressure deficit
#   vpd1 <- ((100 - rh) / 100) * es1
#   return(vpd)
# }

#### Calculate vapor pressure deficit, from Microclimate 2nd ed. Rossenburg (pg. 170)
dvwc2[, dd_rh2:= ifelse(dd_rh>100, 100, dd_rh)]
dvwc2[, dd_rh2:= ifelse(dd_rh2<0,0, dd_rh2)]

dvwc2[,dd_es:= 0.61078 * exp(17.269 * dd_airtc / (dd_airtc + 237.3))] ##  saturated vapor pressure in kilopascals (kPa)
dvwc2[,dd_ea:= dd_rh2 / 100 * dd_es] ## vapor pressure air in kilopascals (kPa)     
dvwc2<-dvwc2[,dd_vpd:= dd_es - dd_ea] ## vapor pressure deficit in kilopascals (kPa)
 
write.table(dvwc2, sec9, append = FALSE, quote = FALSE, sep = ",",
           , na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))
          

###################################################
#####     Roy L RICH 2014.10.22               #####
##### R script to fix incoming climate data   #####
##### based on RAWS station FRNM5 and SNWM5   #####
##### collect airtemp and  RH values          #####
###################################################
library(lubridate)
library(data.table)

raws<-"/Users/rich0475/Documents/B4W_data/RAWS_update_2014.dat" ###data file
pathout<-"/Users/rich0475/Documents/B4W_data/sup_climate_raws_2014.dat" # raws is the climate mesonet data from fire station monitoring network
###convert time zone
dt<-fread(raws)
dt$time2<-gsub("T"," ", dt$time_use)
Sys.setenv(TZ="America/Regina")
dt$time2<-as.POSIXct(dt$time2)
    dt2<-round(minute(dt$time2)/60)*60 # rounds to the nearest 60 minute timestamp (nearest hour)
    dt$time3<-update(dt$time2, min=dt2)
dt$time4<-dt$time3
dt$time4<-as.POSIXct(dt$time4)
dt$time4<-force_tz(dt$time4, tzone="America/Chicago") # daylight savings
dt[,time5:= with_tz(time4, tzone="America/Regina")] # central standard all year

###modify columns for use with derived data
d<-names(dt)

# > d
 # [1] "time_textv1" "TMP_F"       "RELH%"       "SKNT_mph"    "GUST_mph"    "DRCT"        "PRESin"      "ALTIin"      "SOLR_W/m*m"  "TLKE_F"      "PREC_in"     "SINT_in"    
# [13] "FT_F"        "FMgm"        "PEAK_mph"    "PDIR"        "VOLT volt"   "DWP_F"       "site"        "time"        "year"        "temp_C"      "week"        "doy"        
# [25] "Notes_2_2"   "time_zone"   "time_use"    "time2"       "time3"       "time4"       "time5"      

keep<-c("TMP_F","RELH%","SKNT_mph","GUST_mph","DRCT","PRESin" ,"ALTIin" ,"SOLR_W/m*m","TLKE_F","PREC_in", "SINT_in","FT_F", "FMgm" , "PEAK_mph","PDIR", "VOLT volt",   "DWP_F","site","temp_C", "time5" ) 

dt2<-subset(dt, select=keep)
dt2$hour<-hour(dt2$time5)
dt2$doy<-yday(dt2$time5)
dt2$year<-year(dt2$time5)
setnames(dt2, "RELH%","sup_rh" )
#### Calculate vapor pressure deficit, from Microclimate 2nd ed. Rossenburg (pg. 170)
dt2[, sup_rh2:= ifelse(sup_rh>100, 100, sup_rh)]
dt2[, sup_rh2:= ifelse(sup_rh<0,0, sup_rh)]
setnames(dt2, "temp_C","sup_airtc" )
dt2[,sup_es:= 0.61078 * exp(17.269 * sup_airtc / (sup_airtc + 237.3))] ##  saturated vapor pressure in kilopascals (kPa)
dt2[,sup_ea:= sup_rh2 / 100 * sup_es] ## vapor pressure air in kilopascals (kPa)     
dt2<-dt2[,sup_vpd:= sup_es - sup_ea] ## vapor pressure deficit in kilopascals (kPa)
 
write.table(dt2, pathout, append = FALSE, quote = FALSE, sep = ",",
           , na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))
          
    
