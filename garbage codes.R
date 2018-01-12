require(data.table)
require(reshape2)
require(dplyr)
require(stringr)
require(ggplot2)
require(HMDHFDplus)

### Death data - WHO - Cause of Death

# these files are available from http://www.who.int/healthinfo/statistics/mortality_rawdata/en/), 

file1 = fread("c:/r/who/Morticd10_part1")
file2 = fread("c:/r/who/Morticd10_part2")

all_data = rbind(file1,file2)
focus_data = all_data[Country %in% c(1430) & Cause != "AAA"]
focus_data = focus_data[,c(1,4,7,10:35),with=F]
focus_data = focus_data %>% melt(id.vars=c("Country", "Year", "Sex")) %>% data.table()

# build a look-up table to deal with formatting issues

variable_names = focus_data[,.(variable = unique(variable))]
variable_names[,group := as.integer(str_replace(variable, "Deaths", ""))]
variable_names=variable_names[group != 1]
variable_names=variable_names[group != 26]
variable_names[,group:=group-2]
variable_names[group ==0,age_lower:=0]
variable_names[group <=4 & group >0,age_lower:=1]
variable_names[group >4,age_lower:=5+(group-5)*5]
variable_names[group ==0,age_group:=paste0(0, "-", 1)]
variable_names[group <=4 & group >0,age_group:=paste0(1, "-", 4)]
variable_names[group >4,age_group:=paste0(age_lower, "-", age_lower+4)]

focus_data %>% setkey(variable)
variable_names %>% setkey(variable)
focus_data = focus_data %>% merge(variable_names)

focus_data = all_data[Country %in% c(1430, 2450,4308) & Cause != "AAA"]
focus_data = focus_data[,c(1,4,7,10:35,6),with=F]
focus_data = focus_data %>% melt(id.vars=c("Country", "Year", "Sex", "Cause")) %>% data.table()

SA_causes = focus_data[Country == 1430][!is.na(value),.(Deaths = sum(value)), by=Cause][order(-Deaths)] %>% setkey(Cause)

# file is uploaded to github

codes = fread("C:/Users/User/Dropbox/traffic_deaths/icd10 2 letter.csv")
codes[,Cause:=Code]
codes$Code = NULL
codes %>% setkey(Cause)
SA_causes = merge(SA_causes,codes)
SA_causes[order(-Deaths)][1:20] %>% write.table("c:/r/sa_causes.csv", sep=",",row.names=F)