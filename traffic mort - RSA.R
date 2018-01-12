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

# coding as per CDC

coding = fread("c:/r/motor.csv", header=T)
coding = rbind(coding, data.frame(Code = c("V89","V09", "V19")))

focus_data[Cause %in% coding$Code, Type := "Traffic"]
focus_data[!Cause %in% coding$Code, Type := "Non-Traffic"]
focus_data[,Major:=str_sub(Cause,1,1)]

focus_data[Major == "V"][,sum(Deaths1), keyby = .(Type, Cause)]

focus_data = focus_data[,c(1,4,7,10:35,40),with=F]
focus_data = focus_data %>% melt(id.vars=c("Country", "Year", "Sex", "Type")) %>% data.table()

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
death_data_cause = focus_data[,.(Deaths = sum(value)), keyby = .(Country, Year, Sex, Type, age_lower, age_group )]
death_data_total = death_data_cause[,.(Deaths = sum(Deaths)), keyby = .(Country, Year, Sex, age_lower, age_group )]
death_data_total[, Type:="Total"]

death_data = rbind(death_data_cause, death_data_total)
death_data = death_data[Sex !=9]

death_data[Type != "Total"] %>% ggplot(aes(x=age_lower, y = Deaths, colour = Year)) + geom_point(aes(colour = Year))+ facet_wrap(Sex~Type, scales = "free_y")+
  geom_line(aes(group = Year))

death_data_factors = death_data %>% dcast.data.table(Country + Year + Sex + age_group+ age_lower~Type, value.var = "Deaths")
death_data_factors[,reduction := 1-`Non-Traffic`/Total]
death_data_factors[,Country_Name := "South Africa"]

death_data_factors %>% ggplot(aes(x=age_lower, y = reduction, colour = Year)) + geom_point(aes(colour = Year))+ facet_grid(Sex~Country_Name)+
  geom_line(aes(group = Year))

### check age pattern of deaths

age_pattern = death_data_cause[Type == "Traffic"] %>% setkey(Country, Year, Sex, Type, age_lower, age_group)
age_pattern[,total_deaths:=sum(Deaths),keyby = .(Country, Year, Sex)]
age_pattern[,propn_deaths:=Deaths/total_deaths]
age_pattern = age_pattern[,c(2,3,5,9),with=F]
age_pattern %>% setkey(Year, Sex, age_lower)

focus_data = all_data[Country %in% c(1430, 2450,4308) & Cause != "AAA"]
focus_data = focus_data[,c(1,4,7,10:35,6),with=F]
focus_data = focus_data %>% melt(id.vars=c("Country", "Year", "Sex", "Cause")) %>% data.table()
focus_data[Cause %in% coding$Code, Type := "Traffic"]
focus_data[!Cause %in% coding$Code, Type := "Non-Traffic"]

SA_causes = focus_data[Country == 1430][!is.na(value),.(Deaths = sum(value)), by=Cause][order(-Deaths)] %>% setkey(Cause)
codes = fread("C:/Users/User/Dropbox/paper/data/icd10 2 letter.csv")
codes[,Cause:=Code]
codes$Code = NULL
codes %>% setkey(Cause)
SA_causes = merge(SA_causes,codes)
SA_causes[order(-Deaths)][1:20] %>% write.table("c:/r/sa_causes.csv", sep=",",row.names=F)
SA_causes[,Major:=str_sub(Cause,1,1)]
SA_causes[Major == "V"][order(-Deaths)]%>% write.table("c:/r/sa_causes_traffic.csv", sep=",",row.names=F)

focus_data = focus_data[Type != "Traffic"]

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

focus_data = focus_data[!is.na(value),.(Deaths = sum(value)), keyby = .(Country, Year, Sex, Cause, age_lower, age_group )]
focus_data[,total_deaths_Cause:=sum(Deaths),keyby = .(Country, Year, Sex, Cause)]
focus_data[,propn_deaths_Cause:=Deaths/total_deaths_Cause]
focus_data = focus_data[Sex !=9]

focus_data %>% setkey(Year, Sex, age_lower)
focus_data = merge(focus_data, age_pattern)
focus_data[,.(Deaths = sum(Deaths), V1 = sum(abs(propn_deaths_Cause-propn_deaths)^2)), keyby = .(Country, Cause)][!is.na(V1)& Deaths>50][order(V1)][1:100]


focus_data[Country == 1430 & Cause == "Y34"] %>% ggplot(aes(x=age_lower, y = propn_deaths_Cause, colour = Year)) + facet_grid(Sex~.)+
  geom_line(aes(group = Year)) +
  geom_point(data = focus_data[Country == 1430 & Cause == "Y34"],aes(x=age_lower, y = propn_deaths, colour = Year, Group = Year))

### Compare totals to other sources

summary = death_data[!is.na(Deaths) & Type == "Traffic",.(Deaths = sum(Deaths)), keyby = .(Year)]
rtmc = fread("c:/users/user/dropbox/traffic_deaths/rtmc stats.csv") %>% setkey(Year)
rtmc = merge(summary,rtmc)
rtmc[,propn:=Deaths/`Crash Fatalities`]

death_data_factors = death_data %>% dcast.data.table(Country + Year + Sex + age_group+ age_lower~Type, value.var = "Deaths")
death_data_factors[,reduction := 1-`Non-Traffic`/Total]
death_data_factors[,Country_Name := "South Africa"]

death_data_factors %>% ggplot(aes(x=age_lower, y = reduction, colour = Year)) + geom_point(aes(colour = Year))+ facet_grid(Sex~Country_Name)+
  geom_line(aes(group = Year))

### derive mortality rates

all_death_dat %>% setkey(Country, Year, Sex, Age, age_group)
pop_data %>% setkey(Country, Year, Sex, Age, age_group)

mortality_rates = merge(pop_data, all_death_dat, all.x=T,all.y=T)

mortality_rates[is.na(Deaths)][,c(1:2),with=F] %>% unique
mortality_rates[is.na(Population)][,c(1:2),with=F] %>% unique

mortality_rates = merge(pop_data, all_death_dat)
mortality_rates[,mx:= Deaths/Population]
mortality_rates[,mx_no_traffic:= Deaths/Population*(1-reduction)]

mortality_rates[,qx:= 1-exp(-mx)]
mortality_rates[,qx_no_traffic:= 1-exp(-mx_no_traffic)]

mortality_rates[is.na(qx), qx:=1]
mortality_rates[is.na(qx_no_traffic), qx_no_traffic:=1]

mortality_rates[,Year_centre := trunc(Year/5)*5+2]
mortality_rates = mortality_rates[,.(mx = mean(mx), qx = mean(qx), qx_no_traffic = mean(qx_no_traffic)), keyby = .(Country, Year_centre, Sex, Age, age_group)]

mortality_rates[,px:=1-qx]
mortality_rates[,px_no_traffic:=1-qx_no_traffic]
mortality_rates[,tpx := c(1,cumprod(px)), by = .(Country, Year_centre, Sex)]
mortality_rates[,tpx_no_traffic := c(1,cumprod(px_no_traffic)), by = .(Country, Year_centre, Sex)]

mortality_rates[,Lx:=(tpx+lead(tpx))/2,by = .(Country, Year_centre, Sex)]
mortality_rates[,Lx:=ifelse(is.na(Lx),0,Lx),by = .(Country, Year_centre, Sex)]
mortality_rates[,Tx:=(rev(cumsum(rev(Lx)))),by = .(Country, Year_centre, Sex)]
mortality_rates[,ex:=Tx/Lx]

mortality_rates[,Lx_no_traffic:=(tpx_no_traffic+lead(tpx_no_traffic))/2,by = .(Country, Year_centre, Sex)]
mortality_rates[,Lx_no_traffic:=ifelse(is.na(Lx_no_traffic),0,Lx_no_traffic),by = .(Country, Year_centre, Sex)]
mortality_rates[,Tx_no_traffic:=(rev(cumsum(rev(Lx_no_traffic)))),by = .(Country, Year_centre, Sex)]
mortality_rates[,ex_no_traffic:=Tx_no_traffic/Lx_no_traffic]

mortality_rates[,Country_Name := ifelse(Country == 2450, "USA", "UK")]
mortality_rates %>% ggplot(aes(x=Age, y=log(qx))) + geom_line(aes(colour = Year_centre, group = Year_centre))+ facet_wrap(Sex~Country_Name)

mortality_rates %>% ggplot(aes(x=Age, y=log(qx))) + 
  geom_line(aes(group = Year_centre))+ 
  facet_grid(Country_Name +Sex~Year_centre)+
  geom_line(aes(colour = "No Motor", group = Year_centre, x=Age, y= log(qx_no_traffic)), data =mortality_rates )

e0 = mortality_rates[Age == 0][,c("Country_Name", "Sex", "Year_centre", "ex", "ex_no_traffic"),with=F][,Increase:=ex_no_traffic-ex] %>% setkey(Country_Name, Sex, Year_centre)
e0 %>% write.table("eo.csv", sep=",", row.names=F)

### HMD Birth data

USA = HMDHFDplus::readHMDweb(CNTRY = "USA", username = username, password = password, item = "Births")   %>% data.table

#number of years gained - USA MAles

USA[Year == 2012]$Male * e0[Year_centre==2012 & Country_Name == "USA" & Sex == 1]$Increase