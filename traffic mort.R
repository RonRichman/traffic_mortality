require(data.table)
require(reshape2)
require(dplyr)
require(stringr)
require(ggplot2)
require(HMDHFDplus)

source("passwords.R")

### Death data - WHO - Cause of Death

# these files are available from http://www.who.int/healthinfo/statistics/mortality_rawdata/en/), 

file1 = fread("c:/r/who/Morticd10_part1")
file2 = fread("c:/r/who/Morticd10_part2")

all_data = rbind(file1,file2)
focus_data = all_data[Country %in% c(2450,4308) & Cause != "AAA"]

# coding as per CDC

coding = fread("c:/r/motor.csv", header=T)

focus_data[Cause %in% coding$Code, Type := "Traffic"]
focus_data[!Cause %in% coding$Code, Type := "Non-Traffic"]
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
focus_data = focus_data[,.(Deaths = sum(value)), keyby = .(Country, Year, Sex, Type, age_lower, age_group )]
death_data_cause = data.table::copy(focus_data)
death_data_total = death_data_cause[,.(Deaths = sum(Deaths)), keyby = .(Country, Year, Sex, age_lower, age_group )]
death_data_total[, Type:="Total"]

death_data = rbind(death_data_cause, death_data_total)
death_data = death_data[Sex !=9]

### Compare totals to other sources

# death_data[Type == "Traffic",sum(Deaths), keyby = .(Country, Year)]

death_data_factors = death_data %>% dcast.data.table(Country + Year + Sex + age_group+ age_lower~Type, value.var = "Deaths")
death_data_factors[,reduction := 1-`Non-Traffic`/Total]
death_data_factors[,Country_Name := ifelse(Country == 2450, "USA", "UK")]

death_data_factors %>% ggplot(aes(x=age_lower, y = reduction, colour = Year)) + geom_point(aes(colour = Year))+ facet_grid(Sex~Country_Name)+
  geom_line(aes(group = Year))

### HMD Population data

USA = HMDHFDplus::readHMDweb(CNTRY = "USA", username = username, password = password, item = "Population")   %>% data.table
UK = HMDHFDplus::readHMDweb(CNTRY = "GBR_NP", username = username, password = password, item = "Population")   %>% data.table
USA[, Country := 2450]
UK[, Country := 4308]
pop = rbind(USA,UK)

pop[Age==0, age_group := paste0(0, "-", 1)]
pop[Age < 5 & Age>0, age_group := paste0(1, "-", 4)]
pop[Age >= 5,age_group:=paste0(trunc(Age/5)*5, "-", trunc(Age/5)*5+4)]
pop[Age >= 100,age_group:=paste0(95, "-", 99)]
pop_data = pop[Year>=1999,.(Male = sum(Male1), Female = sum(Female1)), keyby= .(Country, Year, Age, age_group)] %>% 
  melt(id.vars = c("Country", "Year", "Age", "age_group")) %>% data.table()

pop_data[,Sex:=ifelse(variable == 'Male',1,2)]
pop_data[,Population:=value]
pop_data[,variable:=NULL]
pop_data[,value:=NULL]

### HMD Death Data

USA = HMDHFDplus::readHMDweb(CNTRY = "USA", username = username, password = password, item = "Deaths_1x1")   %>% data.table
UK = HMDHFDplus::readHMDweb(CNTRY = "GBR_NP", username = username, password = password, item = "Deaths_1x1")   %>% data.table
USA[, Country := 2450]
UK[, Country := 4308]
deaths = rbind(USA,UK)

deaths[Age==0, age_group := paste0(0, "-", 1)]
deaths[Age < 5 & Age>0, age_group := paste0(1, "-", 4)]
deaths[Age >= 5,age_group:=paste0(trunc(Age/5)*5, "-", trunc(Age/5)*5+4)]
deaths[Age >= 100,age_group:=paste0(95, "-", 99)]
deaths_data = deaths[Year>=1999,.(Male = sum(Male), Female = sum(Female)), keyby= .(Country, Year, Age, age_group)] %>% 
  melt(id.vars = c("Country", "Year", "Age", "age_group")) %>% data.table()

deaths_data[,Sex:=ifelse(variable == 'Male',1,2)]
deaths_data[,Deaths:=value]
deaths_data[,variable:=NULL]
deaths_data[,value:=NULL]

deaths_data %>% setkey(Country, Year, Sex, age_group)
death_data_factors %>% setkey(Country, Year, Sex, age_group)

all_death_dat = death_data_factors %>% merge(deaths_data)

data_check = all_death_dat[,sum(Deaths), by = .(Country, Year, Sex, age_group,Total)]
data_check[,ratio:=V1/Total]
data_check[abs(1-ratio)>.05] 


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