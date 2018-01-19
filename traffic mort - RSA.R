require(data.table)
require(reshape2)
require(dplyr)
require(stringr)
require(ggplot2)
require(plyr)

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

### Derive the number of deaths due to motor accident reported in the WHO data

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

ggsave("c:/r/reduction_pre_corr.jpg")


### check age pattern of deaths

age_pattern = death_data_cause[Type == "Traffic"] %>% setkey(Country, Year, Sex, Type, age_lower, age_group)
age_pattern[,total_deaths:=sum(Deaths),keyby = .(Country, Year, Sex)]
age_pattern[,propn_deaths:=Deaths/total_deaths]
age_pattern = age_pattern[,c(2,3,5,9),with=F]
age_pattern %>% setkey(Year, Sex, age_lower)
age_pattern[Sex != 9] %>% ggplot(aes(x=age_lower, y=propn_deaths, colour = Year)) + facet_grid(Sex~.)+
  geom_line(aes(group = Year)) 

ggsave("c:/r/agepattern.jpg")

focus_data = all_data[Country %in% c(1430, 2450,4308) & Cause != "AAA"]
focus_data = focus_data[,c(1,4,7,10:35,6),with=F]
focus_data = focus_data %>% melt(id.vars=c("Country", "Year", "Sex", "Cause")) %>% data.table()
focus_data[Cause %in% coding$Code, Type := "Traffic"]
focus_data[!Cause %in% coding$Code, Type := "Non-Traffic"]

focus_data = focus_data[Type != "Traffic"]

focus_data %>% setkey(variable)
variable_names %>% setkey(variable)

focus_data = focus_data %>% merge(variable_names)

focus_data = focus_data[!is.na(value),.(Deaths = sum(value)), keyby = .(Country, Year, Sex, Cause, age_lower, age_group )]
focus_data[,total_deaths_Cause:=sum(Deaths),keyby = .(Country, Year, Sex, Cause)]
focus_data[,propn_deaths_Cause:=Deaths/total_deaths_Cause]
focus_data = focus_data[Sex !=9]

focus_data %>% setkey(Year, Sex, age_lower)
focus_data = merge(focus_data, age_pattern)
top20 = focus_data[,.(Deaths = sum(Deaths), distance = sqrt(sum(abs(propn_deaths_Cause-propn_deaths)^2))), keyby = .(Country, Cause)][!is.na(distance)& Deaths>10][order(distance)][1:20] 
top20%>%   fwrite("c:/r/closest_match.csv")


focus_data[Country == 1430 & Cause == "Y34"] %>% ggplot(aes(x=age_lower, y = propn_deaths_Cause, colour = Year)) + facet_grid(Sex~.)+
  geom_line(aes(group = Year)) +
  geom_point(data = focus_data[Country == 1430 & Cause == "Y34"],aes(x=age_lower, y = propn_deaths, colour = Year))

ggsave("c:/r/agepattern_match.jpg")

### Compare totals to other sources

summary = death_data[!is.na(Deaths) & Type == "Traffic",.(Deaths = sum(Deaths)), keyby = .(Year)]
rtmc = fread("c:/users/user/dropbox/traffic_deaths/rtmc stats.csv") %>% setkey(Year)
rtmc = merge(summary,rtmc)
rtmc[,propn:=Deaths/`Crash Fatalities`]

rtmc %>% fwrite("c:/r/rtmc.csv")

### Correct the WHO deaths using the RTMC reported deaths

death_data_factors = death_data[!is.na(Deaths)] %>% dcast.data.table(Country + Year + Sex + age_group+ age_lower~Type, value.var = "Deaths")

rtmc %>%  setkey(Year)
death_data_factors %>% setkey(Year)
death_data_factors = death_data_factors %>% merge(rtmc)

death_data_factors[,extra := Traffic/propn - Traffic]
death_data_factors[,Traffic := Traffic + extra]
death_data_factors[,`Non-Traffic` := `Non-Traffic` - extra]

death_data_factors[,reduction := 1-`Non-Traffic`/Total]
death_data_factors[,Country_Name := "South Africa"]

death_data_factors %>% ggplot(aes(x=age_lower, y = reduction, colour = Year)) + geom_point(aes(colour = Year))+ facet_grid(Sex~Country_Name)+
  geom_line(aes(group = Year))

ggsave("c:/r/sa_reduction.jpg")


### derive mortality rates


thembisa = fread("c:/r/thembisa.csv", header=T) %>% melt(id.vars = c("Sex", "Age"))
thembisa[,Year:=as.integer(as.character(variable))]
thembisa[,qx:=value]
thembisa[,variable:=NULL]
thembisa[,value:=NULL]
thembisa[,mx:=-log(1-qx)]
thembisa[,log_mx:=log(mx)]

extend_gompertz = function(dat){
  dat = dat %>% data.table()
  dat = dat[Age >= 80]
  fit = lm(log_mx~Age,data=dat)
  pred_dat = expand.grid(Age = seq(90,110), Year =dat$Year[1], Sex = dat$Sex[1])%>% data.table()
  pred_dat$log_mx = predict(fit, pred_dat) 
  pred_dat[,mx := exp(log_mx)]
  pred_dat[,qx := 1- exp(-mx)]
  return(pred_dat)
}

to_110 = dlply(thembisa, .(Year,Sex), extend_gompertz) %>% rbindlist()
thembisa = rbind(thembisa[Age<91], to_110) %>% setkey(Year, Sex, Age)

thembisa[Age==0, age_group := paste0(0, "-", 1)]
thembisa[Age < 5 & Age>0, age_group := paste0(1, "-", 4)]
thembisa[Age >= 5,age_group:=paste0(trunc(Age/5)*5, "-", trunc(Age/5)*5+4)]
thembisa[Age >= 100,age_group:=paste0(95, "-", 99)]

death_data_factors = death_data_factors[,c(1,3,4,13),with=F] %>% setkey(Year, Sex, age_group)
thembisa %>% setkey( Year, Sex,  age_group)

mortality_rates = merge(thembisa,death_data_factors)

mortality_rates[,qx_no_traffic:= (1-reduction)*qx]

mortality_rates[,px:=1-qx]
mortality_rates[,px_no_traffic:=1-qx_no_traffic]
mortality_rates[,tpx := c(1,cumprod(px)), by = .(Year, Sex)]
mortality_rates[,tpx_no_traffic := c(1,cumprod(px_no_traffic)), by = .(Year, Sex)]

mortality_rates[,Lx:=(tpx+lead(tpx))/2,by = .(Year, Sex)]
mortality_rates[,Lx:=ifelse(is.na(Lx),0,Lx),by = .(Year, Sex)]
mortality_rates[,Tx:=(rev(cumsum(rev(Lx)))),by = .(Year, Sex)]
mortality_rates[,ex:=Tx/Lx]

mortality_rates[,Lx_no_traffic:=(tpx_no_traffic+lead(tpx_no_traffic))/2,by = .(Year, Sex)]
mortality_rates[,Lx_no_traffic:=ifelse(is.na(Lx_no_traffic),0,Lx_no_traffic),by = .(Year, Sex)]
mortality_rates[,Tx_no_traffic:=(rev(cumsum(rev(Lx_no_traffic)))),by = .(Year, Sex)]
mortality_rates[,ex_no_traffic:=Tx_no_traffic/Lx_no_traffic]

mortality_rates[,Country_Name :=  "South Africa"]
mortality_rates %>% ggplot(aes(x=Age, y=log(qx))) + geom_line(aes(colour = Year, group = Year))+ facet_wrap(Sex~Country_Name)

mortality_rates[Year==2015] %>% ggplot(aes(x=Age, y=log(qx))) + 
  geom_line(aes(group = Year))+ 
  facet_grid(Country_Name +Sex~Year)+
  geom_line(aes(colour = "No Motor", group = Year, x=Age, y= log(qx_no_traffic)), data =mortality_rates[Year==2015] )

ggsave("c:/r/qx.jpg")

e0 = mortality_rates[Age == 0][,c("Country_Name", "Sex", "Year", "ex", "ex_no_traffic"),with=F][,Increase:=ex_no_traffic-ex] %>% setkey(Country_Name, Sex, Year)
e0 %>% write.table("c:/r/eo_sa.csv", sep=",", row.names=F)

# Years of life saved if no traffic accidents. Note that births registered at 90% completeness 

475355 * e0[Sex == 1 & Year==2015]$Increase/.9
468447 * e0[Sex == 2 & Year==2015]$Increase/.9