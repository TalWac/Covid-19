#COVID19-plots

library(stringr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(directlabels)

confirm=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

dead=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

recover=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

country <- ifelse(is.na(confirm[,1])==TRUE, "", paste(confirm[,2], confirm[,1]))
country=str_squish(country)
confirm.1=data.frame(country, confirm[,5:ncol(confirm)], status="confirm")

country.dead <- ifelse(is.na(dead[,1])==TRUE, "", paste(dead[,2], dead[,1]))
country.dead=str_squish(country.dead)
dead.1=data.frame(country.dead, dead[,5:ncol(dead)], status="dead")

country.recover <- ifelse(is.na(recover[,1])==TRUE, "", paste(recover[,2], recover[,1]))
country.recover=str_squish(country.recover)
recover.1=data.frame(country.recover, recover[,5:ncol(recover)], status="recover")

names(dead.1)=names(recover.1)=names(confirm.1)

merge=rbind(confirm.1, dead.1, recover.1)

merge.melt=melt(merge, id.vars=c("country", "status"))
merge.melt$log_val=log(merge.melt$value, base = exp(1))
merge.melt[mapply(is.infinite, merge.melt)] <- 0
merge.melt$variable=sub("X", "", merge.melt$variable)
merge.melt$variable=as.Date(merge.melt$variable, format='%m.%d.%y')
colnames(merge.melt)=c("Country", "Status", "Date", "Cases", "log_cases")

head(merge.melt)
#merge.melt[merge.melt[,1]=="US",]
my_con=c("Italy", "Israel", "US")
merge.melt.con=merge.melt[merge.melt[,1]==my_con,]

#ploting


p=ggplot(merge.melt.con, aes(x=Date, y=log_cases))+geom_point(aes(x=Date, y=log_cases, group=Country, colour=Country), alpha=0.5)+geom_smooth(aes(x=Date, y=log_cases, group=Country, colour=Country), method="loess")+facet_grid(~Status, scales='free_y')+theme_bw()+scale_x_date(expand = c(.1, .1))

direct.label(p, method="last.points")


















