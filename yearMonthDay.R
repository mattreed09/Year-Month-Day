start = as.Date("2001-01-01")

day=as.numeric(format(start,format="%d"))
month=as.numeric(format(start,format="%m"))
year=as.numeric(format(start,format="%y"))

df=data.frame(date=character(),year=numeric(),day=numeric(),month=numeric())
while(year != 0)
{
	day=as.numeric(format(start,format="%d"))
	month=as.numeric(format(start,format="%m"))
	year=as.numeric(format(start,format="%y"))
	if (day*month == year)
	df=rbind(df, data.frame(date=as.character(start),year=year,month=month,day=day))
	start = start+1
	if (month == 1 & day == 1)print(year)
}
df$date=as.Date(df$date)

df$gap=0
df$gap[2:nrow(df)] = df$date[2:nrow(df)]-df$date[1:(nrow(df)-1)]


subset(df, gap==max(df$gap))



library(ggplot2)


ggplot(((df)), aes(x=year))+
geom_histogram(binwidth=1,color="white")