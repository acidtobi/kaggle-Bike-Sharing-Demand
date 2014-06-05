require(ggplot2)
require(zoo)

train <- read.csv("/home/tobi/_Projects/[kaggle] Bike Sharing Demand/train.csv",sep=",",header=TRUE)
train$datetime <- strptime(train$datetime, "%Y-%m-%d %H:%M:%S")
train$day <- as.factor(as.character(strptime(train$datetime, "%Y-%m-%d")))
train$weekday <- as.factor(strftime(train$datetime, "%A"))
train$hour <- as.numeric(strftime(train$datetime, "%H"))

all_days = data.frame(day = as.factor(seq(as.Date(train$datetime[1]), as.Date(train$datetime[nrow(train)]), by="day")))
m <- merge(all_days, avg_rentals_per_day, by="day", all.x=T)
m$count <- na.approx(m$count)

rentals_per_weekday <- aggregate(count~weekday, train, mean)

rentals_per_weekday_and_hour <- aggregate(count~weekday+hour, train, mean)
agg <- rentals_per_weekday_and_hour
ggplot(data=agg, aes(x=agg$hour, y=agg$count, colour=as.factor(agg$weekday))) +  geom_line(size=1.5) 

avg_rentals_per_day <- aggregate(count~day, train, mean)
avg_rentals_per_workday <- aggregate(count~day, train[train$workingday==1,], mean)

rentals_per_day <- aggregate(count~day+weekday, train, sum)
ggplot(data=rentals_per_day, aes(x=rentals_per_day$day, y=rentals_per_day$count, colour=as.factor(rentals_per_day$weekday))) +  geom_line(size=1.5) 

t <- train[train$weekday=="Monday" & train$holiday=="0",]
ggplot(data=t, aes(x=t$hour, y=t$count, colour=as.factor(as.character(t$day)))) +  geom_line()

ggplot(data=train, aes(x=train$datetime, y=train$temperature)) +  geom_line()