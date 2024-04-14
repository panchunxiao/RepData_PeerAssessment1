data <- read.csv("activity.csv")

#统计每天的步数。用tapply。
stepsum <- tapply(data$steps, data$date , sum)#tapply的第二个参数data$date是把date的数据作为factors，根据factors对数据进行subset，然后再sum。
hist(stepsum,breaks = 30)
mean(stepsum, na.rm = TRUE) 
median(stepsum,na.rm=TRUE)
hist()
View(data)


plot(data$interval, data$steps,type = "l")

stepave<-aggregate(steps~interval,data = data,FUN = mean,na.rm=TRUE)
plot(stepave$interval,stepave$steps, type = "l")
stepave[which.max(stepave$steps),]

sum(is.na(data$steps))


#replace the NA with mean values
data2 <- data
for(x in 1:17568) {
  if(is.na(data2[x, 1])==TRUE) {
    data2[x, 1] <- stepave[stepave$interval %in% data2[x, 3], 2]
  }
}

stepsum2 <- tapply(data2$steps, data$date , sum)
hist(stepsum2, breaks=30)
mean(stepsum2)
median(stepsum2)

data2$date<-as.Date(data2$date)
library(ggplot2)
isweekday<-function(a){
  if (weekdays(a) %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    return("weekday")
}
data2$daytype<-mapply(isweekday,data2$date)
ggplot(cleardate)+geom_point(aes(x=interval,y=steps,colour=daytype))



