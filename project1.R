
activity <- read.csv("activity.csv", sep = ",",colClasses = c("numeric","character","numeric"))

activity.date <- as.Date(activity$date, format = "%Y-%m-%d")
activity.interval <- as.factor(activity$interval)

str(activity)

total.steps.day <- sum(activity$steps, na.rm = "TRUE")
total.steps.day

steps.day <- aggregate(steps~date, data =activity, FUN = sum, na.rm= "TRUE")
colnames(steps.day) <-c("date","steps")
head(steps.day)

ggplot(steps.day, aes(x=steps))+
  geom_histogram(fill="red", binwidth=800)+
  labs(title= "Total Number Of Steps Each Day ",
       x= "Number of steps")+
  theme_bw()

steps.day.mean <- mean(steps.day$steps)
steps.day.median <- median(steps.day$steps)

steps.day.mean
steps.day.median

interval.step <- aggregate(steps~interval, data = activity, FUN = mean, na.rm="TRUE")
ggplot(interval.step, aes(x=interval,y=steps))+
  geom_line(color="green",size=0.8)+
  labs(title="Average Daily Activity",x="interval",y="number of steps")+
  theme_bw()

max.interval.step <- interval.step[which.max(interval.step$step),]$interval  
      

Miss.NA <-sum(is.na(activity$steps))

Miss.NA

activity<- merge(activity, interval.step, by="interval", suffixes = c("", ".y"))
 
S.NA <- is.na(activity$steps)
activity$steps[S.NA] <- activity$steps.y[S.NA]

activity <- activity[,c(1:3)]

sum(is.na(activity$steps))

steps.day.noNA <- aggregate(steps ~ date, data = activity, FUN = sum)

ggplot(steps.day.noNA, aes(x=steps))+
  geom_histogram(fill="dark orange", binwidth=800)+
  labs(title="Number Of Steps Each Day (missing value filled)",
       x="Number of steps each day" )+
  theme_bw()

steps.day.noNA.mean<- mean(steps.day.noNA$steps)
steps.day.noNa.median <- median(steps.day.noNA$steps)


day.type <- function(date)  {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday"))
    {
    "weekend"
     } else 
       {
    "weekday"
       }
    }
activity$day.type<- as.factor(sapply(activity$date, day.type))

par(mfrow = c(2, 1))
 for (type in c("weekend", "weekday")) {
   steps.day.type <- aggregate(steps~interval, data = activity, subset=activity$day.type == type, FUN =mean)
   plot(steps.day.type, type = "l", main = type, col="blue", lwd=2)
 }
   
     
     
   
 














