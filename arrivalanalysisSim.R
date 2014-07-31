arrfunction <- function(x,replication = 1,name = '') {
library(plyr)
library(chron)
library(reshape2)
library(ggplot2)

#create data frame with data and time descriptors
x <- sort(x)
x <- data.frame(x,years(x),months(x),days(x),weekdays(x),hours(x),minutes(x))
names(x) = c("Timestamp","Year","Month","Day","Weekday","Hour","Minute")

#function for calculating number of specific weekdays (M,T,etc.) between dates
nweekdays <- function(startdate,enddate,weekdayx) {
datediff <- as.numeric(as.Date(enddate,tz = '')-as.Date(startdate),tz = '')
print(sum(format(as.Date(startdate) + 0:datediff,'%w') == weekdayx))
}

#calculate count for each weekday
ndays <- c()
for (i in 0:6) {
    ndays <- rbind(ndays,nweekdays(x[1,1],x[nrow(x),1],i))
}
ndays <- data.frame(Weekday = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),ndays)

#plot arrivals by month
countm <- ddply(x,.(Month),summarize,Count = length(Month),Mean = length(Month))
countm$Mean <- countm$Mean/replication
countm <- data.frame(countm[order(factor(countm$Month,levels = month.name[month.name %in% countm$Month])),],
                     month.id = 1:nrow(countm))

tiff(paste('PatientArrivalsM',ifelse(name != '','.',''),name,'.tiff',sep = ''),1200,1200)
    print(
        ggplot(countm,aes(x = reorder(Month,month.id),y = Mean)) + geom_bar(stat = 'identity',fill = 2) +
        xlab("Month") + ylab("Mean Patient Arrivals") #+ scale_x_discrete(breaks = countm$month.id,labels=countm$Month)
    )
dev.off()

#plot arrivals by weekday
wdlist <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
countwd <- ddply(x,.(Weekday),summarize,Count = length(Weekday),Mean = length(Weekday))
countwd$Mean <- countwd$Mean/(ndays$ndays*replication)
countwd <- data.frame(countwd[order((factor(countwd[,1],levels = wdlist))),],wd.id = 1:nrow(countwd))

tiff(paste('PatientArrivalsWD',ifelse(name != '','.',''),name,'.tiff',sep = ''),1200,1200)
    print(
        ggplot(countwd,aes(x = reorder(Weekday,wd.id),y = Mean)) + geom_bar(stat = 'identity',fill = 3) +
        xlab("Weekday") + ylab("Mean Patient Arrivals")
    )
dev.off()

#plot arrivals by hour of day
counth <- ddply(x,.(Hour),summarize,Count = length(Hour),Mean = length(Hour))
counth$Mean <- counth$Mean/(as.numeric(as.Date(x[nrow(x),1],tz = '')-as.Date(x[1,1],tz = ''))*replication)

tiff(paste('PatientArrivalsH',ifelse(name != '','.',''),name,'.tiff',sep = ''),1200,1200)
    print(
        ggplot(counth,aes(x = Hour,y = Mean)) + geom_bar(stat = 'identity',fill = 4) + ylab('Mean Patient Arrivals')
    )
dev.off()

#plot by hour of day with weekday series
countwdh <- ddply(x,.(Weekday,Hour),summarize,Count = length(Hour),Freq = length(Hour))
countwdh <- merge(countwdh,ndays,by = 'Weekday')
countwdh$Freq <- countwdh$Freq/(countwdh$ndays*replication)

tiff(paste('PatientArrivalsWDbyH',ifelse(name != '','.',''),name,'.tiff',sep = ''),1200,1200)
print(ggplot(countwdh,aes(x = Hour,y = Freq,colour = factor(Weekday))) + geom_line(lwd = 2) +
    labs(colour = 'Weekdays') + scale_colour_discrete(breaks = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")))
dev.off()
}