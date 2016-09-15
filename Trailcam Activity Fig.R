# Code to take summary trailcam data and plot data to show activity patterns from trailcams
# Summer 2016
# Alan Krakauer
# ahkrakauer@ucdavis.edu
# alan.krakauer@gmail.com

library(ggplot2)
library(scales) # not sure if necessary
library(gridExtra) # not sure if necessary
#library(xts)

# Data file
camdat <- read.table("/Users/ahkrakauer/Dropbox/Trail Cam Paper/TrailcamData_Combined.txt", header = TRUE, sep = "\t")
camdat <- data.frame(camdat)
camdat$TOD <- strptime(paste(camdat$Date, camdat$Time), "%m/%d/%Y %H:%M:%S") # Converting to r time
camdat$present[camdat$Males.Present >= 1] <- 1 # Turning Males present into binary
camdat$present[camdat$Males.Present <1] <- 0
camdat$present <- factor(camdat$present) # Forcing males present into a factor
camdat$Day.or.Night <- factor(camdat$Day.or.Night) # Did flash fire (1 = y)
camdat$Male.Display <- factor(camdat$Male.Display) # Were males in display ( 1 = y)
camdat$obstime <- format(camdat$TOD, format = "%H:%M")
camdat$obstime <- as.POSIXct(camdat$obstime, format = "%H:%M")
camdat$date <- as.Date(camdat$TOD)
#camdat$date2 <- format(camdat$TOD, format = "%m-%d")

# make_breaks <- function(strt, hour, interval="day", length.out=1) {
#   strt <- as.POSIXlt(strt - 60*60*24)  # start back one day
#   strt <- ISOdatetime(strt$year+1900L, strt$mon+1L, strt$mday, hour=hour, min=0, sec=0, tz="UTC")
#   seq.POSIXt(strt, strt+(1+length.out)*60*60*24, by=interval)
# }
# 

# hourbreaks <- make_breaks(min(camdat$obstime)+6, hour=0, interval='2 hours', length.out=1)
#hourbreaks <- make_breaks(min(camdat$obstime), interval='2 hours', length.out=1)

p <- ggplot(camdat[camdat$present==1,], aes(date, obstime, color = Day.or.Night, fill = Day.or.Night, alpha = Male.Display, shape = Male.Display)) 
#p <- p +  geom_point(aes(color = camdat$Day.or.Night))
#p <- p +  geom_point() + scale_color_grey(start= 0, end = 0.8)
p <- p +  geom_point() + scale_color_manual(values = c("blue", "orange"))
p <- p+ scale_shape_manual(values = c(1,3))
p <- p+ scale_alpha_manual(values = c(1, 0.5))
#p <- p + scale_color_discrete(drop = TRUE, limits = levels(camdat$Day.or.Night))
p <- p + theme_minimal()
#p<- p + theme(panel.background = element_rect(fill='white', colour='black'))
p <- p + geom_point(position = "jitter")
p <- p + facet_grid(. ~Lek)
p<- p + scale_x_date("", date_breaks = "2 days", labels = date_format("%m-%d"))
#p<- p + scale_y_datetime("", labels = date_format("%H:%M"), breaks = hourbreaks )
p<- p + scale_y_datetime("", date_breaks = "2 hours",labels = date_format("%H:%M"))
p<- p + theme(axis.text.x= element_text(face="bold", color="#333333", size=13, angle = 90))
p <- p + theme(panel.margin.x=unit(0.5, "lines"))
p<- p + theme(legend.position = "none")
#p <- p + theme_bw()

p
#p


# #qplot(x = camdat$TOD, y = camdat$obstime, size = camdat$present, shape = camdat$Lek, color = camdat$present, data= camdat, geom = "point")
# 
# ggsave("first try plot2.png", width = 7, height = 7)
# 
# plot(camdat$obstime, camdat$date, type = "p")
# 
# 
# mnt <- read.table("/Users/ahkrakauer/Dropbox/2016 Field Data/MNT_Trailcam_Summary.txt", header = TRUE, sep = "\t")
# 
# mnt <- data.frame(mnt)
# 
# # mnt$Date <- as.Date(mnt$Date)
# mnt$TOD <- paste(mnt$Date, mnt$Time)
# mnt$TOD <- strptime(paste(mnt$Date, mnt$Time), )
# mnt$present[mnt$Males.Present>0] <- 1
# mnt$present[mnt$Males.Present==0]<- 0
# 
# 
# df.xts <- xts(x= mnt[,c(5:9,14)], order.by = mnt[,"TOD"] )
# 
# hrAvg <- period.apply(df.xts , endpoints(df.xts, on = "hours", 1), function(x) apply(x, 2, mean))
# 
# mnt.hrAvg <- data.frame(datetime = index(hrAvg), hrAvg[,1:5], row.names = NULL)
# 
# p <- ggplot(data = mnt.hrAVG, aes(x=datetime))
# p<- p + geom_bar()
# 
# 
# 
# 
# 
# 
# mnt$DAY <- format(mnt$TOD, "%m-%d")
# mnt$hour<- format(mnt$TOD, "%H")
# mnt$minute<- format(mnt$TOD, "%M")
# mnt$time2 <- as.numeric(mnt$hour)+(as.numeric(mnt$minute)/60)
# #mnt$DAY <- factor(mnt$Day)
# 
# p<- p+ geom_jitter(x= mnt$DAY, y = mnt$time2, size = mnt$Males.Present)
# p<- p+ scale_x_date(labels = date_format("%m-%d"), breaks = date_breaks("1 days"))
# p