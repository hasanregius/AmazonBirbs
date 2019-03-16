setwd("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data")
data=read.csv("finaldata.csv", header=TRUE)
library(raster)
library(rgdal)

# Let's take the positives
positives=subset(data, BdStatus==1)

# Let's figure out the first positive
min(positives$Year) #1907
firstpos=cbind(positives[3,6], positives[3,5])
names(firstpos)=c("x", "y")

# Let's turn them into a spatial object
pts = as.data.frame(cbind(positives$Longitude, positives$Latitude, positives$Year, positives$ZEScore))
colnames(pts) = c("x", "y", "year", "dist2first")
coordinates(pts) = ~x+y
distance(firstpos, pts)


spTransform(firstpos)
raster(firstpos)
?raster::pointDistance

a=nrow(pts)
pt=pts[,1:2]
for (i in 1:139) {
  pts[i,]$Zscore = pointDistance(p1=firstpos, p2=pt[i,], lonlat=TRUE)
}
pts$dist2first = pts$dist2first*1000

plot(pts$year, pts$dist2first, xlab="Year", ylab="Distance from the first positive (km)")
abline(lm(pts$dist2first ~ pts$year))
h=lm(pts$dist2first ~ pts$year)
qmap("yosemite")
plot(pts, add=T)
k=spTransform(pts)
ggmap(k)
