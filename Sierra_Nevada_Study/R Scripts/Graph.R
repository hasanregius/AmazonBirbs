setwd("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data")
dat = read.csv("DecadeTable.csv", header=TRUE)
dat$prev=dat$pos/dat$tot*100 # Creating the prevalence column
dat$zscore=c(forties, fifties, sixties, seventies, eighties, nineties, twothousands)
dat$log.avg.zscore=log10(dat$zscore+1)
dat$log.max.zscore=log10(c(forties, fifties, sixties, seventies, eighties, nineties, twothousands)+1)

library(Hmisc)
par(mar=c(5, 4, 4, 8) + 0.1)
barplot(dat$seki, width=0.8, space=0.225, xlim=c(0,7), ylim=c(0,400), 
        axes=FALSE, xlab="Time Period", ylab="Sample Size", col="grey95", beside=TRUE)
par(new=TRUE)
barplot(dat$tot, width=0.8, space=0.225, xlim=c(0,7), ylim=c(0,400), axes=FALSE, col="grey85")
par(new=TRUE)
mtext(1, at=c(0.5:6.5), line=1, text = c("1900-49", 
      "1950-59", "1960-69", "1971-80", "1981-90", 
      "1991-00", "2000-09"), cex.lab=0.5)
axis(1, at=c(0.5:6.5), labels=FALSE)
axis(2, at=c(0,50,100,150,200,250,300,350,400), 
     labels=c("0","50","100","150","200","250","300","3550", "3600"), line = -1.25)
par(new=TRUE)
plot(dat$X-0.5, dat$prev, type="l", lwd=2, ylab=" ", xlab=" ", ylim=c(0,60), 
     xlim=c(0,7), axes=FALSE, col="darkblue")
axis(4, line=-1, col="darkblue")
mtext("Prevalence", side = 4, line = 1, cex = par("cex.lab"), col="darkblue")
errbar(dat$X-0.5, dat$prev, yplus=dat$upper, yminus=dat$lower, add=T, xlab="", pch="", lwd="1", cap=0.01)
errbar(dat$X-0.5, dat$sekiprev, yplus=dat$sekiupper, yminus=dat$sekilower, add=T, xlab="", pch="", lwd="1", cap=0.01)

data = read.csv("finaldata.csv", header=TRUE)
for (i in 1:1197) {
  if(data[i,14]=="1900s") {data[i,14]="1940s"}
  if(data[i,14]=="1910s") {data[i,14]="1940s"}
  if(data[i,14]=="1920s") {data[i,14]="1940s"}
  if(data[i,14]=="1930s") {data[i,14]="1940s"}
}
z=subset(data, ZEScore!=0)
boxplot(log10(z$ZEScore+1) ~ z$Decade)

z$ZEScore=log10(z$ZEScore+1)
x1=z$ZEScore[z$Decade=="1940s"]
x2=z$ZEScore[z$Decade=="1950s"]
x3=z$ZEScore[z$Decade=="1960s"]
x4=z$ZEScore[z$Decade=="1970s"]
x5=z$ZEScore[z$Decade=="1980s"]
x6=z$ZEScore[z$Decade=="1990s"]
x7=z$ZEScore[z$Decade=="2000s"]
zplot=cbind(x1,x2,x3,x4,x5,x6,x7)

boxplot(x1,x2,x3,x4,x5,x6,x7, 
        names=c("1900-49", "1950-59", "1960-69", "1970-79", "1980-1989", "1990-99", "2000s"),
        col=c("snow3"),
        xlab="Time Period", ylab="Log10 Bd Load (Zoospore Equivalents)")
abline(h = 4, sr)
table(z$ZEScore)

write.csv(z, "positives.csv")
dat$seki = c(0,0,0,0,0,0,3450)
dat$sekiprev = c(0,0,0,0,0,0,1580/3450*100)

# ggplot 2
data = read.csv("/Users/hasansulaeman/Documents/GitHub/Publications/Sierra Nevada Historical Study/3dgraphyoseseki.csv")

ggplot(data, aes(x=year, y=latitude)) + 
  geom_point(aes(color = as.factor(status), shape=group)) + theme_bw() +
  scale_color_manual(values = c("blue", "red")) + 
  scale_x_continuous(breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
  ylab("Latitude") + xlab("Year")

