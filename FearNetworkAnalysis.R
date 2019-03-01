install.packages('igraph')
install.packages('network') 
install.packages('sna')
install.packages('ndtv')
install.packages('visNetwork')
install.packages('extrafont')
install.packages('png')
library(sna)
library(network)
library(ggplot2)
library(ndtv)
library(visNetwork)
library(igraph)
library(extrafont)
library(png)
library(MASS)


##latest attempt
##loading csv matrix -> converting to matrix -> converting
 
THSYmatrix= read.csv(file.choose(), sep=",", row.names=1)
THSYmatrix2<- as.matrix(THSYmatrix) 
THSYedge<- graph.adjacency(THSYmatrix2, weighted = T, mode = 'directed')
THSYlinks<-get.edgelist(THSYedge)
THSYlinks<-as.matrix(THSYlinks)


head(THSYlinks)

##loading csv of nodes file 

THSYnodes<-read.csv(file.choose(), header=T, as.is=T)

head(THSYnodes)

##check dimensions 
dim(THSYlinks)
dim(THSYnodes)

##graphing links/nodes

netTHSY <-graph.data.frame(THSYlinks,THSYnodes,directed=T)

netTHSY

##some plot options###
##plot1##
plot(netTHSY,vertex.label=V(netTHSY)
$species,vertex.label.font=2,
vertex.label.color="gray40",
vertex.label.cex=.7,edge.color="gray85")

##moving the plot manualy, then grabbing the coordinates
tkid <-tkplot(netTHSY)
l <-tkplot.getcoords(tkid)
##throw coords inline as 
plot(netTHSY,layout=l) 

##plot2##
##setting a color scheme by distance from actor 1 (raptor)
dist.from.raptor<-shortest.paths(netTHSY, algorithm="unweighted")[1,]
oranges <-colorRampPalette(c("red", "gold"))
col<-oranges(max(dist.from.raptor)+1)[dist.from.raptor+1]

plot(netTHSY, vertex.color=col, vertex.label=V(netTHSY)
$species, edge.arrow.size=.6,
vertex.label.color="black",
mark.groups=c(2,3,4),mark.col="#C5E5E7",mark.border=NA)

##plot3##orange,limited node size
plot(netTHSY, edge.arrow.size=.2, edge.color="grey",
vertex.color="orange", vertex.frame.color="#ffffff",
vertex.label=V(netTHSY)$species, vertex.label.color="black")

##plot4
##sets node sizes according to degree
V(netTHSY)$size <- deg*2

# Generate colors base on guild:
colrs <- c("tomato3","cyan4", "cyan2", "darkgoldenrod","aquamarine","cyan")
V(netTHSY)$colors<-colrs[V(netTHSY)$guild]

##re-scaleing the edges to be farther/nearer by editing layout=1*## <-##
##layoutediting##

l <- layout.star(netTHSY)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
hubbyscore<- hub_score(netTHSY,weights=NA)$vector ##use after vertex.size for hub value
authorityfear<- authority_score(netTHSY, weights=NA)$vector ##use after vertex.size for authority value
img.hawk <- readPNG(file.choose())##adding image of hawk, didnt work well
V(netTHSY)$raster <- list(img.hawk)

plot(netTHSY, rescale=F, layout=l*1.2,edge.curved=NULL,
edge.arrow.size=0, edge.color="grey",
edge.width=E(netTHSY)$weight/10,
vertex.size=authorityfear*30,
vertex.color=colrs[V(netTHSY)$guild], vertex.frame.color="#ffffff",
vertex.label=V(netTHSY)$species, vertex.label.color="black",
main="Bluish-Slate Antshrike Fear Network")

legend(x=-1.6, y=-.7,
c("Raptor","Sentinal", "Associate","Terrestrial","Canopy","Facultative"), pch=21,
col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

##include this for raptor image
rasterImage(img.hawk, xleft=-.08, xright=.08, ybottom=.1, ytop=.27)  ##raptor on top

rasterImage(img.hawk, xleft=-.3, xright=-.15, ybottom=-.1, ytop=.1) ##raptor to the side

rasterImage(img.hawk, xleft=-.28, xright=.35, ybottom=-.3, ytop=.3) ##raptor huge middle

####same plot ^^^ with groups highlighted (why tho)###
plot(netTHSY, rescale=F, layout=l*1.0,
vertex.color=colrs[V(netTHSY)$guild], vertex.frame.color="#ffffff",
vertex.label=V(netTHSY)$species, vertex.label.color="black",
mark.groups=list(c(2,3,4,5,6),c(9,11,12,13)), mark.col=c("#C5E5E7","#ECD89A"),
 mark.border=NA)



plot(netTHSY)

# Change Edge Attributes
E(netTHSY)$width <- E(netTHSY)$weight/10 ##this one works well
E(netTHSY)$width <- edge.betweenness(netTHSY)
E(netTHSY)$arrow.size <- 2.0
E(netTHSY)$edge.color <- "blue"
E(netTHSY)$width <- 1+E(netTHSY)$weight/15
plot(netTHSY)



##trying to make a comunity
V(netTHSY)$guild<- optimal.community(netTHSY)$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(netTHSY, vertex.color=colrs[V(netTHSY)$guild])


#######################################old code########################

#generate node degree and that sets size
deg<- degree(netTHSY, mode="all")


dat=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE)
# read .csv file
fear=as.matrix(dat)

fearadjacency=graph.adjacency(fear,mode="undirected",weighted=TRUE,diag=FALSE)

summary(fearadjacency)

plot.igraph(fearadjacency,vertex.label=V(fearadjacency)$name,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(fearadjacency)$weight)

plot(fearadjacency, vertex.color="red",vertex.size=6, edge.arrow.size=.4, edge.curved=.1)

plot(fearadjacency,edge.arrow.size=.5, edge.curved=0,
vertex.color="orange", vertex.frame.color="#555555",
vertex.label=V(fearadjacency)$species, vertex.label.color="black",
vertex.label.cex=.9) 

head(fearadjacency)



plot(fearadjacency, vertex.shape="none", vertex.label=fearadjacency$Raptor,
vertex.label.color=V(fearadjacency)$color, vertex.label.font=2.5, 
vertex.label.cex=.6, edge.color="gray70",  edge.width=2)

##makes two graphs side by side
par(mfrow=c(1,2))
rm(list = ls())

hubbyscore<- hub_score(fearadjacency,weights=NA)$vector
plot(fearadjacency, vertex.size=hubbyscore*50, main="Hubs")

authorityfear<- authority_score(fearadjacency, weights=NA)$vector
plot(fearadjacency, vertex.size=hubbyscore*30, main="Authorities")










