### Plots of ecoregion patterns for dana 
source('global.R')
library(jpeg)
library(ggmap)

stay <- read.csv('../data/AllLocations_EnvStay.csv')
stay <- stay[,-8]
stay$Time <- as.character(stay$Time)
stay$Time[as.character(stay$Time) == "Current"] <- 'Current'
                                        #relativize climate variables
stay[,5:7] <- apply(stay[,5:7],2,function(x) x/max(x))
                                        #ordinate
### ord.stay <- nmds(dist(stay[,5:7]),2,2)
### min.stay <- nmds.min(ord.stay)
### min.stay <- read.csv('../data/ord_stay.csv')
min.stay <- princomp(stay[,5:7])$scores
vec.stay <- envfit(min.stay,stay[,-1:-4])
ord.col <- c('red','black','green')[as.numeric(stay$Ecoregion)]
ord.time <- as.numeric(factor(stay$Time))
ord.time <- c(1,0.45,0.5)[ord.time]
ord.alpha <- apply(cbind(ord.col,ord.time),1,function(x) alpha(x[1],x[2]))
ord.pch <- as.numeric(factor(stay$Time))
ord.pch <- c(19,19,1)[ord.pch]
f.stay <- paste(stay[,1],stay[,4])
leg.names <- unique(paste(stay[,1],stay[,4]))
leg.names <- leg.names[c(3,1,2,6,4,5,9,7,8)]
leg.names <- gsub('2010','1990',leg.names)
leg.names <- gsub('2050','2040',leg.names)
leg.names <- gsub('2080','2070',leg.names)
leg.col <- unique(ord.alpha)[c(3,1,2,6,4,5,9,7,8)]

x <- move.all <- read.csv('../data/AllLocations_EnvMove.csv')
move.all[,2:3] <- apply(move.all[,2:3],2,function(x) (x-mean(x))/sd(x))
#move.all$Time <- gsub('Current','2010',move.all$Time)
vec.move <- envfit(move.all[,2:3],move.all[,5:7])
move.col <- c('red','black','green')[as.numeric(move.all$Ecoregion)]
move.time <- as.numeric(factor(move.all$Time))
move.time <- c(1,0.45,0.5)[move.time]
move.alpha <- apply(cbind(move.col,move.time),1,function(x) alpha(x[1],x[2]))
move.pch <- as.numeric(factor(move.all$Time))
move.pch <- c(19,19,1)[move.pch]
move.year <- 
f <- paste(move.all[,1],move.all[,4])
x.f <- paste(x[,1],x[,4])
mu <- data.frame(apply(x[,2:3],2,function(x,f) tapply(x,f,mean),f=x.f))
sd <- apply(x[,2:3],2,function(x,f) tapply(x,f,sd),f=x.f)

map. <- get_map(c(lon=-97,lat=mean(x[,3])+(-3)),
               zoom=4,source='stamen',maptype='toner-background')
map <- map.
attr_map <- attr(map,'bb')
map[map == '#000000'] <- grey(1)
class(map) <- c('ggmap','raster')
attr(map,'bb') <- attr_map

jpeg('../results/map_inset.jpeg',width=2400,height=2400,,quality=100)
chPlot(move.all[,2:3],f=f,col=move.alpha,pch=move.pch,xlim=c(-1.5,2),ylim=c(-2,1))
plot(vec.move,col=grey(0.75))
dev.off()

jpeg('../results/map.jpeg',width=2400,height=2400,quality=100)
ggmap(map)+
    geom_point(aes(x=Longitude,y=Latitude),data=x[grepl('CCV',x.f),2:3],col='red',alpha=0.25,size=3)+
    geom_point(aes(x=Longitude,y=Latitude),data=x[grepl('SD',x.f),2:3],col='black',alpha=0.35,size=3)+
    geom_point(aes(x=Longitude,y=Latitude),data=x[grepl('UHP',x.f),2:3],col='green',alpha=0.25,size=3)+
    labs(x='',y='')+
    theme(line = element_blank(),
          text = element_blank(),
          line = element_blank(),
          text = element_blank())
dev.off()

main <- readJPEG('../results/map.jpeg')
inset <- readJPEG('../results/map_inset.jpeg')
mu <- list(mu=apply(move.all[,2:3],2,function(x,f) tapply(x,f,mean),f=f))
xlim <- c(-1.35,2.5);ylim <- c(-2.35,2)

### Fig 1. Ordination of ecoregions moving through climate space
pdf('../results/EcoReg_FigA.pdf')
sc <- chPlot(min.stay,f=f.stay,col=ord.alpha,pch=ord.pch,xlim=c(-1,1.25),ylim=c(-1,0.5),return.coord=TRUE,se=FALSE)
plot(vec.stay,col=grey(0.75))
text(c(-0.25,-0.25,0.65),c(-0.30,0.25,0.30),labels=c('CCV','SD','UHP'),col=c(2,1,3))
chArrow(sc)
text(sc[[1]][grepl('UHP',rownames(sc[[1]])),1:2],labels=substr(rownames(sc[[1]][grepl('UHP',rownames(sc[[1]])),]),5,8),pos=3,col='darkgrey')
#legend('bottomright',legend=leg.names,pch=rep(c(19,19,1),3),col=leg.col)
dev.off()

### Fig 2. Plot of points moving through geographic space based on climate
pdf('../results/EcoReg_FigB.pdf')
sc.m <- chPlot(move.all[,2:3],f=f,col=move.alpha,pch=move.pch,xlim=xlim,ylim=ylim,cex=0.75,plot.axes=FALSE,xlab='Longitude (decimal)',ylab='Latitude (decimal)',return.coord=TRUE,se=FALSE)
plot(vec.move,col=grey(0.75))
axis(side=1,at=seq(xlim[1],xlim[2],length=5),
     labels=round(seq(mean(x[,2])-(xlim[1]*sd(x[,2])),mean(x[,2])+(xlim[2]*sd(x[,2])),
         length=5),1))
axis(side=2,at=seq(ylim[1],ylim[2],length=5),
     labels=round(seq(mean(x[,3])-(ylim[1]*sd(x[,3])),mean(x[,3])+(ylim[2]*sd(x[,3])),
         length=5),1))
text(c(-0.8997,1.35722411,-0.06612945),c(0.5084873,-0.7592822,1.0456123),
     labels=c('CCV','SD','UHP'),col=c(2,1,3))
chArrow(mu)
text(sc.m[[1]][grepl('UHP',rownames(sc.m[[1]])),1:2],labels=substr(rownames(sc.m[[1]][grepl('UHP',rownames(sc.m[[1]])),]),5,8),pos=3,col='darkgrey')
rasterImage(main,0.75,0.15,2.5,2)
dev.off()

gitPush('../results')
