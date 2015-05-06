### Eco-regions into the future
library(vegan)
library(ecodist)
source('global.R')

### read in data from csv with rows=sites
x <- read.csv('../data/pop_points.csv')

### get elevation values from the web based on lat and lon
elev <- as.numeric()
for (i in 1:nrow(x)){elev[i] <- getElev(x[i,4],x[i,3])}
x <- data.frame(x,Elevation=elev)
cex.elev <- elev/max(elev) + 1


### generate figure showing
### 1 = plot of sites in geographic space with climate vectors overlayed
ord <- apply(x[,3:4],2,function(x) (x-mean(x))/sd(x))
v.fit <- envfit(ord,x[,5])

plot(ord,col=as.numeric(x[,1]),cex=cex.elev^2)
plot(v.fit,label='Climate',col='darkgrey')

### Example of a "cross-hair" plot (i.e. centroid with standard deviation bars)
ch.plot(ord,factor(x[,1]),plot.legend=TRUE)
plot(v.fit,label='Climate',col='darkgrey')

#######################################################
### 2 - Vectors of climate change into the future!
#######################################################

####################
### make up vectors
### You won't need to do this since you'll use data from the climate models
### You will need to break up the data into a list of matrices, one matrix for each set of 
### climate variables for each year.
### Skip down to the next comment.
####################
x.now <- x[,3:4]
x.2050 <- apply(x[,3:4],2,function(x,y) unlist(tapply(x,y,function(z) z + mean(z)*0.1)),y=x[,1])
x.2050[,1] <- unlist(tapply(x.2050[,1],x[,1],function(z) z + mean(z)*0.05))
x.2080 <- apply(x[,3:4],2,function(x,y) unlist(tapply(x,y,function(z) z + mean(z)*0.2)),y=x[,1])
clim.v <- list(current=x.now,'2050'=x.2050,'2080'=x.2080)
clim.v <- do.call(rbind,clim.v)
clim.v <- apply(clim.v,2,function(x) (x-mean(x))/sd(x))
clim.v[,1] <- clim.v[,1] * -1
clim.v <- split(clim.v,substr(rownames(clim.v),1,4))
clim.v <- lapply(clim.v,matrix,ncol=2)
clim.v <- clim.v[c(3,1,2)]
time <- c(rep(names(clim.v)[1],nrow(clim.v[[1]])),rep(names(clim.v)[2],nrow(clim.v[[2]])),rep(names(clim.v)[3],nrow(clim.v[[3]])))
clim.mat <- data.frame(time=time,do.call(rbind,clim.v))

########################
### Next Comment is here!
### One easy way to format the data, would be to have 
### each column be a different climate variable and 
### each row would be a different site, year combo.
### So, you would have two additional columns (site and year)
### Here is an example using the simulated data above:
####################################
clim.v <- split(clim.mat[,-1],clim.mat$time)


######################################################
### Here's the plotting script, sorry, it's obtuse.
### Just make sure your data is formatted like clim.v
### and run it.
######################################################
par(mfrow=c(1,2))
plot(ord,col=as.numeric(x[,1]),cex=cex.elev^2)
plot(v.fit,label='Climate',col='darkgrey')
plot(1,1,pch='',xlim=range(do.call(rbind,clim.v)[,1]),ylim=range(do.call(rbind,clim.v)[,2]),xlab='X1',ylab='X2')
lines(c(clim.v[[1]][1,1],clim.v[[2]][1,1]),c(clim.v[[1]][1,2],clim.v[[2]][1,2]),col='lightgrey')
lines(c(clim.v[[2]][1,1],clim.v[[3]][1,1]),c(clim.v[[2]][1,2],clim.v[[3]][1,2]),col='lightgrey')

for (i in 2:nrow(clim.v[[1]])){
    lines(c(clim.v[[1]][i,1],clim.v[[2]][i,1]),c(clim.v[[1]][i,2],clim.v[[2]][i,2]),col='lightgrey')
    lines(c(clim.v[[2]][i,1],clim.v[[3]][i,1]),c(clim.v[[2]][i,2],clim.v[[3]][i,2]),col='lightgrey')

}
points(clim.v[[1]][,1],clim.v[[1]][,2],pch=1,col=as.numeric(x[,1]))
points(clim.v[[3]][,1],clim.v[[3]][,2],pch=19,col=as.numeric(x[,1]))

