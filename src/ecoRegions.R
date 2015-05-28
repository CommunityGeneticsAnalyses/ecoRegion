### Plots of ecoregion patterns for dana 
source('global.R')

### Fig 1. Ordination of ecoregions moving through climate space
stay <- read.csv('../data/AllLocations_EnvStay.csv')
stay <- stay[,-8]
stay$Time <- as.character(stay$Time)
stay$Time[as.character(stay$Time) == "Current"] <- '2010'

                                        #relativize climate variables
stay[,5:7] <- apply(stay[,5:7],2,function(x) x/max(x))
                                        #ordinate
### ord.stay <- nmds(dist(stay[,5:7]),2,2)
### min.stay <- nmds.min(ord.stay)
min.stay <- read.csv('../data/ord_stay.csv')
vec.stay <- envfit(min.stay,stay)
ord.col <- c('blue','green','red')[as.numeric(stay$Ecoregion)]
ord.time <- as.numeric(factor(stay$Time))
ord.time <- c(1,0.45,0.5)[ord.time]
ord.alpha <- apply(cbind(ord.col,ord.time),1,function(x) alpha(x[1],x[2]))
ord.pch <- as.numeric(factor(stay$Time))
ord.pch <- c(19,19,1)[ord.pch]
f.stay <- paste(stay[,1],stay[,4])

pdf('../results/EcoReg_FigA.pdf')
chPlot(min.stay,f=f.stay,col=ord.alpha,pch=ord.pch)
plot(vec.stay,col=grey(0.3))
dev.off()
gitPush('../results')

### Fig 2. Plot of points moving through geographic space based on climate
move.all <- read.csv('../data/AllLocations_EnvMove.csv')
move.all[,2:3] <- apply(move.all[,2:3],2,function(x) (x-mean(x))/sd(x))
vec.move <- envfit(move.all[,2:3],move.all[,5:7])
move.col <- c('blue','green','red')[as.numeric(move.all$Ecoregion)]
move.time <- as.numeric(factor(move.all$Time))
move.time <- c(1,0.45,0.5)[move.time]
move.alpha <- apply(cbind(move.col,move.time),1,function(x) alpha(x[1],x[2]))
move.pch <- as.numeric(factor(move.all$Time))
move.pch <- c(19,19,1)[move.pch]
f <- paste(move.all[,1],move.all[,4])

pdf('../results/EcoReg_FigB.pdf')
chPlot(move.all[,2:3],f=f,col=move.alpha,pch=move.pch,xlim=c(-1.75,2),ylim=c(-2,1))
plot(vec.move,col=grey(0.3))
dev.off()
gitPush('../results')
