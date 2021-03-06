library(RCurl)
library(XML)
library(vegan)


gitPush <- function(x='.',message='update'){
    system(paste('git add -A ',x,sep=''))
    system(paste('git commit -am ',message,sep=''))
    system('git push')
}

alpha <- function(col, alpha=1){
    if(missing(col))
        stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2,
          function(x)
              rgb(x[1], x[2], x[3], alpha=alpha))
}

getElev <- function(latitude=52.4822,longitude=-1.8946){
    url <- paste(
        "http://www.earthtools.org/height",
        latitude,
        longitude,
        sep = "/"
        )
    page <- getURL(url)
    ans <- xmlTreeParse(page, useInternalNodes = TRUE)
    heightNode <- xpathApply(ans, "//meters")[[1]]
    as.numeric(xmlValue(heightNode))
}

chPlot <- function(x,f,col,pch,se=FALSE,xlim=c(-1,1),ylim=c(-1,1),cex=1,plot.axes=TRUE,return.coord=FALSE,xlab='Axis 1',ylab='Axis 2'){
    col <- tapply(col,f,function(x) x[1])
    pch <- tapply(pch,f,function(x) x[1])
    mu <- apply(x,2,function(x,f) tapply(x,f,mean),f=f)
    if (se){
        bars <- apply(x,2,function(x,f) 
            tapply(x,f,function(x) sd(x)/sqrt(length(x))),f=f)
    }else{
        bars <- apply(x,2,function(x,f) 
            tapply(x,f,sd),f=f)
    }
    bar.up <- mu + bars
    bar.lo <- mu - bars
### make the plot
    if (plot.axes){
        plot(mu,col=col,pch=pch,xlim=xlim,ylim=ylim,cex=cex,xlab=xlab,ylab=ylab)
    }else{
        plot(mu,col=col,pch=pch,xlim=xlim,ylim=ylim,cex=cex,xaxt='n',yaxt='n',xlab=xlab,ylab=ylab)
    }

    for (i in 1:nrow(mu)){
        lines(c(bar.up[i,1],bar.lo[i,1]),rep(mu[i,2],2),col=col[i])
        lines(rep(mu[i,1],2),c(bar.up[i,2],bar.lo[i,2]),col=col[i])
    }
    if (return.coord){return(list(mu,bars))}
}

chArrow <- function(x){
    n <- do.call(rbind,strsplit(rownames(x[[1]]),split=' '))
    for (i in 1:length(unique(n[,1]))){
        y <- x[[1]][n[,1] == unique(n[,1])[i],]
        n. <- as.numeric(do.call(rbind,strsplit(rownames(y),split=' '))[,2])
        y <- y[order(n.),]
        for (j in 1:(nrow(y) - 1)){
            arrows(y[j,1],y[j,2],y[(j+1),1],y[(j+1),2],
                   code=2,angle=10,length=0.1,lwd=1.25,col='darkgrey')
        }
    }
}

ch.point <- function(x,f){
    list(mu=tapply(x,f,mean),se=tapply(x,f,function(x) sd(x)/sqrt(length(x))))
}

ch.vector <- function(x,t,pch,col){
    mu <- apply(x,2,function(x,t) tapply(x,t,mean),t=t)
    se <- apply(x,2,function(x,t) tapply(x,t,function(x) sd(x)/sqrt(length(x))),t=t)
    cbind(mu,se)
}

ch.plot <-
function(x='ordination matrix',g='groupings',cex=1,plot.legend=FALSE,loc='topleft',mu.pch=19){
  mu <- apply(x,2,function(x,g) tapply(x,g,mean),g=g)
  se <- apply(x,2,function(x,g) tapply(x,g,function(x) sd(x)/sqrt(length(x))),g=g)
  mu <- na.omit(mu)
  se <- na.omit(se)
                                        #error bars
  cl.xu <- mu[,1] + se[,1]
  cl.xl <- mu[,1] - se[,1]
  cl.yu <- mu[,2] + se[,2]
  cl.yl <- mu[,2] - se[,2]
    if (plot.legend){
                                        #coloring
      mu.col <- rainbow(length(unique(g)))[as.numeric(unique(g))]
      plot(mu,pch=mu.pch,cex=cex,xlim=c(min(cl.xl),max(cl.xu)),ylim=c(min(cl.yl),max(cl.yu)),col=mu.col)
      for (i in 1:nrow(mu)){
        lines(x=c(cl.xl[i],cl.xu[i]),y=c(mu[i,2],mu[i,2]))
        lines(x=c(mu[i,1],mu[i,1]),y=c(cl.yl[i],cl.yu[i]))
      }    
      legend(loc,legend=rownames(se),cex=cex*0.5,pch=mu.pch,col=mu.col,border='grey')
  }else{
                                        #coloring
    mu.col <- 'black'
    plot(mu,pch=mu.pch,cex=cex,xlim=c(min(cl.xl),max(cl.xu)),ylim=c(min(cl.yl),max(cl.yu)),col=mu.col)
    for (i in 1:nrow(mu)){
      lines(x=c(cl.xl[i],cl.xu[i]),y=c(mu[i,2],mu[i,2]))
      lines(x=c(mu[i,1],mu[i,1]),y=c(cl.yl[i],cl.yu[i]))
    }
  }
}
