
Rreverse <- function(x) {
	tmp <- c(1:length(x))
	for(i in 1:length(x)) {
		tmp[i] <- x[length(x)-i+1]
	}
	tmp
}


par(mfrow=c(2,3))

ncolors <- 10

mycolors <- heat.colors(ncolors)
mycolors <- Rreverse(mycolors)
mycolors <- c("yellow","orange","green3","blue","black")
normal <- 5606
levelnames <- c("<= 0.25", "0.25 - 0.50", "0.50 - 0.75", "0.75 - 0.95", ">= 0.95")
#mycolors <- gray((ncolors-1):0 / ncolors)

tnames <- c("CAR4081","consGenv", "consAenv", "consBenv")

mymax <- 0

tsize <- 1.25
tlab <- 1.25



plotsegs <- function(xa0, ya0, xa1, ya1, weight, lab1, lab2) {
  boundaries <- seq(0,ncolors)/ncolors;
  poscolors <- hsv(0.3,1:ncolors/ncolors,1);
  negcolors <- hsv(1.0,1:ncolors/ncolors,1);
#  colors <- c(heat.colors(ncolors),Rreverse(topo.colors(ncolors)))
#  colors <- terrain.colors(ncolors)
#  colors <- cm.colors(ncolors)
  
  weight <- weight / normal
  m <- max(max(weight),-min(weight))
  boundaries <- m*boundaries

  if( mymax == 0 ) {
    plot(xa0,ya0,type="n",ylab=lab1,xlab=lab2,font.lab=3,cex.lab=tlab,cex.axix=tsize)
  } else {
    plot(xa0,ya0,type="n",xlim=c(0,mymax*1.3),ylim=c(0,mymax),
         ylab=lab1,xlab=lab2,font.lab=3,cex.lab=tlab,cex.axis=tsize)
  }
  tmp <- data.frame(xa0,ya0,xa1,ya1,weight)
  for(i in 1:ncolors) {
    # positive
    tmp2 <- tmp[tmp$weight >= boundaries[i] & tmp$weight <= boundaries[i+1],]
    # negative
    tmp3 <- tmp[tmp$weight <= -boundaries[i] & tmp$weight <= -boundaries[i+1],]

    #    print(poscolors[i]);
    if (length(tmp2$xa0)>0 ) {
      segments(tmp2$xa0, tmp2$ya0, tmp2$xa1, tmp2$ya1, col=poscolors[i], lwd=1)
    }
    
    print(negcolors[i]);
    if (length(tmp3$xa0)>0 ) {
      segments(tmp3$xa0, tmp3$ya0, tmp3$xa1, tmp3$ya1, col=negcolors[i], lwd=1)
    }
  }

  legend(120,80,legend=c(Rreverse(negcolors),poscolors),
         lwd=seq(from=2,to=2,length=(ncolors*2)),
         col=c(Rreverse(negcolors),poscolors),
         cex=tsize,bty="n",
         y.intersp=0.5)
}

d <- read.table("m01")
plotsegs(d$V1,d$V2,d$V3,d$V4,d$V5,tnames[0+1],tnames[1+1])

d <- read.table("m02")
plotsegs(d$V1,d$V2,d$V3,d$V4,d$V5,tnames[0+1],tnames[2+1])

d <- read.table("m03")
plotsegs(d$V1,d$V2,d$V3,d$V4,d$V5,tnames[0+1],tnames[3+1])

d <- read.table("m12")
plotsegs(d$V1,d$V2,d$V3,d$V4,d$V5,tnames[1+1],tnames[2+1])

d <- read.table("m13")
plotsegs(d$V1,d$V2,d$V3,d$V4,d$V5,tnames[1+1],tnames[3+1])

d <- read.table("m23")
plotsegs(d$V1,d$V2,d$V3,d$V4,d$V5,tnames[2+1],tnames[3+1])

dev.print(device=postscript,file="test.ps",height=8,width=16,horizontal=T)
