Rreverse <- function(x) {
	tmp <- c(1:length(x))
	for(i in 1:length(x)) {
		tmp[i] <- x[length(x)-i+1]
	}
	tmp
}


par(mfrow=c(2,3))
ncolors <- 5
mycolors <- heat.colors(ncolors)
# mycolors <- topo.colors(ncolors)
#mycolors <- terrain.colors(ncolors)
#mycolors <- cm.colors(ncolors)
mycolors <- Rreverse(mycolors)
mycolors <- c("yellow","orange","green3","blue","black")
normal <- 966
levelnames <- c("<= 0.25", "0.25 - 0.50", "0.50 - 0.75", "0.75 - 0.95", ">= 0.95")
#mycolors <- gray((ncolors-1):0 / ncolors)
# minmax <- c( c(0.001,0.25), c(0.25,0.50), c(0.50,0.75), c(0.75,0.95), c(0.95,1) )
mins <- c(0.05, 0.25, 0.50, 0.75, 0.95)
maxs <- c(0.25, 0.50, 0.75, 0.95, 1.00)
tnames <- c("H. Sapiens","Sulfaci1", "Halo Mari", "E. Coli")

mymax <- 120

tsize <- 1.25
tlab <- 1.25


plotsegs <- function(xa0, ya0, xa1, ya1, weight, lab1, lab2) {
	weight <- weight / normal
	if( mymax == 0 ) {
		plot(xa0,ya0,type="n",ylab=lab1,xlab=lab2,font.lab=3,cex.lab=tlab,cex.axix=tsize)
	} else {
		plot(xa0,ya0,type="n",xlim=c(0,mymax*1.3),ylim=c(0,mymax),
			ylab=lab1,xlab=lab2,font.lab=3,cex.lab=tlab,cex.axis=tsize)
	}
	tmp <- data.frame(xa0,ya0,xa1,ya1,weight)
	for(i in 1:ncolors) {
		#as.integer(tmp$weight/50 + 1) == i,]
		tmp2 <- tmp[tmp$weight >= mins[i] & tmp$weight <= maxs[i],]
                if (length(tmp2$xa0)>0 ) {
                  segments(tmp2$xa0,tmp2$ya0,tmp2$xa1,tmp2$ya1,
                           col=mycolors[i],lwd=1)
                }
	}
	legend(120,80,legend=levelnames,
		lwd=seq(from=2,to=2,length=ncolors),
		col=mycolors,
		cex=tsize,bty="n",
		y.intersp=1)
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
