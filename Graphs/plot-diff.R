
Rreverse <- function(x) {
	tmp <- c(1:length(x))
	for(i in 1:length(x)) {
		tmp[i] <- x[length(x)-i+1]
	}
	tmp
}


par(mfrow=c(2,3))

ncolors <- 20

mycolors <- heat.colors(ncolors)
mycolors <- Rreverse(mycolors)
mycolors <- c("yellow","orange","green3","blue","black")
normal <- 11212
levelnames <- c("<= 0.25", "0.25 - 0.50", "0.50 - 0.75", "0.75 - 0.95", ">= 0.95")
#mycolors <- gray((ncolors-1):0 / ncolors)

tnames <- c("CAR4081","consGenv", "consAenv", "consBenv")

mymax <- 0

tsize <- 1.25
tlab <- 1.25



plotsegs <- function(edges, lab1, lab2) {
  xa0 <- edges$V1
  ya0 <- edges$V2
  xa1 <- edges$V3
  ya1 <- edges$V4
  weight <- edges$V5
  score1 <- edges$V6
  score2 <- edges$V7

  poscolors <- hsv(0.3,1:ncolors/ncolors,1);
  negcolors <- hsv(1.0,1:ncolors/ncolors,1);
#  colors <- c(heat.colors(ncolors),Rreverse(topo.colors(ncolors)))
#  colors <- terrain.colors(ncolors)
#  colors <- cm.colors(ncolors)

  weight <- weight / normal
  score1 <- score1 / normal
  score2 <- score2 / normal

  boundaries <- seq(0,ncolors)/ncolors;
  boundaries2 <- boundaries
  m <- max(max(weight),-min(weight))
  boundaries <- m*boundaries
  
  tmp <- data.frame(xa0,ya0,xa1,ya1,weight,score1,score2)

  if( mymax == 0 ) {
    tempm1 <- tmp[tmp$weight > boundaries[1] | tmp$weight < -boundaries[1], ]

    xmin <- min(tempm1$xa0)
    xmax <- max(tempm1$xa0)
    ymin <- min(tempm1$ya0)
    ymax <- max(tempm1$ya0)
    xmax <- xmin + (xmax-xmin)*1.3
  } else {
    xmin <- 0
    xmax <- mymax*1.3
    ymin <- 0
    ymax <- mymax
  }

  plot(xa0,ya0,type="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
       ylab=lab1,xlab=lab2,font.lab=3,cex.lab=tlab,cex.axis=tsize)
    

  
  for(i in 2:ncolors) {
    tmp4 <- tmp[tmp$score1 >= boundaries2[i] & tmp$score1 <= boundaries2[i+1],]
    
    if (length(tmp4$xa0)>0 ) {
      print(i)
      print (1.0-0.*i/ncolors)
      color <- grey(1.0-0.25*i/ncolors)
      segments(tmp4$xa0, tmp4$ya0, tmp4$xa1, tmp4$ya1, col=color, lwd=1)
    }
  }

  for(i in 2:ncolors) {
    # positive
    tmp2 <- tmp[tmp$weight >= boundaries[i] & tmp$weight <= boundaries[i+1],]
    # negative
    tmp3 <- tmp[tmp$weight <= -boundaries[i] & tmp$weight <= -boundaries[i+1],]

    #    print(poscolors[i]);
    if (length(tmp2$xa0)>0 ) {
      segments(tmp2$xa0, tmp2$ya0, tmp2$xa1, tmp2$ya1, col=poscolors[i], lwd=1)
    }
    
    #    print(negcolors[i]);
    if (length(tmp3$xa0)>0 ) {
      segments(tmp3$xa0, tmp3$ya0, tmp3$xa1, tmp3$ya1, col=negcolors[i], lwd=1)
    }
  }

  labels <- c(Rreverse(negcolors),poscolors)
  labels <- c()
  colors <- c(Rreverse(negcolors),poscolors)
  colors <- c()

  for(i in seq((ncolors+1),2,-4)) {
    min <- -boundaries[i]
    max <- -boundaries[i-1]
    labels <- c(labels,paste(min))
    colors <- c(colors,negcolors[i-1])
  }
  
  for(i in seq(2,(ncolors+1),4)) {
    min <- boundaries[i-1]
    max <- boundaries[i]
    labels <- c(labels,paste(max))
    colors <- c(colors,poscolors[i-1])
  }

  legend(xmin+(xmax-xmin)*0.65,ymin+(ymax-ymin)*0.6,
         legend=labels,
         lwd=seq(from=2,to=2,length=(ncolors*2)),
         col=colors,
         cex=tsize,bty="n",
         y.intersp=0.8)
}

d <- read.table("m01")
plotsegs(d,tnames[0+1],tnames[1+1])

d <- read.table("m02")
plotsegs(d,tnames[0+1],tnames[2+1])

d <- read.table("m03")
plotsegs(d,tnames[0+1],tnames[3+1])

d <- read.table("m12")
plotsegs(d,tnames[1+1],tnames[2+1])

d <- read.table("m13")
plotsegs(d,tnames[1+1],tnames[3+1])

d <- read.table("m23")
plotsegs(d,tnames[2+1],tnames[3+1])

# dev.print(device=postscript,file="test.ps",height=8,width=16,horizontal=T)
