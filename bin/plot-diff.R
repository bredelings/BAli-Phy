# run as Rexec plot-diff.R test

argv <- function(x) {
  args = commandArgs()
  offset = 0
  for(i in 1:length(args)) {
    if (args[i] == "--args")
      offset=i;
  }
  args[offset+x]
}

Rreverse <- function(x) {
  tmp <- c(1:length(x))
  for(i in 1:length(x)) {
    tmp[i] <- x[length(x)-i+1]
  }
  tmp
}

getcolor <- function(x) {
  if (x>=0) {
    x = x*x
    start = 1.0
    end = 0
    
    h = start + x * (end-start)
  }
  else {
    x = -x;
    x = x*x
    start = 0.3
    end = 1
    
    h = start + x * (end-start)
  }
  hsv(h,x,1)
}


par(mfrow=c(2,3))

ncolors <- 60

mycolors <- heat.colors(ncolors)
mycolors <- Rreverse(mycolors)
mycolors <- c("yellow","orange","green3","blue","black")
# normal <- 11212
#mycolors <- gray((ncolors-1):0 / ncolors)

mymax <- 0

tsize <- 1.25
tlab <- 1.25


# if we are comparing
plotsegs <- function(normal, edges, lab1, lab2) {
  xa0 <- edges$V1
  ya0 <- edges$V2
  xa1 <- edges$V3
  ya1 <- edges$V4
  weight <- edges$V5
  score1 <- edges$V6
  score2 <- edges$V7

  poscolors <- hsv(0.3,1:ncolors/ncolors,1);
  negcolors <- hsv(1.0,1:ncolors/ncolors,1);

  if (length(score1) == 0 ) {
    score1 = weight   # first one is the same as first-second
    score2 = weight*0 # second is zero

    a = seq(1.0,0.5,length=ncolors);
    b = 1:ncolors/ncolors
    poscolors = lapply(1:ncolors/ncolors,getcolor)
    negcolors = c();
  }

#  colors <- c(heat.colors(ncolors),Rreverse(topo.colors(ncolors)))
#  colors <- terrain.colors(ncolors)
#  colors <- cm.colors(ncolors)

  weight <- weight / normal
  score1 <- score1 / normal
  score2 <- score2 / normal

# boundaries2 is the ABSOLUTE scale
  boundaries2 <- seq(0,ncolors)/ncolors
  boundaries <- boundaries2

  if (length(score1) != 0) {
# boundaries is the RELATIVE scale
    m <- max(max(weight),-min(weight))
    boundaries <- m*boundaries
  }
  
# data frame with scaled weight and scores
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
    

  
  # Draw (in grey) the lines that we all agree on
  for(i in 2:ncolors) {
    tmp4 <- tmp[tmp$score1 >= boundaries2[i] & tmp$score1 <= boundaries2[i+1],]
    
    if (length(tmp4$xa0)>0 ) {
#      print("i = ")
#      print(i)
#      print (1.0-0.25*i/ncolors)
      color <- grey(1.0-0.25*i/ncolors)
      segments(tmp4$xa0, tmp4$ya0, tmp4$xa1, tmp4$ya1, col=color, lwd=1)
    }
  }


  # Draw (in color) the lines that we disagree on
  for(i in 2:ncolors) {
    # positive
    tmp2 <- tmp[tmp$weight >= boundaries[i] & tmp$weight <= boundaries[i+1],]
    # negative
    tmp3 <- tmp[tmp$weight <= -boundaries[i] & tmp$weight <= -boundaries[i+1],]

    if (length(tmp2$xa0)>0 ) {
      segments(tmp2$xa0, tmp2$ya0, tmp2$xa1, tmp2$ya1, col=getcolor(i/ncolors), lwd=1)
    }
    
    #    print(negcolors[i]);
    if (length(tmp3$xa0)>0 ) {
      segments(tmp3$xa0, tmp3$ya0, tmp3$xa1, tmp3$ya1, col=getcolor(-i/ncolors), lwd=1)
    }
  }


  labels <- c()
  colors <- c()

  if (length(negcolors) > 0) {
    for(i in seq(-1,1,length=8)) {
      labels = c(labels,paste(i*m))
      colors = c(colors,getcolor(i))
    }
  }
  else {
    for(i in seq(0,1,length=11)) {
      labels = c(labels,paste(i))
      colors = c(colors,getcolor(i))
    }
  }

  legend(xmin+(xmax-xmin)*0.65,ymin+(ymax-ymin)*0.6,
         legend=labels,
         lwd=seq(from=2,to=2,length=(ncolors*2)),
         col=colors,
         cex=tsize,bty="n",
         y.intersp=0.8)
}

plotfile = function(input) {
  if (is.character(input)) {
    input <- file(input,"r")
    on.exit(close(input))
  }
  if (!inherits(input, "connection")) 
    stop("argument `file' must be a character string or connection")
  if (!isOpen(input)) {
    open(input, "r")
    on.exit(close(input))
  }

  total = scan(input,n=1,sep=" ")
  nlines = scan(input,n=1,sep=" ")
  name1 = scan(input,what="",n=1,sep=" ")
  name2 = scan(input,what="",n=1,sep=" ")
  d <- read.table(input,nrows=nlines)

  print("Plotting...")
  plotsegs(total,d,name1,name2);
}

ifile = file(argv(1),"r")
open(ifile,"r")

plotfile(ifile)
plotfile(ifile)
plotfile(ifile)
plotfile(ifile)
plotfile(ifile)
plotfile(ifile)

close(ifile)
# dev.print(device=postscript,file="test.ps",height=8,width=16,horizontal=T)
