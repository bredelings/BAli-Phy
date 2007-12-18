#!/usr/bin/R CMD BATCH
pdf(file="pathgraph.pdf",height=8,width=8)

Rreverse = function(x) {
  tmp =c(1:length(x))
  for(i in 1:length(x)) {
    tmp[i] =x[length(x)-i+1]
  }
  tmp
}

argv = function(x) {
  args = commandArgs()
  offset = 0
  for(i in 1:length(args)) {
    if (args[i] == "--args")
      offset=i;
  }
  args[offset+x]
}

#par(mfrow=c(2,3))
ncolors =5
mycolors =heat.colors(ncolors)
# mycolors =topo.colors(ncolors)
#mycolors =terrain.colors(ncolors)
#mycolors =cm.colors(ncolors)
mycolors =Rreverse(mycolors)

levelnames = c("0.10", "0.25", "0.50", "0.75", "0.95")
levels     = c(0.10  ,  0.25 ,  0.50 ,  0.75 ,  0.95 )

mycolors =gray((ncolors-1):0 / ncolors)
# minmax =c( c(0.001,0.25), c(0.25,0.50), c(0.50,0.75), c(0.75,0.95), c(0.95,1) )
mins =c(0.05, 0.25, 0.50, 0.75, 0.95)
maxs =c(0.25, 0.50, 0.75, 0.95, 1.00)
tnames =c("H. Sapiens","Sulfaci1", "Halo Mari", "E. Coli")

mymax = 120

tsize =1.25
tlab =1.25
mymax=0

plotcolor = function(x)
{
  x = (x+x*x*x)/2
  x = 1-x
  gray(x*x)
}

plotsegs = function(normal,xa0, ya0, xa1, ya1, weight, lab1, lab2)
{
  xmax=max(xa1)
  ymax=max(ya1)
  weight = weight / normal

  plot(xa0,ya0,type="n",ylab=lab1,xlab=lab2,font.lab=3,cex.lab=tlab,cex.axis=tsize)

  tmp = data.frame(xa0,ya0,xa1,ya1,weight)
  
  tmp2 = tmp

  tmp2 = tmp[tmp$weight>0.01, ]

  o = sort.list(tmp2$weight);
  tmp3 = tmp2[o,]
  tmp2 = tmp3
  
  colors = plotcolor(tmp2$weight)
  segments(tmp2$xa0,tmp2$ya0,tmp2$xa1,tmp2$ya1,col=colors,lwd=1)

  mins =c(0.05, 0.25, 0.50, 0.75, 0.95)
  
  legend(xmax*0.82,ymax*0.25,legend=levelnames,
         lwd=2,
         col=plotcolor(levels),
         cex=tsize,bty="n",
         y.intersp=1)
}


filename = argv(1)
input = file(filename);
open(input,"r");
total = scan(input,n=1,sep=" ")
name1 = scan(input,what="",n=1,sep=" ")
name1 = scan(input,what="",n=1,sep=" ")
name2 = scan(input,what="",n=1,sep=" ")
close(input)

d = read.table(filename,skip=1)
plotsegs(total,d$V1,d$V2,d$V3,d$V4,d$V5,name1,name2);

# d =read.table("m02")
# plotsegs(d$V1,d$V2,d$V3,d$V4,d$V5,tnames[0+1],tnames[2+1])

# d =read.table("m03")
# plotsegs(d$V1,d$V2,d$V3,d$V4,d$V5,tnames[0+1],tnames[3+1])

# d =read.table("m12")
# plotsegs(d$V1,d$V2,d$V3,d$V4,d$V5,tnames[1+1],tnames[2+1])

# d =read.table("m13")
# plotsegs(d$V1,d$V2,d$V3,d$V4,d$V5,tnames[1+1],tnames[3+1])

# d =read.table("m23")
# plotsegs(d$V1,d$V2,d$V3,d$V4,d$V5,tnames[2+1],tnames[3+1])

#dev.print(device=postscript,file="test.ps",height=8,width=16,horizontal=T)
