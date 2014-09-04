args = commandArgs(trailingOnly=T)
infile = args[1]
outfile = args[2]

svg(file=outfile,height=8,width=8)

M = as.matrix(read.table(infile))
L = dim(M)[1]
library(stats)
points = cmdscale(M)

#library(MASS)
#mds = isoMDS(M)
#points = mds$points

minx = min(points[,1])
maxx = max(points[,1])
miny = min(points[,2])
maxy = max(points[,2])

xw = (maxx-minx)
yw = (maxy-miny)

plot(points,type="n",ylab="",xlab="",axes=F)

colorseq1 = hsv(seq(from=1,to=0.33,length=L),1,1)
colorseq2 = hsv(seq(from=1,to=0.33,length=L),0.2,1)

lines(points,pch=20,col=hsv(0,0,0.95))
points(points,pch=20,col=colorseq1)

# dev.copy2eps(file="NNI-SPR.eps")
text(points[1,1],points[1,2]-yw*0.001,pos=1,"S")
text(points[L,1],points[L,2]-yw*0.001,pos=1,"E")
