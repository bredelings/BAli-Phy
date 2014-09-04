args = commandArgs(trailingOnly=T)
N1 = as.numeric(args[1])
N2 = as.numeric(args[2])
infile = args[3]
outfile = args[4]

svg(file=outfile,height=8,width=8) 

M = as.matrix(read.table(infile))
L = dim(M)[1]
library(stats)
points = cmdscale(M)

print(c(N1,N2))

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

colorseq1 = hsv(seq(from=0.83333, to=1,length=N1),
                seq(from=0.2,to=1,     length=N1),
                1)

colorseq2 = hsv(seq(from=0.5,to=0.666666,length=N2),
                seq(from=0.2,to=1,     length=N2),
                1)

lines(points[1:N1,],                pch=20,col=hsv(1,       0.05, 0.85))
lines(points[(N1+1):(N1+N2),],      pch=20,col=hsv(0.66666, 0.05, 0.85))

points(points,pch=20,col=c(colorseq1,colorseq2))

xwidth = (maxx-minx)*0.025
ywidth = (maxy-miny)*0.025
nbox = 12
i = (0:(nbox-1))*xwidth
start.x = maxx-nbox*xwidth*1.1
start.y = miny+2.1*ywidth
height = ywidth
rect(start.x+i,start.y,start.x+xwidth+i,start.y+height, col=colorseq1[floor(seq(from=0,to=N1,length=nbox+1))])

start.x = start.x
start.y = start.y - ywidth
height = ywidth
rect(start.x+i,start.y,start.x+xwidth+i,start.y+height, col=colorseq2[floor(seq(from=0,to=N2,length=nbox+1))])

# dev.copy2eps(file="NNI-SPR.eps")
text(points[1,1],points[1,2]-yw*0.001,pos=1,"S1")
text(points[N1,1],points[N1,2]-yw*0.001,pos=1,"E1")

text(points[N1+1,1],points[N1+1,2]-yw*0.001,pos=1,"S2")
text(points[N1+N2,1],points[N1+N2,2]-yw*0.001,pos=1,"E2")
