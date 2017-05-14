args = commandArgs(trailingOnly=T)
N1 = as.numeric(args[1])
N2 = as.numeric(args[2])
infile = args[3]
outfile = args[4]

svg(file=outfile,height=8,width=8) 

M = as.matrix(read.table(infile))
L = dim(M)[1]
if (dim(M)[2] != L) { stop("Matrix not square") }
if (N1 + N2 != L) { stop("Wrong number of points.") }

# Actually do the MDS
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

# Don't waste space for axes and axis-labels that we aren't drawing.
par(mar=c(0,0,0,0))

# Do any empty plot with sets the drawing window to contain the points
plot(points,type="n",ylab="ylab",xlab="xlab",axes=F)

# A sequence of colors for the FIRST set of trees
colorseq1 = hsv(seq(from=0.83333, to=1,length=N1),
                seq(from=0.2,to=1,     length=N1),
                1)

# A sequence of colors for the SECOND set of trees
colorseq2 = hsv(seq(from=0.5,to=0.666666,length=N2),
                seq(from=0.2,to=1,     length=N2),
                1)

# Draw lines between the first set of points
lines(points[1:N1,],                pch=20,col=hsv(1,       0.20, 0.85,0.5))
# Draw lines between the second set of points
lines(points[(N1+1):(N1+N2),],      pch=20,col=hsv(0.66666, 0.20, 0.85,0.5))

points[,1] = points[,1] + rnorm(length(points[,1]),0,xw/500)
points[,2] = points[,2] + rnorm(length(points[,2]),0,yw/500)

# Draw the points
points(points,pch=20,col=c(colorseq1,colorseq2))

xwidth = (maxx-minx)*0.025
ywidth = (maxy-miny)*0.025
nbox = 12
i = (0:(nbox-1))*xwidth

# Draw the color legend for the first sequence
start.x = maxx-nbox*xwidth*1.1
start.y = miny+2.1*ywidth
height = ywidth
rect(start.x+i,start.y,start.x+xwidth+i,start.y+height, col=colorseq1[floor(seq(from=0,to=N1,length=nbox+1))])

# Draw the color legend for the second sequence
start.x = start.x
start.y = start.y - ywidth
height = ywidth
rect(start.x+i,start.y,start.x+xwidth+i,start.y+height, col=colorseq2[floor(seq(from=0,to=N2,length=nbox+1))])

# Label the beginning and ending of the FIRST sequence
text(points[1,1],points[1,2]-yw*0.001,pos=1,"S1")
text(points[N1,1],points[N1,2]-yw*0.001,pos=1,"E1")

# Label the beginning and ending of the SECOND sequence
text(points[N1+1,1],points[N1+1,2]-yw*0.001,pos=1,"S2")
text(points[N1+N2,1],points[N1+N2,2]-yw*0.001,pos=1,"E2")
