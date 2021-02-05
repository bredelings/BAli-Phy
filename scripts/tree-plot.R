# Read arguments from command line
args = commandArgs(trailingOnly=T)
infile = args[1]
outfile = args[2]
P = length(args) - 2
args = tail(args,P)

total = 0
Start = c()
End = c()
N = c()
for(i in 1:P)
{
    N[i] = as.numeric(args[i])

    Start[i] = total+1
    total    = total + N[i]
    End[i]   = total
}

# Read distance matrix
M = as.matrix(read.table(infile))
L = dim(M)[1]
if (dim(M)[2] != L) { stop("Matrix not square") }
if (total != L) { stop("Wrong number of points.") }

# Perform MDS projection
library(stats)
points = cmdscale(M)

# Get range of points
minx = min(points[,1])
maxx = max(points[,1])
miny = min(points[,2])
maxy = max(points[,2])

xw = (maxx-minx)
yw = (maxy-miny)

# create SVG file
svg(file=outfile,height=8,width=8) 

# plot points invisibly... to scale viewport?
plot(points,type="n",ylab="",xlab="",axes=F)

color = c(12,8,4,10,6,2)
colorseq = c()

# draw lines and compute color sequence
for (i in 1:P)
{
    c = hsv(seq(from=(color[i]-1)/12,to=color[i]/12,     length=N[i]),
            seq(from=0.2,to=1,     length=N[i]),
            1)
    colorseq = append(colorseq,c)

    lines( points[Start[i]:End[i],], pch=20, col=hsv(color[i]/12, 0.05, 0.9))
}

# draw points with color sequence
points(points,pch=20,col=colorseq)

xwidth = (maxx-minx)*0.025
ywidth = (maxy-miny)*0.025
nbox = 12
j = (0:(nbox-1))*xwidth
start.x = maxx-nbox*xwidth*1.1
start.y = miny+2.0*ywidth
height = ywidth
for (i in 1:P)
{
    rect(start.x+j,start.y,start.x+xwidth+j,start.y+height, col=colorseq[floor(seq(from=Start[i],to=End[i],length=nbox+1))])
    start.x = start.x
    start.y = start.y - ywidth
                                        # dev.copy2eps(file="NNI-SPR.eps")
    text(points[Start[i],1],points[Start[i],2]-yw*0.001,pos=1,paste("S",i,sep=""))
    text(points[End[i],1],points[End[i],2]-yw*0.001,pos=1,paste("E",i,sep=""))
}

