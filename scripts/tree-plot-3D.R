args = commandArgs(trailingOnly=T)
infile = args[1]
P = length(args) - 1
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

# Actually do the MDS
library(stats)
points = cmdscale(M,k=3)

#library(MASS)
#mds = isoMDS(M)
#points = mds$points

group = 1
for(group in 1:P)
{
    for (i in Start[group]:End[group])
    {
        cat(sprintf("%f\t%f\t%f\t%i\t%i\n",points[i,1], points[i,2], points[i,3], group, i-Start[group]))
    }
}
