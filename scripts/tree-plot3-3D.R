args = commandArgs(trailingOnly=T)
N1 = as.numeric(args[1])
N2 = as.numeric(args[2])
N3 = as.numeric(args[3])
infile = args[4]

M = as.matrix(read.table(infile))

L = dim(M)[1]

if (dim(M)[2] != L) { stop("Matrix not square") }
if (N1 + N2 + N3 != L) { stop("Wrong number of points.") }

# Actually do the MDS
library(stats)
points = cmdscale(M,k=3)


#library(MASS)
#mds = isoMDS(M)
#points = mds$points

first = 1
group = 1
for (i in 1:L)
{
    if (i == N1+1)
    {
        group = 2
        first = N1+1
    }
    if (i == N1+N2+1)
    {
        group = 3
        first = N1+N2+1
    }
    cat(sprintf("%f\t%f\t%f\t%i\t%i\n",points[i,1], points[i,2], points[i,3],group,i-first))
}
