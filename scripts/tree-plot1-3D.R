args = commandArgs(trailingOnly=T)
infile = args[1]

M = as.matrix(read.table(infile))
L = dim(M)[1]

library(stats)
points = cmdscale(M, k=3)

for (i in 1:L)
{
    cat(sprintf("%f\t%f\t%f\n",points[i,1], points[i,2], points[i,3]))
}
