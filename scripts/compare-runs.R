# read filename from the command line
args = commandArgs(trailingOnly=T)
filename = args[1]
outfile = args[2]

pdf(file=outfile,height=10,width=7) 

# read file
LOD = read.table(filename,header=F)
N = ncol(LOD)
L = nrow(LOD)

# sort LOD by last column
O = order(LOD[,N])
LOD = LOD[O,]

# create PP table
PP = (10**LOD)/(1+10**LOD)

# split last column out of tables
aveLOD = as.vector(LOD[,N])
avePP  = as.vector( PP[,N])

LOD = LOD[,1:N-1]
PP  = PP[,1:N-1]

# set up the two plotting surfaces
par(mfrow=c(2,1))

#-------------- Plot 1 -------------------

plot(avePP,xlab="Split",ylab="PP",type="n",ylim=c(0,1),xaxt="n")
axis(side=1,at=1:L,1:L)

# plot results from another analysis for comparison
# lines(avePP_other,col=hsv(0.63,0.4,1),lwd=2)

# Plot the bars representing the range for each split
minPP = apply(PP, 1, min, na.rm=TRUE)
maxPP = apply(PP, 1, max, na.rm=TRUE)
for(i in 1:L) {
  lines(c(i,i),c(minPP[i],maxPP[i]),lwd=2)
}

# Plot the estimate for each split, and connect the dots
lines(avePP,col=hsv(1,1,1),lwd=2)

#-------------- Plot 2 -------------------

plot(aveLOD,xlab="Split",ylab="LOD10", type="n",xaxt="n")
axis(side=1,at=1:L,1:L)

# plot results from another analysis for comparison
# lines(LOD,col=hsv(0.63,0.4,1),lwd=2)

# Plot the bars representing the range for each split
minLOD = apply(LOD, 1, min, na.rm=TRUE)
maxLOD = apply(LOD, 1, max, na.rm=TRUE)
for(i in 1:L) {
  lines(c(i,i),c(minLOD[i],maxLOD[i]),lwd=2)
}

# Plot the estimate for each split, and connect the dots
lines(aveLOD,col=hsv(1,1,1),lwd=2)

