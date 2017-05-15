# read filename from the command line
args = commandArgs(trailingOnly=T)
filename = args[1]
outfile1 = args[2]
outfile2 = args[3]

# read file
LOD = read.table(filename,header=F)
N = ncol(LOD)   # number of runs + 1
L = nrow(LOD)   # number of splits

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

lodToPP = function(x) {y=exp(x);y/(1+y)}
ppToLod = function(x) {log10(x/(1-x))}

#-------------- Plot 1 -------------------
svg(file=outfile1,height=3,width=7) 

par(mar=c(4, 4, 0, 4) + 0.1)
plot(avePP,xlab="Split",ylab="PP",type="n",ylim=c(0,1),xaxt="n")
axis(side=1,at=1:L,1:L)

# plot results from another analysis for comparison
# lines(avePP_other,col=hsv(0.63,0.4,1),lwd=2)

ptcolor = hsv(0, 1, 0, 0.5)

for(i in 1:(N-1)) {
    xs = 1:L
    xs = xs + rnorm(L, 0, N/500)
    points(xs, PP[,i], col=ptcolor, pch=".",cex=4)
}

# Plot the bars representing the range for each split
minPP = apply(PP, 1, min, na.rm=TRUE)
maxPP = apply(PP, 1, max, na.rm=TRUE)
for(i in 1:L) {
  lines(c(i,i),c(minPP[i],maxPP[i]),lwd=2)
}

lodticks = c(-3,-2,-1,0,1,2,3)
axis(side=4, at=lodToPP(lodticks),labels = lodticks)
mtext(side=4,line=3,expression(log[10](PP/(1-PP))))

ppmin = min(minPP)
ppmax = max(maxPP)

# Plot the estimate for each split, and connect the dots
lines(avePP,col=hsv(1,1,1),lwd=2)

#-------------- Plot 2 -------------------
svg(file=outfile2,height=3,width=7) 

par(mar=c(4, 4, 0, 4) + 0.1)
plot(aveLOD,xlab="Split",ylab=expression(log[10](PP/(1-PP))), type="n",xaxt="n")

axis(side=1,at=1:L,1:L)

lodToPP = function(x) {y=exp(x);y/(1+y)}
ppToLod = function(x) {log10(x/(1-x))}

#par(mar=c(4,1,1,1))

ppticks =c(0.001,0.01,0.1,0.5,0.9,0.99,0.999)
pplabels= c("0.001","0.01","0.1","0.5","0.9","0.99","0.999")
axis(side=4, at=ppToLod(ppticks),labels = pplabels)
mtext(side=4,line=3,'PP')

# plot results from another analysis for comparison
# lines(LOD,col=hsv(0.63,0.4,1),lwd=2)

for(i in 1:(N-1)) {
    xs = 1:L
    xs = xs + rnorm(L, 0, N/500)
    points(xs, LOD[,i], col=ptcolor, pch=".",cex=4)
}

# Plot the bars representing the range for each split
minLOD = apply(LOD, 1, min, na.rm=TRUE)
maxLOD = apply(LOD, 1, max, na.rm=TRUE)
for(i in 1:L) {
  lines(c(i,i),c(minLOD[i],maxLOD[i]),lwd=2)
}

# Plot the estimate for each split, and connect the dots
lines(aveLOD,col=hsv(1,1,1),lwd=2)

