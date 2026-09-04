args <- commandArgs(trailingOnly=TRUE)
infile <- args[1]
outfile <- args[2]
extended_infile <- if (length(args) >= 3) args[3] else NULL

# Read the LOD threshold and supported-split count used by the report plot.
read_support_curve <- function(filename) {
    curve <- read.table(filename, header=FALSE)
    if (ncol(curve) < 2) stop("Support-level data must contain at least two columns")
    curve[, 1:2, drop=FALSE]
}

curves <- list(read_support_curve(infile))
labels <- character()
colors <- "#1F5A94"
if (!is.null(extended_infile)) {
    curves[[2]] <- read_support_curve(extended_infile)
    labels <- c("Full Splits", "Partial Splits")
    colors <- c("#1F5A94", "#A13D2D")
}

# LOD 0 corresponds to posterior probability 0.5; lower-support rows are intentionally omitted.
curves <- lapply(curves, function(curve) curve[curve[, 1] >= 0, , drop=FALSE])
visible_x <- unlist(lapply(curves, function(curve) curve[, 1]))
visible_y <- unlist(lapply(curves, function(curve) curve[, 2]))
xmax <- if (length(visible_x) > 0) max(visible_x) else 1
ymax <- if (length(visible_y) > 0) max(visible_y) else 1
if (xmax == 0) xmax <- 1
if (ymax == 0) ymax <- 1

svg(filename=outfile, width=7.5, height=6)
par(mar=c(4.2, 4.2, 1.0, 1.0) + 0.1)
plot(NA,
     xlim=c(0, xmax), ylim=c(0, ymax), xaxs="i", yaxs="i",
     xlab="Log10 posterior Odds (LOD)", ylab="Supported Splits")
for (i in seq_along(curves)) {
    lines(curves[[i]][, 1], curves[[i]][, 2], col=colors[i], lwd=1.25)
}
if (length(labels) > 0) {
    legend("topright", legend=labels, col=colors, lty=1, lwd=1.25, bg="white")
}
dev.off()
