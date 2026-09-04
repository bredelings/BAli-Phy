args <- commandArgs(trailingOnly=TRUE)
infile <- args[1]
outfile <- args[2]
plot_title <- args[3]
curve_label <- args[4]

# Read each blank-line-separated predicate as its own curve so unrelated predicates are not joined.
read_srq_curves <- function(filename) {
    lines <- readLines(filename, warn=FALSE)
    curves <- list()
    current <- character()

    for (line in c(lines, "")) {
        if (grepl("^[[:space:]]*$", line)) {
            if (length(current) > 0) {
                curve <- read.table(text=paste(current, collapse="\n"), header=FALSE)
                if (ncol(curve) < 2) stop("SRQ curves must contain at least two columns")
                curves[[length(curves) + 1]] <- curve[, 1:2, drop=FALSE]
                current <- character()
            }
        } else {
            current <- c(current, line)
        }
    }

    if (length(curves) == 0) stop("SRQ input contains no curves")
    curves
}

curves <- read_srq_curves(infile)
wrapped_title <- paste(strwrap(plot_title, width=34), collapse="\n")

# Prefer Cairo because macOS Quartz can crash when the dynamic-library search path is overridden.
# Fall back to R's configured bitmap device for installations built without Cairo.
png_type <- if (isTRUE(capabilities("cairo"))) "cairo" else getOption("bitmapType")
png(filename=outfile, width=600, height=600, res=144, type=png_type)
par(mar=c(4.2, 4.2, 4.2, 1.0) + 0.1)
plot(NA,
     xlim=c(0, 1), ylim=c(0, 1), xaxs="i", yaxs="i",
     xlab="Regenerations (fraction)", ylab="Time (fraction)",
     main=wrapped_title)
for (curve in curves) {
    lines(curve[, 1], curve[, 2], col="#1F5A94", lwd=1.25)
}
abline(a=0, b=1, col="#A13D2D", lty=2, lwd=1.25)
legend("bottomright",
       legend=c(curve_label, "Goal"),
       col=c("#1F5A94", "#A13D2D"), lty=c(1, 2), lwd=1.25,
       bg="white")
dev.off()
