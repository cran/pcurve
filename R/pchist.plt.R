"pchist.plt"<-
function(pc)
{
	n <- dim(pc$x)[1]
	p <- dim(pc$x)[2]
        cat("\nLeft-mouse-click to return to menu\n")
	par(mfrow = c(2, 2), cex = 1)
	d2 <- dist(pc$s)
	d2m <- dist(mdsform(pc$s), "man")/2
	d3 <- dist(pc$x)
	d3m <- dist(mdsform(pc$x), "man")/2
	mx <- round(max(d2, d3) + 0.5)
	mdx <- max(d2m, d3m)
	ncl <- 1 + round(log(n, 2))
	hist(d3, main = "Euclidean distances - raw data ", prob = FALSE, xlab = "", 
		breaks = seq(0, mx, length = ncl))
	hist(d2, main = "Euclidean distances - fitted data ", prob = FALSE, xlab = 
		"", breaks = seq(0, mx, length = ncl))
	hist(d3m, main = "Bray-Curtis distances - raw data", prob = FALSE, xlab = 
		"", breaks = seq(0, mdx, length = ncl))
	hist(d2m, main = "Bray-Curtis distances - fitted data", prob = FALSE, xlab
		 = "", breaks = seq(0, mdx, length = ncl))
	par(mfrow = c(1, 1), cex = 1)
	invisible()
}
