"pcdists.plt"<-
function(pc, pch = ".")
{
	p <- dim(pc$x)[2]
	n <- dim(pc$x)[1]
        cat("\nLeft-mouse-click to return to menu\n")
	par(mfrow = c(2, 2), cex = 1)
	d1 <- dist(matrix(pc$lambda, ncol = 1))
	d2 <- dist(pc$s)
	d2m <- dist(mdsform(pc$s), "man")/2
	d3 <- dist(pc$x)
	d3m <- dist(mdsform(pc$x), "man")/2
	chidist <- sqrt(pc$dist/(n * (p - 1) - sum(pc$df)))
	plot(d1, d3, xlab = "Distances between estimated locations", ylab = 
		"Distances between data", main = "Euclidean - Raw data", pch = 
		pch)
	abline(h = chidist, lty = 2, col = 2)
	plot(d1, d2, xlab = "Distances between estimated locations", ylab = 
		"Distances between fitted points", main = "Euclidean - Fitted", 
		pch = pch)
	abline(h = chidist, lty = 2, col = 2)
	plot(d1, d3m, xlab = "Distances between estimated locations", ylab = 
		"Distances between data", main = "Bray-Curtis - Raw data", pch
		 = pch)
	plot(d1, d2m, xlab = "Distances between estimated locations", ylab = 
		"Distances between fitted points", main = 
		"Bray-Curtis - Fitted", pch = pch)
	par(mfrow = c(1, 1), cex = 1)
	invisible()
}
