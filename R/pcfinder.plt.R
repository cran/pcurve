"pcfinder.plt"<-
function(pc)
{
	n <- dim(pc$x)[1]
	lam <- pc$lambda[pc$tag]
	diff <- (lam[2:n] - lam[1:(n - 1)])/median(lam[2:n] - lam[1:(n - 1)])
        cat("\nLeft-mouse-click to return to menu\n")
	plot(pc$lambda[pc$tag][1:(n - 1)], diff, type = "h", pch = 1, xlab = 
		"Order", ylab = "Differences")
	points(pc$lambda[pc$tag][1:(n - 1)], diff, pch = 1)
	mtext("Differences between consecutive locations on the gradient", side
		 = 3, line = 1, cex = 1.25)
	abline(0, 0, lty = 2, col = 3)
}
