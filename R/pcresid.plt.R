"pcresid.plt"<-
function(pc, vert = TRUE, pch = 1, txtlab = 1:dim(pc$x)[1])
{
	n <- dim(pc$x)[1]
	p <- dim(pc$x)[2]
	if(vert)
		rows <- c(2, 1)
	else rows <- c(1, 2)
        cat("\nLeft-mouse-click to return to menu\n")
	par(mfrow = rows, mar = rep(4.1, 4))
	for(i in 1:p) {
		if(is.numeric(pch))
			plot(pc$lambda, pc$x[, i], pch = pch, xlab = 
				"Locations", ylab = dimnames(pc$x)[[2]][i], cex
				 = 0.8)
		else {
			plot(pc$lambda, pc$x[, i], pch = pch, xlab = 
				"Locations", ylab = dimnames(pc$x)[[2]][i], cex
				 = 0.8, type = "n")
			text(pc$lambda, pc$x[, i], txtlab)
		}
		mtext(paste("Response Curve : Var # ", i), line = 1, cex = 1)
		lines(pc$lambda[pc$tag], pc$s[pc$tag, i], col = 2, lwd = 1.5)
		if(is.numeric(pch))
			plot(pc$lambda, pc$x[, i] - pc$s[, i], pch = pch, xlab
				 = "Locations", ylab = dimnames(pc$x)[[2]][i], 
				cex = 0.8)
		else {
			plot(pc$lambda, pc$x[, i] - pc$s[, i], pch = pch, xlab
				 = "Locations", ylab = dimnames(pc$x)[[2]][i], 
				cex = 0.8, type = "n")
			text(pc$lambda, pc$x[, i] - pc$s[, i], txtlab)
		}
		mtext("Residual Plot", line = 1, cex = 1)
		abline(0, 0, lty = 2, col = 3)
		lines(lowess(pc$lambda, pc$x[, i] - pc$s[, i]), col = 2, lwd = 
			1.5)
		locator(1)
	}
	par(mfrow = c(1, 1), mar = c(5, 4, 3, 3) + 0.1)
	invisible()
}
