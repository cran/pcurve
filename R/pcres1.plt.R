"pcres1.plt"<-
function(pc, id = 3, resp.n)
{
	n <- dim(pc$x)[1]
	p <- dim(pc$x)[2]
	if(missing(resp.n))
		resp.n <- n.plt(p)
        cat("\nLeft-mouse-click to return to menu\n")
        par(mfrow = resp.n, cex = 1.25)
        for(i in 1:p) {
		plot(pc$s[, i], pc$x[, i] - pc$s[, i], xlab = "Fitted values", 
			ylab = "Residuals", main = dimnames(pc$x)[[2]][i])
		abline(h = 0, lty = 2, col = 3)
		if(trunc(i/6) == i/6)
			locator(1)
	}
	par(mfrow = c(1, 1))
	invisible()
}
