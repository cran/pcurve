"pcresp.plt"<-
function(pc, resp.n, pch = pch)
{
	n <- dim(pc$x)[1]
	p <- dim(pc$x)[2]
	if(missing(resp.n))
		resp.n <- n.plt(p)
        cat("\nLeft-mouse-click to return to menu\n")
	par(mfrow = resp.n, cex = 1.25)
	for(i in 1:p) {
		if(is.numeric(pch))
			plot(pc$lambda, pc$x[, i], pch = pch, xlab = 
				"Locations", ylab = dimnames(pc$x)[[2]][i])
		else {
			plot(pc$lambda, pc$x[, i], pch = pch, xlab = 
				"Locations", ylab = dimnames(pc$x)[[2]][i], 
				type = "n")
			text(pc$lambda, pc$x[, i], 1:n)
		}
		lines(pc$lambda[pc$tag], pc$s[pc$tag, i], col = 2, lwd = 1.5)
		if(trunc(i/6) == i/6)
			locator(1)
	}
	par(mfrow = c(1, 1), cex = 1)
	invisible()
}
