"pcenv.plt"<-
function(lambda, xx, resp.n, pch = 1)
{
	xx <- data.frame(xx)
	n <- dim(xx)[1]
	p <- dim(xx)[2]
        cat("\nLeft-mouse-click to return to menu\n")
	if(is.null(names(xx)))
		names(xx) <- paste("Var", 1:p, sep = ".")
	if(p == 1)
		names(xx) <- "Selected Variable"
	if(missing(resp.n))
		resp.n <- n.plt(p)
	par(mfrow = resp.n, cex = 1.25)
	for(i in 1:p) {
		if(is.numeric(pch))
			plot(xx[, i], lambda, pch = pch, ylab = 
				"Est. Locations", xlab = names(xx)[i])
		else {
			plot(xx[, i], lambda, pch = pch, ylab = 
				"Est. Locations", xlab = names(xx)[i], type = 
				"n")
			text(xx[, i], lambda, pch = pch, 1:n)
		}
		if(is.numeric(xx[, i]))
			lines(supsmu(xx[, i], lambda, bass = 10), lwd = 1.5, 
				col = 2)
		if(trunc(i/6) == i/6)
			locator(1)
	}
	par(mfrow = c(1, 1), cex = 1)
	invisible()
}
