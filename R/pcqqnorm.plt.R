"pcqqnorm.plt"<-
function(pc, id = 3, resp.n, txtlab = 1:dim(pc$x)[1])
{
	n <- dim(pc$x)[1]
	p <- dim(pc$x)[2]
	if(missing(resp.n))
		resp.n <- n.plt(p)
        cat("\nLeft-mouse-click to return to menu\n")
	par(mfrow = resp.n, cex = 1.25)
	for(i in 1:p) {
		res <- pc$x[, i] - pc$s[, i]
		ord <- order(res)
		res <- res[ord]
		labs <- txtlab[ord]
		x <- qnorm(ppoints(length(res)))[order(order(res))]
		plot(x[(id + 1):(n - id)], res[(id + 1):(n - id)], xlim = c(min(
			x), max(x)), ylim = c(min(res), max(res)), xlab = 
			"Quantiles of Standard Normal", ylab = "Residuals", 
			main = dimnames(pc$x)[[2]][i])
		text(x[c((1:id), (n - id + 1):n)], res[c((1:id), (n - id + 1):n
			)], labs[c((1:id), (n - id + 1):n)])
		if(trunc(i/6) == i/6)
			locator(1)
	}
	par(mfrow = c(1, 1))
	invisible()
}
