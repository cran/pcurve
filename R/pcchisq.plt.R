"pcchisq.plt"<-
function(pc, id = 6, txtlab = 1:dim(pc$x)[1])
{
	n <- dim(pc$x)[1]
	p <- dim(pc$x)[2]
	res <- pc$x - pc$s
	sigma <- apply(res^2, 2, sum)/(n - pc$df)
	sres <- res %*% diag(1/sigma)
	res <- apply(sres^2, 1, sum)
	res.sort <- sort(res)
        cat("\nLeft-mouse-click to return to menu\n")
	plot(qchisq(ppoints(res), p - 1)[1:(n - id)], res.sort[1:(n - id)], 
		xlim = c(0, max(qchisq(ppoints(res), p))), ylim = c(0, max(
		res.sort)), xlab = paste(
		"Quantiles of Chisquare Distribution : df = ", p - 1), ylab = 
		"Squared Residuals")
	text(qchisq(ppoints(res), p - 1)[n:(n - id + 1)], res.sort[n:(n - id + 
		1)], txtlab[res >= res.sort[n - id + 1]])
	mtext("Chisquare Quantile Plot", line = 1, cex = 1.25)
}
