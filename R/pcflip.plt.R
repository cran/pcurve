"pcflip.plt"<-
function(pc, pch = 1, txtlab = 1:dim(pc$x)[1])
{
	rota <- pca(pc$x)$v
	pc$s <- sweep(pc$s, 2, apply(pc$x, 2, mean)) %*% rota
	pc$x <- sweep(pc$x, 2, apply(pc$x, 2, mean)) %*% rota
	p <- dim(pc$x)[2]
	n <- dim(pc$x)[1]
	cv1 <- 1
	cv2 <- 2
	z <- list()
	z$x <- 0
        cat("\nLeft-mouse-click on plots to progress through principal coordinate plots.\nRight-mouse-click to return to menu\n")
        repeat {
		if(is.numeric(pch))
			eqscplot(pc$x[, c(cv1, cv2)], tol = 0.2, pch = pch,
                                axes = FALSE, xlab = paste("Dim ", cv1),
                                ylab = paste("Dim ", cv2))
		else {
			eqscplot(pc$x[, c(cv1, cv2)], pch = pch, axes = FALSE,
				xlab = paste("Dim ", cv1), ylab = paste("Dim ", 
				cv2), type = "n")
			text(pc$x[, c(cv1, cv2)], txtlab)
		}
		mtext("Flip Plot", line = 1.5, cex = 1.25)
		segments(pc$x[, cv1], pc$x[, cv2], pc$s[, cv1], pc$s[, cv2], 
			col = 3)
		lines(pc$s[pc$tag, c(cv1, cv2)], lwd = 2, col = 2)
		z <- locator(1)
		if(length(z$x)) {
			if(z$x > z$y)
				if(cv1 < p)
				  cv1 <- cv1 + 1
				else cv1 <- 1
			else if(cv2 < p)
				cv2 <- cv2 + 1
			else cv2 <- 2
			if((z$x < 0) & (z$y < 0)) {
				cv1 <- 1
				cv2 <- 2
			}
		}
		else (break)
	}
	invisible()
}



