"finder"<-
function(pc, fit.pc = TRUE, maxit = 10)
{
	pc.fin <- function(pc, ns, nss, new.order, resp.n, pch = 1)
	{
		n <- dim(pc$x)[1]
		p <- dim(pc$x)[2]
		if(missing(resp.n))
			resp.n <- n.plt(p)
		par(oma = c(0, 0, 2, 0), mar = c(4, 5, 1, 1))
		par(mfrow = resp.n, cex = 1.25)
		mtext("Configuration plot", side = 3, line = 0, cex = 1, outer
			 = TRUE)	#
		if(missing(new.order)) {
			if(length(nss) == 2) {
				for(i in 1:p) {
				  plot(pc$lambda[pc$tag], pc$x[pc$tag, i], pch
				     = pch, xlab = "Locations", ylab = dimnames(
				    pc$x)[[2]][i])
				  s1 <- supsmu(pc$lambda[pc$tag][nss[[1]]], pc$
				    x[pc$tag, i][nss[[1]]], bass = 10)
				  s2 <- supsmu(pc$lambda[pc$tag][nss[[2]]], pc$
				    x[pc$tag, i][nss[[2]]], bass = 10)
				  x <- c(s1$x, NA, s2$x)
				  y <- c(s1$y, NA, s2$y)
				  lines(x, y, col = 2, lwd = 1.5)
				  abline(v = (pc$lambda[pc$tag][ns[1]] + pc$
				    lambda[pc$tag][ns[1] + 1]) * 0.5, lty = 2, 
				    col = 3)
				  if(trunc(i/6) == i/6)
				    locator(1)
				}
			}
			else if(length(nss) == 3) {
				for(i in 1:p) {
				  plot(pc$lambda[pc$tag], pc$x[pc$tag, i], pch
				     = pch, xlab = "Locations", ylab = dimnames(
				    pc$x)[[2]][i])
				  s1 <- supsmu(pc$lambda[pc$tag][nss[[1]]], pc$
				    x[pc$tag, i][nss[[1]]], bass = 10)
				  s2 <- supsmu(pc$lambda[pc$tag][nss[[2]]], pc$
				    x[pc$tag, i][nss[[2]]], bass = 10)
				  s3 <- supsmu(pc$lambda[pc$tag][nss[[3]]], pc$
				    x[pc$tag, i][nss[[3]]], bass = 10)
				  x <- c(s1$x, NA, s2$x, NA, s3$x)
				  y <- c(s1$y, NA, s2$y, NA, s3$y)
				  lines(x, y, col = 2, lwd = 1.5)
				  abline(v = (pc$lambda[pc$tag][ns[1]] + pc$
				    lambda[pc$tag][ns[1] + 1]) * 0.5, lty = 2, 
				    col = 3)
				  abline(v = (pc$lambda[pc$tag][ns[2]] + pc$
				    lambda[pc$tag][ns[2] + 1]) * 0.5, lty = 2, 
				    col = 3)
				  if(trunc(i/6) == i/6)
				    locator(1)
				}
			}
		}
		else {
			for(i in 1:p) {
				plot(rank(pc$lambda[pc$tag]), pc$x[pc$tag, i][
				  new.order], pch = pch, xlab = "Locations", 
				  ylab = dimnames(pc$x)[[2]][i])
				lines(supsmu(rank(pc$lambda[pc$tag]), pc$x[pc$
				  tag, i][new.order], bass = 5), col = 2, lwd
				   = 1.5)
				if(trunc(i/6) == i/6)
				  locator(1)
			}
		}
		par(oma = c(0, 0, 0, 0), mar = c(4, 5, 3, 3) + 0.1)
		par(mfrow = c(1, 1), cex = 1)
		invisible()
	}
	n <- dim(pc$x)[1]
	lam <- pc$lambda[pc$tag]
	retz <- FALSE
	diff <- (lam[2:n] - lam[1:(n - 1)])/median(lam[2:n] - lam[1:(n - 1)])
	plot(pc$lambda[pc$tag][1:(n - 1)], diff, type = "h", pch = 1, xlab = 
		"Order", ylab = "Differences", main = 
		"SELECT BREAKS -- no more than 2")
	points(pc$lambda[pc$tag][1:(n - 1)], diff, pch = 1, col = 4)
	abline(0, 0, lty = 2, col = 3)
	ns <- sort(identify(pc$lambda[pc$tag][1:(n - 1)], diff, col = 2, cex = 
		1.25))
	nss <- list()
	if(length(ns) == 1) {
		nss[[1]] <- 1:ns[1]
		nss[[2]] <- (ns[1] + 1):n
	}
	else if(length(ns) == 2) {
		nss[[1]] <- 1:ns[1]
		nss[[2]] <- (ns[1] + 1):ns[2]
		nss[[3]] <- (ns[2] + 1):n
	}
	else if(length(ns) == 3) {
		nss[[1]] <- 1:ns[1]
		nss[[2]] <- (ns[1] + 1):ns[2]
		nss[[3]] <- (ns[2] + 1):ns[3]
		nss[[4]] <- (ns[3] + 1):n
	}
	pc.fin(pc, ns, nss)
	locator(1)
	cat("\nEnter reordering Eg 2 -1 3 or return to quit >>  ")
	nin <- scan()
	for(i in 1:length(nin)) {
		if(nin[i] < 0)
			nss[[abs(nin[i])]] <- rev(nss[[abs(nin[i])]])
	}
	if(length(ns) == 1) {
		if(all(abs(nin) == c(1, 2)))
			new.order <- c(nss[[1]], nss[[2]])
		else if(all(abs(nin) == c(2, 1)))
			new.order <- c(nss[[2]], nss[[1]])
	}
	else if(length(ns) == 2) {
		if(all(abs(nin) == c(1, 2, 3)))
			new.order <- c(nss[[1]], nss[[2]], nss[[3]])
		else if(all(abs(nin) == c(1, 3, 2)))
			new.order <- c(nss[[1]], nss[[3]], nss[[2]])
		else if(all(abs(nin) == c(2, 1, 3)))
			new.order <- c(nss[[2]], nss[[1]], nss[[3]])
		else if(all(abs(nin) == c(2, 3, 1)))
			new.order <- c(nss[[2]], nss[[3]], nss[[1]])
		else if(all(abs(nin) == c(3, 1, 2)))
			new.order <- c(nss[[3]], nss[[1]], nss[[2]])
		else if(all(abs(nin) == c(3, 2, 1)))
			new.order <- c(nss[[3]], nss[[2]], nss[[1]])
	}
	if(fit.pc) {
		zz <- pcurve(pc$x, start = (1:n)[order(pc$tag[new.order])], df
			 = "vary", plot.init = FALSE, use.loc = FALSE, maxit = maxit)
		list(pcfit = zz, retz = TRUE)
	}
	list(pcfit = zz, retz = TRUE)
}
