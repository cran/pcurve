"cavecs"<-
function(table, type = "SY", ret = TRUE, retn = 1, plt = FALSE, chk0 = TRUE)
{
	fail <- FALSE
	if(!is.matrix(table))
		stop("Not a table")
	if(chk0) {
		chk <- apply(table, 1, sum)
		table <- table[chk != 0,  ]
	}
	Dr <- apply(table, 1, sum)/sum(table)
	Dc <- apply(table, 2, sum)/sum(table)
	if(any(Dr <= 0) || any(Dc <= 0)) {
		cat("\nEmpty row or column (or negative sum) in table\n")
		fail <- TRUE
	}
	else {
		Dr <- 1/sqrt(Dr)
		Dc <- 1/sqrt(Dc)
		X <- diag(Dr) %*% (table/sum(table)) %*% diag(Dc)
		dimnames(X) <- dimnames(table)
		X.svd <- svd(X)
		d <- X.svd$d[-1]
		h <- diag(Dr) %*% X.svd$u[, -1]
		g <- diag(Dc) %*% X.svd$v[, -1]
		if(type == "PC") {
			h <- h %*% diag(d)
		}
		else if(type == "CV") {
			g <- g %*% diag(d)
		}
		else {
			h <- h %*% diag(sqrt(d))
			g <- g %*% diag(sqrt(d))
		}
		if(plt)
			eqscplot(h[, 1], h[, 2], type = "p", main = "CA Plot", 
				xlab = "1st CA Variate", ylab = 
				"2nd CA Variate")
	}
	if(ret)
		if(!fail)
			h[, retn]
		else fail
}
