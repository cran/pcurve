"pcget.lam"<-
function(x, s, latent = FALSE, stretch = 2, uni.lam, tag)
{
	storage.mode(x) <- "double"
	storage.mode(s) <- "double"
	storage.mode(stretch) <- "double"
	storage.mode(uni.lam) <- "double"
	if(!missing(tag))
		s <- s[tag,  ]
	np <- dim(x)
	if(length(np) != 2)
		stop("get.lam needs a matrix input")
	n <- np[1]
	p <- np[2]	#
#
#  Transpose matrices for C -- stored by rows not cols !!!!!!!!!! and 'transpose' back !!
#
	tt <- .C("getlam",
		n,
		p,
		t(x),
		s = t(x),
                latent = as.integer(latent),
		lambda = double(n),
		tag = integer(n),
		dist = double(n),
		as.integer(nrow(s)),
		t(s),
		stretch,
		uni.lam,
		double(p),
		double(p), PACKAGE = "pcurve")[c("s", "tag", "lambda", "dist")]
	if(latent) {
		tt$lambda <- (tt$lambda - min(tt$lambda))/diff(range(tt$lambda)
			)
		tt$tag <- order(tt$lambda)
	}
	tt$s <- t(matrix(as.vector(tt$s), nrow = p, ncol = n))
	tt$dist <- sum(tt$dist)
	class(tt) <- "principal.curve"
	tt
}

