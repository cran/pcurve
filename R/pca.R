"pca"<-
function(mat, cent = TRUE, scle = FALSE)
{
	mat <- as.matrix(mat)
	if((!cent) & (!scle))
		z <- svd(mat)
	else z <- svd(scale(mat, center = cent, scale = scle))
	d <- diag(z$d)
	pcs <- z$u %*% d
	list(pcs = pcs, d = d, v = z$v)
}
