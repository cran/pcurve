#require(mva) for dist (from R 1.9 part of stats) (see zzz.R)
#require (vegan) for spantree

"startPC"<-
function(x, ext.dist = TRUE, dc = 0.9, rank = FALSE, type = "pca", metric = "bray", 
	fuzz = TRUE, eps = 1e-006, verb = TRUE)
{
	if(verb)
		cat("\nEstimating starting configuration using : ")
	n <- dim(x)[1]
	p <- dim(x)[2]
	dists <- sum(diag(var(x))) * (n - 1)
	if((is.numeric(type)) && (length(type) == n)) {
		if(verb)
			cat("user supplied\n")
		lambda <- type
	}
	else if(type == "pca") {
		if(verb)
			cat("PCA\n")
		lambda <- as.vector(pca(x)$pcs[, 1])
	}
	else if(type == "ca") {
		if(verb)
			cat("CA\n")
		lambda <- cavecs(x)
	}
	else if(type == "pca.bc") {
		if(verb)
			cat("PCA.BC\n")
		xx <- mdsform(x[, 1:p], scale = TRUE)
		lambda <- as.vector(pca(xx)$pcs[, 1])
	}
	else if(type == "mds") {
                require(vegan)
		if(verb)
			cat("MDS\n")
		mdssims <- vegdist(mdsform(x[, 1:p], scale = FALSE), method = "euclidean") + 
			ifelse(fuzz, eps, 0)
		lambda <- isoMDS(mdssims, y = cmdscale(mdssims),
                                 k = 2, trace = FALSE)$points[,1]
	}
	else if(type == "mds.bc") {
                require(vegan)
		if(verb)
			cat("MDS.BC\n")
			mdssims <- vegdist(x[, 1:p], method="bray") + ifelse(
				fuzz, eps, 0)
               		if(ext.dist) {
			if(verb)
				cat("Using extended distances \n")
                        mdssims <- stepacross(mdssims, toolong = dc)
		}
        	lambda <- isoMDS(mdssims, y = cmdscale(mdssims), 
			         k = 2, trace = FALSE)$points[,1]
	}
	else if(type == "cs.bc") {
                require(vegan)
		if(verb)
			cat("CS.BC\n")
		mdssims <- dist(mdsform(x[, 1:p], scale = TRUE), method = "man")/2 +
			ifelse(fuzz, eps, 0)
		if(ext.dist) {
			if(verb)
				cat("Using extended distances \n")
                        mdssims <- stepacross(mdssims, toolong = dc)
                      }
		lambda <- as.vector(cmdscale(mdssims, 2)[, 1])
	}
	else if(type == "mst.bc") {
		if(verb)
			cat("MST.BC\n")
		lambda <- spantree(mdsform(x[, 1:p], scale = TRUE))[, 1]
	}
	else if(type == "mst") {
		if(verb)
			cat("MST\n")
		lambda <- spantree(x[, 1:p])[, 1]
	}
	else if(type == "ran") {
		if(verb)
			cat("Random\n")
		lambda <- sample(1:n)
	}
	tag <- order(lambda)
	if(rank)
		lambda <- rank(lambda)
	start <- list(s = x, tag = tag, lambda = lambda, dist = dists)
	structure(start, class = "principal.curve")
}
