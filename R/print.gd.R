"print.gd"<-
function(x, digits = max(3, .Options$digits - 3), ...)
{
	resid <- x$residuals
	df <- x$df
	rdf <- df[2]
	if(nsingular <- df[3] - df[1])
		cat("\nCoefficients: (", nsingular, 
			" not defined because of singularities)\n", sep = "")
	else cat("Coefficients:\n")
	print(format(round(x$coef, digits = digits)), quote = FALSE, ...)
	cat("Multiple R-Squared:", format(signif(x$r.squared, digits)), "\n")
	invisible(x)
}
