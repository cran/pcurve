"gdsqform"<-
function(x, scale = FALSE)
{
	if(scale)
		x <- scaleto1(x)
	x/sqrt(apply(x^2, 1, sum))
}
