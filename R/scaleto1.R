"scaleto1"<-
function(x)
{
	scale(x, center = FALSE, scale = apply(x, 2, max))
}
