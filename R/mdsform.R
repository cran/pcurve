"mdsform"<-
function(x, scale = TRUE)
{
	if(scale)
		x <- scaleto1(x)
	x <- x/apply(x, 1, sum)
	x
}
