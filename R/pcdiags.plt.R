"pcdiags.plt"<-
function(zz, xx, pch = 1, graphics = TRUE)
{
	choice <- 1
	pch.no <- 1
	while(choice != 0) {
		items <- c("Residuals plot", "Absolute residuals plot", 
			"QQnorm residuals plot", "QQchisq residuals plot", 
			"Response-residuals plots", "Differenced locations", 
			"Response plots", "Flip plots (R mouse-click to exit)", "Fix curve", 
			"Scatterplots of distances", "Histograms of distances", 
			ifelse(is.numeric(pch), "Use Case for plot symbols?", 
			"Use plot symbols?"))
		if(!missing(xx))
			items <- c(items, "Env. vars. vs Gradient")
		choice <- menu(items, graphics = graphics, 
			"\nPrincipal Curves Fit Analysis\n")
		switch(choice + 1,
			stop,
			{
				pcres1.plt(zz)
				locator(1)
			}
			,
			{
				pcres2.plt(zz)
				locator(1)
			}
			,
			{
				pcqqnorm.plt(zz)
				locator(1)
			}
			,
			{
				pcchisq.plt(zz)
				locator(1)
			}
			,
			pcresid.plt(zz, pch = pch),
			{
				pcfinder.plt(zz)
				locator(1)
			}
			,
			{
				pcresp.plt(zz, pch = pch)
				locator(1)
			}
			,
			pcflip.plt(zz, pch = pch),
			{
				zz <- finder(zz, TRUE, 10)
			}
			,
			{
				pcdists.plt(zz)
				locator(1)
			}
			,
			{
				pchist.plt(zz)
				locator(1)
			}
			,
			pch <- ifelse(is.numeric(pch), "C", pch.no)
                        ,
			{
				pcenv.plt(zz[[4]], xx, pch = pch)
				locator(1)
			}
			)
	}
}


