#require(modreg) # for smooth.spline (from R 1.9 part of stats)
#require(mva) #for dist (from R 1.9 part of stats) (see zzz.R)
#require(mgcv) #for gam (demoted to within function)
#require(MASS) #for eqscplot and isoMDS(see zzz.R)
#require(vegan) #for vegdist and stepacross
"pcurve"<-
function (x, xcan = NULL, start = "ca", rank = FALSE, cv.fit = FALSE, 
    penalty = 1, cv.all = FALSE, df = "vary", fit.meth = "spline", 
    canfit = "lm", candf = FALSE, vary.adj = FALSE, subset, robust = FALSE, 
    lowf = 0.5, min.df, max.df, max.df.cv.fit, ext.dist = TRUE, 
    ext.dc = 0.9, metric = "bray", latent = FALSE, plot.pca = TRUE, 
    thresh = 0.001, plot.true = TRUE, plot.init = FALSE, plot.segs = TRUE, 
    plot.resp = TRUE, plot.cov = TRUE, maxit = 10, stretch = 2, 
    fits = FALSE, prnt.fits = TRUE, trace = TRUE, trace.all = FALSE, 
    pch = 1, row.chk0 = FALSE, col.chk0 = TRUE, use.loc = FALSE) 
{
    spline.r2 <- function(x, y, df) {
        fit <- smooth.spline(x, y, control.spar = list(low = 0), 
            df = df)
        len <- length(fit$x)
        ins <- rep(1:len, fit$w)
        fit$x <- fit$x[ins]
        fit$y <- fit$y[ins]
        fit$yin <- fit$yin[ins]
        res <- fit$yin - fit$y
        totss <- sum((fit$yin - mean(fit$yin))^2)
        1 - sum(res^2)/totss
    }
    this.call <- match.call()
    x <- as.matrix(x)
    if (!missing(subset)) {
        x <- x[subset, ]
        if (!is.null(xcan)) 
            xcan <- xcan[subset, ]
        if (is.numeric(start)) 
            start <- start[subset]
    }
    if (row.chk0) {
        chk <- apply(x, 1, sum)
        x <- x[chk != 0, ]
    }
    if (col.chk0) {
        chk <- apply(x, 2, sum)
        x <- x[, chk != 0]
    }
    d <- dim(x)
    n <- d[1]
    p <- d[2]
    min.df <- 2
    if (missing(max.df)) 
        max.df <- 5 * log10(n)
    if (missing(max.df.cv.fit)) 
        max.df.cv.fit <- 6 * log10(n)
    if (penalty == "np") {
        np <- n * p
        if (np >= 1000) 
            penalty <- 1
        else if (np <= 100) 
            penalty <- 2
        else penalty <- 4 - log(np, 10)
    }
    pdfs <- 1:p
    if (is.null(dimnames(x))) 
        dimnames(x) <- list(1:n, paste("Response", 1:p))
    if (!is.null(xcan)) {
        if (is.vector(xcan) | is.factor(xcan) | is.null(dim(xcan))) 
            xcan <- as.matrix(xcan, ncol = 1)
        dcan <- dim(xcan)
        ncan <- dcan[1]
        pcan <- dcan[2]
        for (i in 1:pcan) if (is.character(xcan[, i])) 
            xcan[, i] <- as.factor(xcan[, i])
        xcan <- data.frame(xcan)
        if (is.null(names(xcan))) 
            names(xcan) <- list(1:ncan, paste("Cov", 1:pcan, 
                sep = "."))
        if (length(candf == 1)) 
            candf <- rep(candf, pcan)
    }
    pcax <- pca(x)
    rota <- pcax$v
    dist.old <- 10^20
    dist.raw <- sum(diag(var(x))) * (n - 1)
    if (inherits(start, "principal.curve")) 
        pcurve <- pcurve.start <- start
    else if (missing(start)) {
        pcurve <- pcurve.start <- startPC(x, ext.dist = ext.dist, 
            dc = ext.dc, rank = rank, type = "ca", metric = metric)
    }
    else pcurve <- pcurve.start <- startPC(x, ext.dist = ext.dist, 
        dc = ext.dc, rank = rank, type = start, metric = metric)
    if (use.loc) 
        cat("\nYou have specified use.loc = TRUE:\nTo progress to next plot,\nleft-mouse-click on current plot...\n")
    if (robust) 
        cat("\nRobust lowess fit : f = ", lowf, "\n")
    else if (df[1] == "vary") 
        pss <- 1:p
    else if (length(df) == 1) {
        df <- rep(df, p)
        cat("\nB-spline fit : DF = ", round(df, 2), "\n")
    }
    else cat("\nB-spline fit : DF = ", round(df, 2), "\n")
    if (plot.init) {
        resp.n <- n.plt(p)
        par(oma = c(0, 0, 2, 0), mar = c(4, 5, 1, 1))
        par(mfrow = resp.n, cex = 1.25)
    }
    for (i in 1:p) {
        fit <- smooth.spline(pcurve$lambda, x[, i], control.spar = list(low = 0), 
            penalty = penalty)
        if (plot.init) {
            plot(pcurve$lambda, x[, i], pch = pch, xlab = "Locations", 
                ylab = dimnames(x)[[2]][i])
            lines(fit$x, fit$y, col = 2, lwd = 1.5)
            if (floor(i/6) == (i - 1)/6) {
                if ((is.numeric(start)) | (inherits(start, "principal.curve"))) 
                  mtext("Response plots and spline fits : Init config : User supplied", 
                    line = 1, cex = 1, outer = TRUE)
                else mtext(paste("Response plots and spline fits : Init config : ", 
                  start), line = 1, cex = 1, outer = TRUE)
            }
            if ((floor(i/6) == i/6) & use.loc) 
                locator(1)
        }
        if (df[1] == "vary" & !robust) 
            pdfs[i] <- max(min(fit$df, max.df), min.df)
        NULL
    }
    if (plot.init) {
        par(mfrow = c(1, 1), cex = 1)
        if (use.loc) {
            loc <- locator(1)
            if (length(loc) == 0) 
                stop("\nUser termination\n")
        }
    }
    if (df[1] == "vary" & !robust) {
        if (!vary.adj) 
            df <- rep(median(pdfs), p)
        else df <- pdfs
        cat("GCV DFs : Penalty = ", round(penalty, 2))
        if (vary.adj) 
            cat("\nPC B-spline fit DF : ", round(df, 2), "\n")
        else cat("\nPC B-spline fit DF = ", round(mean(df), 2), 
            "\n")
    }
    if (plot.true) {
        par(oma = c(0, 0, 0, 0), mar = c(4, 5, 2, 2))
        if (plot.pca) {
            swx <- apply(x, 2, mean)
            xx <- sweep(x, 2, swx) %*% rota
            ss <- sweep(pcurve$s, 2, swx) %*% rota
        }
        else {
            xx <- x
            ss <- pcurve$s
        }
        eqscplot(xx[, 1:2], tol = 0.2, pch = pch, axes = FALSE, 
            xlab = "Dim 1", ylab = "Dim 2")
        if ((is.numeric(start)) | (inherits(start, "principal.curve"))) 
            mtext("PCA plot : Init config : User supplied", line = 0, 
                cex = 1)
        else mtext(paste("PCA plot : Init config : ", start), 
            line = 0, cex = 1)
        if (plot.segs) 
            segments(xx[, 1], xx[, 2], ss[, 1], ss[, 2], col = 3)
        lines(ss[pcurve$tag, 1:2], lty = 2, col = 2)
    }
    it <- 0
    used.cv <- FALSE
    loopon <- TRUE
    while (loopon | cv.fit) {
        it <- it + 1
        s <- NULL
        if (!is.null(xcan)) {
            locs <- pcurve$lambda
            if (length(locs[is.na(locs)]) > 0) 
                locs <- pcurve.start$lambda
            pname <- xs <- vector("character", pcan)
            assign("data", data.frame(locs, xcan))
            assign("candf", candf)
            if (canfit == "gam") {
                for (i in 1:pcan) if (!candf[i]) 
                  pname[i] <- paste("s(", names(xcan)[i], ")", 
                    sep = "")
                else pname[i] <- paste("s(", names(xcan)[i], 
                  ", fx = TRUE, k = ", candf[i] + 1, ")", sep = "")
                for (i in 1:(pcan - 1)) xs[i] <- paste(pname[i], 
                  "+", sep = "")
                xs[pcan] <- pname[pcan]
            }
            else {
                for (i in 1:(pcan - 1)) xs[i] <- paste(names(xcan)[i], 
                  "+", sep = "")
                xs[pcan] <- names(xcan)[pcan]
            }
            xss <- paste(xs, collapse = "", sep = "")
            formu <- formula(paste("locs~", xss, sep = ""))
            assign("xss", xss)
            assign("formu", formu)
            if (canfit == "gam") {
                require(mgcv)
                pcurve$lambda <- fitted(zfit <- gam(formu, data = data))
            }
            else {
                pcurve$lambda <- check.lambda <- fitted(zfit <- lm(formu, 
                  data = data))
                partres <- (predict.lm(zfit, type = "terms") + 
                  zfit$residuals)
            }
            zfit$data <- data
            if (trace) {
                cat("Fitting covariates: %var explained >  ", 
                  round(cor(pcurve$lambda, locs)^2 * 100, 2), 
                  "\n")
            }
        }
        for (j in 1:p) {
            if (loopon) {
                if (!robust) {
                  if (fit.meth == "spline") {
                    if (!cv.all) 
                      spline.fit <- smooth.spline(pcurve$lambda, 
                        x[, j], control.spar = list(low = 0), 
                        cv = FALSE, penalty = penalty, df = df[j])
                    else {
                      spline.fit <- smooth.spline(pcurve$lambda, 
                        x[, j], control.spar = list(low = 0), 
                        penalty = penalty)
                      df[j] <- spline.fit$df
                    }
                    sj <- spline.fit$y
                  }
                  else if (fit.meth == "poisson") {
                    require(mgcv)
                    dfj <- round(df[j])
                    sj <- fitted(gam(x[, j] ~ s(pcurve$lambda, 
                      m = dfj), family = poisson()))[order(pcurve$lambda)]
                  }
                  else if (fit.meth == "binomial") {
                    dfj <- round(df[j])
                    require(mgcv)
                    sj <- fitted(gam(x[, j] ~ s(pcurve$lambda, 
                      m = dfj), family = binomial()))[order(pcurve$lambda)]
                  }
                  else if (fit.meth == "lowess")
                    sj <- lowess(pcurve$lambda, x[, j], f = lowf)$y
                }
                else sj <- lowess(pcurve$lambda, x[, j], f = lowf)$y
              }
            else {
                fit <- smooth.spline(pcurve$lambda, x[, j], control.spar = list(low = 0), 
                  penalty = penalty)
                if (fit$df > max.df.cv.fit) 
                  fit <- smooth.spline(pcurve$lambda, x[, j], 
                    control.spar = list(low = 0), df = max.df.cv.fit, 
                    penalty = penalty)
                sj <- fit$y
                df[j] <- fit$df
                cv.fit <- FALSE
                used.cv <- TRUE
            }
            s <- cbind(s, sj)
        }
        dist.old <- pcurve$dist
        if (fit.meth == "spline" & !robust) 
            pcurve <- pcget.lam(x, s, latent = latent, stretch = stretch, 
                uni.lam = spline.fit$x)
        else pcurve <- pcget.lam(x, s, latent = latent, stretch = stretch, 
            uni.lam = sort(unique(pcurve$lambda)))
        if (trace.all) 
            print(pcurve)
        perc <- round(100 * (1 - pcurve$dist/dist.raw), 2)
        if (trace) 
            if (!used.cv) 
                cat("Iter ", it, " --- % dist^2 expl : ", perc, 
                  "  Length : ", round(max(pcurve$lambda), 2), 
                  "\n")
            else cat("CV Fit", it, " --- % dist^2 expl : ", perc, 
                "  Length : ", round(max(pcurve$lambda), 2), 
                "\n")
        if (plot.true) {
            if (plot.pca) {
                ss <- sweep(pcurve$s, 2, swx) %*% rota
            }
            else {
                xx <- x
                ss <- pcurve$s
            }
            eqscplot(xx[, 1:2], tol = 0.2, pch = pch, axes = FALSE, 
                xlab = "Dim 1", ylab = "Dim 2")
            mtext(paste("Iter # ", it, "  % d^2 explained = ", 
                perc), line = 0, cex = 1)
            if (plot.segs) 
                segments(xx[, 1], xx[, 2], ss[, 1], ss[, 2], 
                  col = 3)
            lines(ss[pcurve$tag, 1:2], col = 2, lwd = 2)
        }
        if (!used.cv) 
            loopon <- ((abs(dist.old - pcurve$dist)/dist.old > 
                thresh) & (it < maxit))
    }
    if (robust) 
        df <- rep(10^(-lowf) * 20, p)
    sdf <- (n * (p - 1) - sum(df))
    cat("\n% d^2 expl = ", round(100 * (1 - pcurve$dist/dist.raw), 
        2), ": s^2 = ", round(pcurve$dist/sdf, 3), ": Aprx. dfs = ", 
        round(sdf, 1), "\n\n")
    if (fits) {
        ssfit <- ss[pcurve$tag, ]
        pcfits <- (ssfit[2:n, ] - ssfit[1:(n - 1), ])^2
        minnp <- min(c(p, n))
        for (i in minnp:2) pcfits[, i] <- apply(pcfits[, 1:i], 
            1, sum)
        pcfits <- sqrt(pcfits)
        pcfits <- apply(pcfits, 2, sum)
        pcafits <- diag(pcax$d^2)/sum(diag(pcax$d^2))
        fits.df <- data.frame(round(100 * pcafits, 2), round(100 * 
            cumsum(pcafits), 2), round((100 * pcfits)/pcfits[minnp], 
            2))
        names(fits.df) <- c("PCA % var", "Cum PCA %var", "Cum PC Lengths")
        if (prnt.fits) 
            print(fits.df[1:min(c(6, p, n)), ])
        pfr <- r2 <- 1:p
        names(pfr) <- names(r2) <- dimnames(x)[[2]]
        for (i in 1:p) {
            r2[i] <- spline.r2(pcurve$lambda, x[, i], df[i])
            pfr[i] <- 1 - pf(r2[i]/(1 - r2[i]), df[i], n - df[i] - 
                1)
        }
        cat("\n % R2 for smooths : \n")
        if (prnt.fits) 
            print(100 * round(r2, 4))
        cat("\n % Prob (F) for smooths : \n")
        if (prnt.fits) 
            print(round(pfr, 4))
        if (!is.null(xcan) & prnt.fits) {
            if (canfit == "gam") {
                cat("\nCW Advice required on extracting linear components from gam as can be done for gam.objects in S+.  \nTry setting canfit to lm\n\n")
            }
            else print.gd(summary(zfit))
        }
    }
    if (plot.resp) {
        if (use.loc) 
            locator(1)
        par(oma = c(0, 0, 2, 0), mar = c(4, 5, 1, 1) + 0.1)
        resp.n <- n.plt(p)
        par(mfrow = resp.n, cex = 1.25)
        for (i in 1:p) {
            plot(pcurve$lambda, x[, i], pch = pch, xlab = "Locations", 
                ylab = dimnames(x)[[2]][i])
            lines(pcurve$lambda[pcurve$tag], pcurve$s[pcurve$tag, 
                i], col = 2, lwd = 1.5)
            if (floor(i/6) == (i - 1)/6) 
                mtext("Response plots and fitted curves", line = 1, 
                  cex = 1, outer = TRUE)
            if ((floor(i/6) == i/6) & use.loc) 
                locator(1)
        }
        mtext("Response plots and fitted curves", line = 1, cex = 1, 
            outer = TRUE)
        par(oma = c(0, 0, 0, 0), mfrow = c(1, 1), cex = 1)
    }
    if (!is.null(xcan) & plot.cov) {
        if (use.loc) 
            locator(1)
        par(oma = c(0, 0, 2, 0), mar = c(4, 5, 2.5, 1) + 0.1)
        cov.n <- n.plt(pcan)
        par(mfrow = cov.n, cex = 1.25)
        if (canfit == "gam") 
            plot.gam(zfit, se = TRUE, rug = TRUE)
        mtext("Covariate partial effects plots", line = 1, cex = 1, 
            outer = TRUE)
        if (canfit == "lm") 
            for (i in 1:pcan) {
                plot(xcan[, i], partres[, i], pch = pch, xlab = dimnames(xcan)[[2]][i], 
                  ylab = "Partial residual")
                abline(coef(lm(partres[, i] ~ xcan[, i])))
            }
    }
    par(mfrow = c(1, 1), cex = 1)
    par(oma = c(0, 0, 0, 0), mar = c(4, 5, 3, 3) + 0.1)
    if (fits) {
        if (!is.null(xcan)) 
            fit.lst <- list(fits.df = fits.df, zfit = zfit, r2 = r2, 
                pfr = pfr)
        else fit.lst <- list(fits.df = fits.df, r2 = r2, pfr = pfr)
        structure(list(s = pcurve$s, x = x, tag = pcurve$tag, 
            lambda = pcurve$lambda, df = df, dist = pcurve$dist, 
            fit.lst = fit.lst, call = this.call), class = "principal.curve")
    }
    else structure(list(s = pcurve$s, x = x, tag = pcurve$tag, 
        lambda = pcurve$lambda, df = df, dist = pcurve$dist, 
        call = this.call), class = "principal.curve")
}

