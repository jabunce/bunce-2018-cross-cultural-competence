#helper functions


#WAIC function
waic <- function(stanfit){
  log_lik <- extract (stanfit, "log_lik")$log_lik
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)
  lpd <- log(colMeans(exp(log_lik)))
  p_waic <- colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2*elpd_waic
  loo_weights_raw <- 1/exp(log_lik-max(log_lik))
  loo_weights_normalized <- loo_weights_raw/
    matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
  loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized)/
    colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
    p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
    pointwise=pointwise, total=total, se=se))
}

#colVars function needed by waic function
colVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=FALSE) {
  if (SumSquares) return(colSums(x^2, na.rm, dims))
  N <- colSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {x <- if (dims==length(dim(x))) x - mean(x, na.rm=na.rm) else
                     sweep(x, (dims+1):length(dim(x)), colMeans(x,na.rm,dims))}
  (colSums(x^2, na.rm, dims) - colSums(x, na.rm, dims)^2/N) / Nm1
}



########################### density plotting function

denschart3 <- function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
    pt.cex = cex, bg = par("bg"), 
    color = "grey20", colorHPDI ="grey60", HPDI=0.9, vline = NULL,
    gcolor = par("fg"), lcolor = "gray", xlim = range(unlist(x)), yvals = 1:length(x), yextra=0.7,
    main = NULL, xlab = NULL, ylab = NULL, height=0.7 , border=NA, adjust=1, ...) 
  {
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")
    if (!is.list(x)) 
        stop("'x' must be a list of vectors or matrices")
    n <- length(x)
    glabels <- NULL
    if (is.list(x)) {
        if (is.null(labels)) 
            labels <- names(x)
        if (is.null(labels)) 
            labels <- as.character(1L:n)
        labels <- rep_len(labels, n)
        #if (is.null(groups)) 
        #    groups <- col(x, as.factor = TRUE)
        #glabels <- levels(groups)
    }
    plot.new()
    linch <- if (!is.null(labels)) 
        max(strwidth(labels, "inch"), na.rm = TRUE)
    else 0
    if (is.null(glabels)) {
        ginch <- 0
        goffset <- 0
    }
    else {
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- 0.4
    }
    if (!(is.null(labels) && is.null(glabels))) {
        nmai <- par("mai")
        nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
            0.1
        par(mai = nmai)
    }
    if (is.null(groups)) {
        o <- 1L:n
        y <- yvals #o    						#vertical spacing
        ylim <- c(0.2, max(y) + yextra) #n + 1)
    }
    else {
        # sub-groups, so need more rows
        o <- sort.list(as.numeric(groups), decreasing = TRUE)
        x <- x[o]
        groups <- groups[o]
        color <- rep_len(color, length(groups))[o]
        lcolor <- rep_len(lcolor, length(groups))[o]
        offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
        y <- 1L:n + 2 * offset
        ylim <- range(0, y + 2)
    }
    plot.window(xlim = xlim, ylim = ylim, log = "")
    lheight <- par("csi")
    if (!is.null(labels)) {
        linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
        loffset <- (linch + 0.1)/lheight
        labs <- labels[o]
        #mtext(labs, side = 2, line = -1 #0.4, #loffset,           #### y-labels
        #      at = y, adj = 1, 
        #      col = "black", las = 2, cex = cex, ...)
        
        #text(labels=labs, x=-5.2, y=y, pos=2, adj=1)
    }

    #vertical lines
    if ( !is.null(vline) ) {
    	for ( t in 1:length(vline) ) {
    		lines(x=list( x=c(vline[t],vline[t]), y=c(0,max(y)+0.5) ), lty=2, lwd=0.75)
    	} # for t
    } #if


    # draw densities at each y offset
    for ( i in 1:n ) {
        a <- density( x[[i]] , adjust=adjust )
        a$y <- a$y/max(a$y) * height + y[i] - 0.3
        polygon( a$x , a$y , col=color , border=NA ) 
        Cuts <- HPDI( x[[i]] , HPDI)
        XX <- a$x[which(a$x > Cuts[1] & a$x < Cuts[2])]
        YY <- a$y[which(a$x > Cuts[1] & a$x < Cuts[2])]
        polygon( c(min(XX), XX, max(XX)), c(min(a$y), YY, min(a$y)),
          col=colorHPDI, border=NA )
    }

    if (!is.null(groups)) {
        gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
            2) - 1)
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
        mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
            col = gcolor, las = 2, cex = cex, ...)
        if (!is.null(gdata)) {
            abline(h = gpos, lty = "dotted")
            points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
                cex = pt.cex/cex, ...)
        }
    }
    #axis(side=1, at=c(-6,-4,-2,0,2,4,6))
    #box()
    #title(main = main, xlab = xlab, ylab = ylab, ...)
    invisible()
}


#################### Stan traces printing function

Stan_traces <- function(m = m1){
    for ( z2 in 1:J ) {
        for ( z3 in 1:T ) {
            print(traceplot(m, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
        }
    }
    for ( z1 in 1:Mach) {
        for ( z2 in 1:T ) {
            print(traceplot(m, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
        }
    }
    for ( z1 in 1:Mach) {
        for ( z2 in 1:T ) {
            print(traceplot(m, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
        }
    }
    for ( z1 in 1:Mach) {
        for ( z2 in 1:T ) {
            for ( z3 in 1:T ) {
                print(traceplot(m, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
            }
        }
    }
    for ( z2 in 1:(T*2) ) {
        for ( z3 in 1:K ) {
            print(traceplot(m, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
        }
    }
    for ( z2 in 1:T ){
        print(traceplot(m, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
    }
    for ( z3 in 1:T ){
        print(traceplot(m, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
    }
    for ( z2 in 1:T ){
        print(traceplot(m, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
    }
    for ( z2 in 1:T ){
        print(traceplot(m, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
    }
    for ( z2 in 1:(T*2) ) {
        for ( z3 in 1:(T*2) ) {
            print(traceplot(m, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
        }
    }
    print(traceplot(m, pars="lp__", inc_warmup=T))
}

