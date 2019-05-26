
### New plot.ts
temp_plot <- function (x, y = NULL, plot.type = c('multiple', 'single'), xy.labels, xy.lines, panel = lines, nc, yax.flip = FALSE, 
                       mar.multi = c(0, 5.1, 0, if (yax.flip) 5.1 else 2.1), oma.multi = c(6, 0, 5, 0), axes = TRUE, other=NULL, other_col='red', other_lwd=2, ...) {
  
  plotts <- function(x, y = NULL, plot.type = c('multiple', 'single'), xy.labels, xy.lines, panel = lines, nc, xlabel, 
                     ylabel, type = 'l', xlim = NULL, ylim = NULL, xlab = 'Time', ylab, log = '', col = par('col'), bg = NA, pch = par('pch'), 
                     cex = par('cex'), lty = par('lty'), lwd = par('lwd'), axes = TRUE, frame.plot = axes, ann = par('ann'), cex.lab = par('cex.lab'), 
                     col.lab = par('col.lab'), font.lab = par('font.lab'), cex.axis = par('cex.axis'), col.axis = par('col.axis'), 
                     font.axis = par('font.axis'), main = NULL, other=NULL, other_col='red', other_lwd=2, ...) {
    
    plot.type <- match.arg(plot.type)
    nser <- NCOL(x)
    
    if (plot.type == 'multiple' && nser > 1) {
      addmain <- function(main, cex.main = par('cex.main'), font.main = par('font.main'), col.main = par('col.main'), ...)
        mtext(main, side = 3, line = 3, cex = cex.main, font = font.main, col = col.main, ...)
      panel <- match.fun(panel)
      nser <- NCOL(x)
      if (nser > 10) stop('cannot plot more than 10 series as \'multiple\'')
      if (is.null(main)) main <- xlabel
      nm <- colnames(x)
      if (is.null(nm))  nm <- paste('Series', 1L:nser)
      if (missing(nc)) 
        nc <- if (nser > 4) 2
      else 1
      
      nr <- ceiling(nser/nc)
      oldpar <- par(mar = mar.multi, oma = oma.multi, mfcol = c(nr, nc))
      on.exit(par(oldpar))
      
      for (i in 1L:nser) {
        plot.default(x[, i], axes = FALSE, xlab = '', ylab = '', log = log,
                     col = col, bg = bg, pch = pch, ann = ann, type = 'n', ...)
        panel(x[, i], col = col, bg = bg, pch = pch, cex = cex, lwd = lwd,
              lty = lty, type = type, ...)
        
        # EDITED CODE
        if (!is.null(other))
          lines(other[, i], col=other_col, lwd=2)
        
        if (frame.plot) box(...)
        
        y.side <- if (i%%2 || !yax.flip) 2
        else 4
        do.xax <- i%%nr == 0 || i == nser
        if (axes) {
          axis(y.side, xpd = NA, cex.axis = cex.axis, 
               col.axis = col.axis, font.axis = font.axis, 
               ...)
          if (do.xax) 
            axis(1, xpd = NA, cex.axis = cex.axis, col.axis = col.axis, 
                 font.axis = font.axis, ...)
        }
        if (ann) {
          mtext(nm[i], y.side, line = 3, cex = cex.lab, 
                col = col.lab, font = font.lab, ...)
          if (do.xax) 
            mtext(xlab, side = 1, line = 3, cex = cex.lab, 
                  col = col.lab, font = font.lab, ...)
        }
      }
      if (ann && !is.null(main)) {
        par(mfcol = c(1, 1))
        addmain(main, ...)
      }
      return(invisible())
    }
    x <- as.ts(x)
    if (!is.null(y)) {
      y <- hasTsp(y)
      if (NCOL(x) > 1 || NCOL(y) > 1) stop('scatter plots only for univariate time series')
      if (is.ts(x) && is.ts(y)) {
        xy <- ts.intersect(x, y)
        xy <- xy.coords(xy[, 1], xy[, 2], xlabel, ylabel, log)
      }
      else xy <- xy.coords(x, y, xlabel, ylabel, log)
      
      xlab <- if (missing(xlab)) xy$xlab
      else xlab
      
      ylab <- if (missing(ylab)) xy$ylab
      else ylab
      
      xlim <- if (is.null(xlim)) range(xy$x[is.finite(xy$x)])
      else xlim
      
      ylim <- if (is.null(ylim)) range(xy$y[is.finite(xy$y)])
      else ylim
      
      n <- length(xy$x)
      if (missing(xy.labels)) xy.labels <- (n <= 150)
      do.lab <- if (is.logical(xy.labels)) xy.labels
      else {
        if (!is.character(xy.labels)) stop("'xy.labels' must be logical or character")
        TRUE
      }
      ptype <- if (do.lab) 'n'
      else if (missing(type)) 'p'
      else type
      
      dev.hold()
      on.exit(dev.flush())
      plot.default(xy, type = ptype, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, log = log, col = col, 
                   bg = bg, pch = pch, cex = cex, lty = lty, lwd = lwd, axes = axes, frame.plot = frame.plot, ann = ann, main = main, ...)
      
      if (missing(xy.lines)) xy.lines <- do.lab
      if (do.lab) 
        text(xy, labels = if (is.character(xy.labels)) 
          xy.labels
          else if (all(tsp(x) == tsp(y))) 
            formatC(unclass(time(x)), width = 1)
          else seq_along(xy$x), col = col, cex = cex)
      
      if (xy.lines) 
        lines(xy, col = col, lty = lty, lwd = lwd, 
              type = if (do.lab) 
                'c'
              else 'l')
      
      return(invisible())
    }
    if (missing(ylab)) {
      ylab <- colnames(x)
      if (length(ylab) != 1L) ylab <- xlabel
    }
    
    if (is.matrix(x)) {
      k <- ncol(x)
      tx <- time(x)
      xy <- xy.coords(x = matrix(rep.int(tx, k), ncol = k), y = x, log = log, setLab = FALSE)
      xy$x <- tx
    }
    else xy <- xy.coords(x, NULL, log = log, setLab = FALSE)
    if (is.null(xlim)) xlim <- range(xy$x)
    if (is.null(ylim)) ylim <- range(xy$y[is.finite(xy$y)])
    plot.new()
    plot.window(xlim, ylim, log, ...)
    if (is.matrix(x)) {
      for (i in seq_len(k)) lines.default(xy$x, x[, i], 
                                          col = col[(i - 1L)%%length(col) + 1L], lty = lty[(i - 1L)%%length(lty) + 1L], 
                                          lwd = lwd[(i - 1L)%%length(lwd) + 1L], bg = bg[(i - 1L)%%length(bg) + 1L],
                                          pch = pch[(i - 1L)%%length(pch) + 1L], cex = cex[(i - 1L)%%length(cex) + 1L], type = type)
    }
    else {
      lines.default(xy$x, x, col = col[1L], bg = bg, lty = lty[1L], 
                    lwd = lwd[1L], pch = pch[1L], cex = cex[1L], 
                    type = type)
    }
    if (ann) 
      title(main = main, xlab = xlab, ylab = ylab, ...)
    if (axes) {
      axis(1, ...)
      axis(2, ...)
    }
    if (frame.plot) 
      box(...)
  }
  
  xlabel <- if (!missing(x)) deparse(substitute(x))
  ylabel <- if (!missing(y)) deparse(substitute(y))
  
  plotts(x = x, y = y, plot.type = plot.type, xy.labels = xy.labels, 
         xy.lines = xy.lines, panel = panel, nc = nc, xlabel = xlabel, 
         ylabel = ylabel, axes = axes, other=other, other_col=other_col, other_lwd=other_lwd, ...)
}

### Old acf
old_acf <- stats:::plot.acf

### New acf
plot.acf <- function (x, ci = 0.95, type = 'h', xlab = 'Lag', ylab = NULL, ylim = NULL, main = NULL, ci.col = 'blue', 
                      ci.type = c('white', 'ma'), max.mfrow = 6, ask = Npgs > 1 && dev.interactive(), 
                      mar = if (nser > 2) c(3, 2, 2, 0.8) 
                      else par('mar'), 
                      oma = if (nser > 2) c(1, 1.2, 1, 1) 
                      else par('oma'), 
                      mgp = if (nser > 2) c(1.5, 0.6, 0) 
                      else par('mgp'), xpd = par('xpd'), 
                      cex.main = if (nser > 2) 1 
                      else par('cex.main'),
                      verbose = getOption('verbose'), ...) {
  
  ci.type <- match.arg(ci.type)
  if ((nser <- ncol(x$lag)) < 1L) 
    stop('x$lag must have at least 1 column')
  if (is.null(ylab)) 
    ylab <- switch(x$type, correlation = 'ACF', covariance = 'ACF (cov)', 
                   partial = 'Partial ACF')
  if (is.null(snames <- x$snames)) 
    snames <- paste('Series ', if (nser == 1L) x$series
                    else 1L:nser)
  
  with.ci <- ci > 0 && x$type != 'covariance'
  with.ci.ma <- with.ci && ci.type == 'ma' && x$type == 'correlation'
  if (with.ci.ma && x$lag[1L, 1L, 1L] != 0L) {
    warning('can use ci.type=\'ma\' only if first lag is 0')
    with.ci.ma <- FALSE
  }
  
  clim0 <- if (with.ci) qnorm((1 + ci)/2)/sqrt(x$n.used)
  else c(0, 0)
  
  Npgs <- 1L
  nr <- nser
  
  if (nser > 1L) {
    sn.abbr <- if (nser > 2L) abbreviate(snames)
    else snames
    
    if (nser > max.mfrow) {
      Npgs <- ceiling(nser/max.mfrow)
      nr <- ceiling(nser/Npgs)
    }
    
    ### Not included below: mfrow = rep(nr, 2L)
    opar <- par(mar = mar, oma = oma, mgp = mgp, 
                ask = ask, xpd = xpd, cex.main = cex.main)
    on.exit(par(opar))
    
    if (verbose) {
      message('par(*) : ', appendLF = FALSE, domain = NA)
      str(par('mfrow', 'cex', 'cex.main', 'cex.axis', 'cex.lab', 
              'cex.sub'))
    }
  }
  
  if (is.null(ylim)) {
    ylim <- range(x$acf[, 1L:nser, 1L:nser], na.rm = TRUE)
    if (with.ci) ylim <- range(c(-clim0, clim0, ylim))
    if (with.ci.ma) {
      for (i in 1L:nser) {
        clim <- clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1, 
                                                   i, i]^2)))
        ylim <- range(c(-clim, clim, ylim))
      }
    }
  }
  
  for (I in 1L:Npgs) for (J in 1L:Npgs) {
    dev.hold()
    iind <- (I - 1) * nr + 1L:nr
    jind <- (J - 1) * nr + 1L:nr
    if (verbose) 
      message(gettextf('Page [%d,%d]: i =%s; j =%s', I, 
                       J, paste(iind, collapse = ','), paste(jind, collapse = ',')), 
              domain = NA)
    for (i in iind) for (j in jind) if (max(i, j) > nser) {
      frame()
      box(col = 'light gray')
    }
    else {
      clim <- if (with.ci.ma && i == j) 
        clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1, i, j]^2)))
      else clim0
      plot(x$lag[, i, j], x$acf[, i, j], type = type, xlab = xlab, 
           ylab = if (j == 1) 
             ylab
           else '', ylim = ylim, ...)
      abline(h = 0)
      if (with.ci && ci.type == 'white') 
        abline(h = c(clim, -clim), col = ci.col, lty = 2)
      else if (with.ci.ma && i == j) {
        clim <- clim[-length(clim)]
        lines(x$lag[-1, i, j], clim, col = ci.col, lty = 2)
        lines(x$lag[-1, i, j], -clim, col = ci.col, lty = 2)
      }
      title(if (!is.null(main)) 
        main
        else if (i == j) 
          snames[i]
        else paste(sn.abbr[i], '&', sn.abbr[j]), line = if (nser > 
                                                            2) 
          1
        else 2)
    }
    if (Npgs > 1) {
      mtext(paste('[', I, ',', J, ']'), side = 1, line = -0.2, 
            adj = 1, col = 'dark gray', cex = 1, outer = TRUE)
    }
    dev.flush()
  }
  invisible()
}