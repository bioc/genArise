imageLimma <-  function(z, row, column, meta.row, meta.column, low = NULL, high = NULL){
  if (is.null(row) || is.null(column) || is.null(meta.row) || is.null(meta.column))
    stop("Layout needs to contain components ngrid.r, ngrid.c, nspot.r and spot.c")
  if (length(z) != row * column * meta.row * meta.column)
    stop("Number of image spots does not agree with layout dimensions")
  if (is.character(low))
    low <- col2rgb(low)/255
  if (is.character(high))
    high <- col2rgb(high)/255
  if (!is.null(low) && is.null(high))
    high <- c(1, 1, 1) - low
  if (is.null(low) && !is.null(high))
    low <- c(1, 1, 1) - high
  zr0 <- range(z, na.rm = TRUE)
  zr <- range(z, na.rm = TRUE, finite = TRUE)
  zmax <- max(abs(zr))
  zmin <- zr[1]
  zerocenter <- (zmin < 0)
  if (zerocenter) {
    if (is.null(low))
      low <- c(0, 1, 0)
    if (is.null(high))
      high <- c(1, 0, 0)
    zlim <- c(-zmax, zmax)
  }
  else {
    if (is.null(low))
      low <- c(1, 1, 1)
    if (is.null(high))
      high <- c(0, 0, 1)
      zlim <- c(zmin, zmax)
  }
  col <- rgb(seq(high[1], low[1], len = 123), seq(high[2], low[2], len = 123), seq(high[3], low[3], len = 123))
  dim(z) <- c(column, row, meta.column, meta.row)
  z <- aperm(z, perm = c(1, 3, 2, 4))
  dim(z) <- c(meta.column * column, meta.row * row)
  old.par <- par(mar = c(1, 1, 1, 1))
  on.exit(par(old.par))
  
    image(x = 0:(meta.column * column), y = 0:(meta.row * row), z = z, zlim = zlim, ylim = c(row * meta.row, 0), col = col, add = FALSE, xaxs = "i", yaxs = "i", xlab = "", ylab = "",axes = FALSE)
  
  for (igrid in 0:meta.row) lines(c(0, meta.column * column), rep(igrid * row,2))
  for (igrid in 0:meta.column) lines(rep(igrid * column, 2), c(0, meta.row * row))

}
