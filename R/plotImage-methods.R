setMethod(
  f = "plotImage",
  signature = "MultiSet",
  definition = function(x, slot = "bg", lowcol, highcol, ncols = 123, ...){
    
    layout <- getArrayLayout(x)
    
    if (slot == "fg"){
      x <- fg(x)
    } else if (slot =="bg"){
       x <- bg(x)
    } else {
      stop("Only 'fg' or 'bg'  slots can be plotted as a microarray image.")
    }
    
    blockrows <- max(layout[,'block.row'])
    blockcols <- max(layout[,'block.col'])
    spotrows <- max(layout[,'spot.row'])
    spotcols <- max(layout[,'spot.col'])
    features <- blockrows * blockcols * spotrows *spotcols
    
    low <- col2rgb(lowcol)/255
    high <- col2rgb(highcol)/255
    col <- rgb(seq(low[1], high[1], len = ncolors),
               seq(low[2], high[2], len = ncolors),
               seq(low[3], high[3], len = ncolors)
    )
    
    nr <- spotrows * blockrows
    nc <- spotcols * blockcols
    row.ind <- (layout[,'block.row'] - 1) * spotrows + layout[,'spot.row']
    col.ind <- (layout[,'block.col'] - 1) * spotcols + layout[,'spot.col']
    ord <- order(row.ind, col.ind)
    
    z <- matrix(x[ord], nrow = nr, ncol = nc, byrow = TRUE)
    z <- t(z)[, nr:1]
    
    image(1:nc, 1:nr, z, axes = F, xlab = "", ylab = "", col = col)
    box(lwd = 1)
    abline(v = ((1:blockcols - 1) * spotcols + 0.5), lwd = 1)
    abline(h = ((1:blockrows - 1) * spotrows + 0.5), lwd = 1)
  }
)