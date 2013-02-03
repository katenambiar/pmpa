#' Image plot of microarray data
#' 
#' Creates image plot representing the microarray foreground or background signal intensity
#' organised by the spatial location of the spots on the array.
#' 
#' @param x MultiSet object with fMedian and/or bMedian matrices in the assayData slot
#' and layout data (block, column and row) in the fData slot.
#' @param lowcol Colour associated with low signal intensities
#' @param highcol Colour associated with high signal intensities
#' @param ncols number of colour shades used
#' @return MultiSet object with transformed and background corrected foreground signal in the fMedian matrix
#'  
#' @export
#' @docType methods
#' @rdname plotImage-methods
setGeneric(
  name = "plotImage", 
  def = function(x, ...) standardGeneric("plotImage")
)

#' @rdname plotImage-methods
#' @aliases plotImage
setMethod(
  f = "plotImage",
  signature = "MultiSet",
  definition = function(x, slot = "bg", lowcol, highcol, ncols = 123, ...){
    layout <- getArrayLayout(x)
    
    if (ncol(x) > 1){
      warning("Multiple arrays found in MultiSet object. Only the first array will be displayed")
    }
    
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
    col <- rgb(seq(low[1], high[1], len = ncols),
               seq(low[2], high[2], len = ncols),
               seq(low[3], high[3], len = ncols)
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