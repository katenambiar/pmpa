#' Peptide Microarray Quality Assessment Plots
#' 
#' @param x MultiSet object
#' @param arr integer value indicate array to plot - corresponds to
#' column of MultiSet data object
#' @param transform Expression to transform data. Defaults to log2
#' @return plot on current graphics device
#' @export
arrayQAplot <- function(x, arr, titletext, transform = "log2"){
  par(oma = c(0, 0, 3, 0))
  layout(matrix(c(1:6,2,3,7,8),2,5, byrow = T), 
         widths = c(1.4, 0.8, 0.8, 1)
         )
  plotSubarrayBlocks(x, arr, 
                     outcex = 0.6, 
                     transform = transform
                     )
  plotImage(x, arr, 
            slot = "fg", 
            lowcol = "white", 
            highcol = "black", 
            titletext = "FG Image", 
            transform = transform
            )
  plotImage(x, arr, 
            slot = "bg", 
            lowcol = "white", 
            highcol = "black", 
            titletext = "BG Image"
            )
  plotSubarrayScatter(x, arr, 
                      c(1,2), 
                      cex = 0.6, 
                      transform = transform
                      )
  plotSubarrayScatter(x, arr, 
                      c(2,3), 
                      cex = 0.6, 
                      transform = transform
                      )
  plotSubarrayDensity(x, arr, 
                      transform = transform
                      )
  plotSubarrayScatter(x, arr, 
                      c(1,3), 
                      cex = 0.6, 
                      transform = transform
                      )
  plotSubarrayClosestValues(x, arr, 
                            cex = 0.6, 
                            transform = transform
                            )
  mtext(titletext, outer = TRUE)
}
