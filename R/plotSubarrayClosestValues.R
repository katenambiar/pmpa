#' Scatter plot of closest two values from subarrays of peptide microarray data
#' 
#' 
#' @param x MultiSet object with fMedian matrix in the assayData slot
#' @param arr Index indicating which array should be plotted
#' @param transform function to apply to transform the raw data
#' @return plot on current graphics device
#'  
#' @export
#' @docType methods
#' @rdname plotSubarrayClosestValues-methods
setGeneric(
  name = "plotSubarrayClosestValues", 
  def = function(x, ...) standardGeneric("plotSubarrayClosestValues")
)

#' @rdname plotSubarrayClosestValues-methods
#' @aliases plotSubarrayClosestValues
setMethod(
  f = "plotSubarrayClosestValues",
  signature = "MultiSet",
  definition = function(x, arr, transform = "log2", ...){
    if (is.function(transform)){
      transformFunc <- transform
      
    } else if (transform == "none"){
      transformFunc <- function(y) identity(y)
      
    } else {
      transformExpression <- parse(text = paste(transform, "(y)", sep = ""))
      transformFunc <- function (y) eval(transformExpression)
    }
    
    arraydata <- transformFunc(fg(x[ ,arr]))
    minval <- min(arraydata)
    maxval <- max(arraydata)
    
    plotdata <- list(P1_2 = data.frame(SA.x = arraydata[fData(x)$Subarray == 1, ], SA.y  = arraydata[fData(x)$Subarray == 2, ]),
                     P1_3 = data.frame(SA.x = arraydata[fData(x)$Subarray == 1, ], SA.y  = arraydata[fData(x)$Subarray == 3, ]),
                     P2_3 = data.frame(SA.x = arraydata[fData(x)$Subarray == 2, ], SA.y  = arraydata[fData(x)$Subarray == 3, ])
    )
    
    plotdata.abs.diff <- cbind(abs(plotdata$P1_2[,1]-plotdata$P1_2[,2]), abs(plotdata$P1_3[,1]-plotdata$P1_2[,2]), abs(plotdata$P2_3[,1]-plotdata$P2_3[,2]))
    plotdata.closest <- apply(plotdata.abs.diff, 1, which.min)
    
    a <- list()
    for(i in 1: length(plotdata.closest)){
      a[[i]] <- plotdata[[plotdata.closest[i]]][i,]
    }
    
    plotdata.final <- do.call("rbind", a)
    
    plot(SA.y ~ SA.x, data = plotdata.final,
         las = 1,
         pch = 20,
         xlim = c(minval, maxval),
         ylim = c(minval, maxval),
         xlab = "",
         ylab = "",
         ...
    )
    
    lmfit <- lm(SA.y ~ SA.x, data = plotdata.final,)
    lmfit.1 <- lm(SA.y-SA.x ~ SA.x, data = plotdata.final,)
    p <- (1 - pt(coef(summary(lmfit))[2,1]/coef(summary(lmfit))[2,2], lmfit$df.residual))*2
    p1 <- (1 - pt(abs(coef(summary(lmfit))[2,1] - 1)/coef(summary(lmfit))[2,2], lmfit$df.residual))*2
    
    abline(lmfit, col = "blue")
    abline(0,1, col = "red")
    
    betaval <- round(coef(summary(lmfit))[2,1], 3)
    lci <- round(confint(lmfit)[2,1], 3)
    uci <- round(confint(lmfit)[2,2], 3)
    
    lgnd1 <- bquote(R^2== .(round(summary(lmfit)$adj.r.squared, 3)))
    lgnd2 <- bquote(beta== .(betaval) * (.(lci)-.(uci)))

    legend("topleft", 
           c(as.expression(lgnd1), 
             as.expression(lgnd2)),
           bty = "n",
           cex = 0.9
    )
}
)

