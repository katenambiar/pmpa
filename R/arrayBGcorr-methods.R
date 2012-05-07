
setMethod(
  f = "arrayBGcorr",
  signature = "pepArrayPP",
  definition = function(x, method = "none"){
    if (method == "none"){
      y <- x
      return (y)
    } else {
      stop("Method must be either 'none', 'subtract' or 'SNR'")
    }
  }
  )