setMethod(
  f = "prData",
  signature = "pepArrayPP",
  definition = function(x){
    protocolData(x)@data
  }
  )