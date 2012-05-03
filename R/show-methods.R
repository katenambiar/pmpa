#-----------------------------------------------------------------------------------------------------
#   Show Methods
#-----------------------------------------------------------------------------------------------------

setMethod("show", "pepArrayRaw", 
          function(object) {
            cat("Object of class ", class(object), "\n", sep = "")
            cat(ncol(getFG(object)), " Samples \n", sep = "")
            cat(nrow(getFG(object)), " Probes \n", sep = "")
          })