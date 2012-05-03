#-----------------------------------------------------------------------------------------------------
#   pepArray class definitions
#-----------------------------------------------------------------------------------------------------

# pepArrayRaw Class Definition
setClass ("pepArrayRaw", 
          representation (FG = "matrix", BG = "matrix", flags = "matrix", peptideAnnotation = "data.frame", sampleAnnotation = "data.frame")
          )

# pepArrayCorr Class Definition
setClass ("pepArrayCorr", 
          representation (I = "matrix", flags = "matrix", peptideAnnotation = "data.frame", sampleAnnotation = "data.frame")
          )

# pepArrayNorm Class Definition
setClass ("pepArrayNorm", 
          representation (I = "matrix", flags = "matrix", peptideAnnotation = "data.frame", sampleAnnotation = "data.frame")
          )

# pepArraySummary Class Definition
setClass ("pepArraySummary", 
          representation (I = "matrix", flags = "matrix", peptideAnnotation = "data.frame", sampleAnnotation = "data.frame")
          )

