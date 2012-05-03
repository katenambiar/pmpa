#-----------------------------------------------------------------------------------------------------
#   Definitions for pepArraySummary class
#-----------------------------------------------------------------------------------------------------

# pepArraySummary Class Definition
setClass ("pepArraySummary", 
          representation (I = "matrix", flags = "matrix", peptideAnnotation = "data.frame", sampleAnnotation = "data.frame")
          )