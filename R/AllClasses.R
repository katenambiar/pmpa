#-----------------------------------------------------------------------------------------------------
#   pepArray class definitions
#-----------------------------------------------------------------------------------------------------

# pepArrayRaw Class Definition
setClass ("pepArrayRaw", 
          representation (FG = "matrix", BG = "matrix", flags = "matrix", peptideAnnotation = "data.frame", sampleAnnotation = "data.frame")
          )

