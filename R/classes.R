# pepArrayRaw Super Class Definition
setClass ("pepArrayRaw", 
          representation (flags = "matrix", peptideAnnotation = "data.frame", sampleAnnotation = "data.frame")
          )

# pepArrayRawRG Class Definition
setClass ("pepArrayRawRG", 
          representation (Rfg = "matrix", Rbg = "matrix", Gfg = "matrix", Gbg = "matrix"),
          contains = "pepArrayRaw"
          )

# pepArrayRawR Class Definition
setClass ("pepArrayRawR", 
          representation (Rfg = "matrix", Rbg = "matrix"),
          contains = "pepArrayRaw"
          )

# pepArrayRawG Class Definition
setClass ("pepArrayRawG", 
          representation (Gfg = "matrix", Gbg = "matrix"),
          contains = "pepArrayRaw"
          )