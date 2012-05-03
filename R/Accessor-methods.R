#-----------------------------------------------------------------------------------------------------
#   Accessor Methods
#-----------------------------------------------------------------------------------------------------

setMethod("getFG", "pepArrayRaw", function(x) slot(x, "FG"))
setMethod("getBG", "pepArrayRaw", function(x) slot(x, "BG"))
setMethod("getFlags", "pepArrayRaw", function(x) slot(x, "flags"))
setMethod("getPepAnnot", "pepArrayRaw", function(x) slot(x, "peptideAnnotation"))
setMethod("getSampAnnot", "pepArrayRaw", function(x) slot(x, "sampleAnnotation"))


setMethod("getI", "pepArrayNorm", function(x) slot(x, "I"))
setMethod("getFlags", "pepArrayNorm", function(x) slot(x, "flags"))
setMethod("getPepAnnot", "pepArrayNorm", function(x) slot(x, "peptideAnnotation"))
setMethod("getSampAnnot", "pepArrayNorm", function(x) slot(x, "sampleAnnotation"))


setMethod("getI", "pepArrayCorr", function(x) slot(x, "I"))
setMethod("getFlags", "pepArrayCorr", function(x) slot(x, "flags"))
setMethod("getPepAnnot", "pepArrayCorr", function(x) slot(x, "peptideAnnotation"))
setMethod("getSampAnnot", "pepArrayCorr", function(x) slot(x, "sampleAnnotation"))
