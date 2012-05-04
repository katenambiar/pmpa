#-----------------------------------------------------------------------------------------------------
#   Accessor Methods
#-----------------------------------------------------------------------------------------------------

setMethod("getFG", "pepArray", function(x) slot(x, "FG"))
setMethod("getBG", "pepArray", function(x) slot(x, "BG"))
setMethod("getFlags", "pepArray", function(x) slot(x, "flags"))
setMethod("getPepAnnot", "pepArray", function(x) slot(x, "peptideAnnotation"))
setMethod("getSampAnnot", "pepArray", function(x) slot(x, "sampleAnnotation"))
