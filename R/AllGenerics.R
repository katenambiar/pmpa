#-----------------------------------------------------------------------------------------------------
#   Generic methods for peparray
#-----------------------------------------------------------------------------------------------------

if(!isGeneric("getI")) {
  setGeneric(
    name = "getI", 
    def = function(x) standardGeneric("getI")
    )
}

if(!isGeneric("getFG")) {
  setGeneric(
    name = "getFG", 
    def = function(x) standardGeneric("getFG")
    )
}

if(!isGeneric("getBG")) {
  setGeneric(
    name = "getBG", 
    def = function(x) standardGeneric("getBG")
    )
}

if(!isGeneric("getFlags")) {
  setGeneric(
    name = "getFlags", 
    def = function(x) standardGeneric("getFlags")
    )
}

if(!isGeneric("getPepAnnot")) {
  setGeneric(
    name = "getPepAnnot", 
    def = function(x) standardGeneric("getPepAnnot")
    )
}

if(!isGeneric("getSampAnnot")) {
  setGeneric(
    name = "getSampAnnot", 
    def = function(x) standardGeneric("getSampAnnot")
    )
}


if(!isGeneric("arrayCV")) {
  setGeneric(
    name = "arrayCV", 
    def = function(x, ...) standardGeneric("arrayCV")
    )
}
