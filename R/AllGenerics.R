#-----------------------------------------------------------------------------------------------------
#   Generic methods for peparray
#-----------------------------------------------------------------------------------------------------

# Foreground Intensity Accessor
if(!isGeneric("fg")) {
  setGeneric(
    name = "fg", 
    def = function(x) standardGeneric("fg")
    )
}

# Background Intensity Accessor
if(!isGeneric("bg")) {
  setGeneric(
    name = "bg", 
    def = function(x) standardGeneric("bg")
    )
}

# ExpressionSet CV Accessor
if(!isGeneric("cv")) {
  setGeneric(
    name = "cv", 
    def = function(x) standardGeneric("cv")
    )
}


if(!isGeneric("annotateArrays")) {
  setGeneric(
    name = "annotateArrays", 
    def = function(x, ...) standardGeneric("annotateArrays")
    )
}


if(!isGeneric("arrayBGcorr")) {
  setGeneric(
    name = "arrayBGcorr", 
    def = function(x, ...) standardGeneric("arrayBGcorr")
    )
}


if(!isGeneric("intraArrayNorm")) {
  setGeneric(
    name = "intraArrayNorm", 
    def = function(x, ...) standardGeneric("intraArrayNorm")
    )
}

if(!isGeneric("normaliseArrays")) {
  setGeneric(
    name = "normaliseArrays", 
    def = function(x, ...) standardGeneric("normaliseArrays")
    )
}

# Calculate CV of intra-array replicates
if(!isGeneric("arrayCV")) {
  setGeneric(
    name = "arrayCV", 
    def = function(x, ...) standardGeneric("arrayCV")
    )
  }

# Summarise intra-array replicates
if(!isGeneric("arraySummary")) {
  setGeneric(
    name = "arraySummary", 
    def = function(x, ...) standardGeneric("arraySummary")
    )
  }

if(!isGeneric("arraySecAb")) {
  setGeneric(
    name = "arraySecAb", 
    def = function(x, ...) standardGeneric("arraySecAb")
    )
}
