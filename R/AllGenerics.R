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
if(!isGeneric("cv.exprs")) {
  setGeneric(
    name = "cv.exprs", 
    def = function(x) standardGeneric("cv.exprs")
    )
}


if(!isGeneric("readAnnotation")) {
  setGeneric(
    name = "readAnnotation", 
    def = function(x, ...) standardGeneric("readAnnotation")
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

# Calculate CV of intra-array replicates
if(!isGeneric("arrayCV")) {
  setGeneric(
    name = "arrayCV", 
    def = function(x, ...) standardGeneric("arrayCV")
    )
  }

# Calculate the average of intra-array replicates
if(!isGeneric("arrayAve")) {
  setGeneric(
    name = "arrayAve", 
    def = function(x, ...) standardGeneric("arrayAve")
    )
  }

if(!isGeneric("arraySecAb")) {
  setGeneric(
    name = "arraySecAb", 
    def = function(x, ...) standardGeneric("arraySecAb")
    )
}

# Extension to Boxplot S3 methods
if(!isGeneric("boxplot")) {
  setGeneric("boxplot")
}