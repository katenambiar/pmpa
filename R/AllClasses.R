library(Biobase) #For testing only - Remove in final package
library(plyr)

# pepArray Class Definition
setClass ("pepArray",
          contains = "eSet"
          )

# pepArray Constructor Method
setMethod ("initialize", "pepArray",
           function(.Object,
                    assayData = assayDataNew(fg = fg, bg = bg, flags = flags),
                    fg = new("matrix"),
                    bg = new("matrix"),
                    flags = new("matrix"),
                    phenoData = new("AnnotatedDataFrame"),
                    featureData = new("AnnotatedDataFrame"),
                    experimentData = new("MIAME"),
                    annotation = character(),
                    protocolData = new("AnnotatedDataFrame"),
                    ...
                    ) {
             
             if(!missing(assayData) && any(!missing(fg), !missing(bg), !missing(flags))){
               warning("Using 'assayData'; ignoring 'fg', 'bg', 'flags'")
             }
             callNextMethod(.Object, 
                            assayData = assayData,
                            phenoData = phenoData,
                            featureData = featureData,
                            experimentData = experimentData,
                            annotation = annotation,
                            protocolData = protocolData,
                            ...)
           })
