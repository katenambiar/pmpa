# pepArrayPP Class
setClass ("pepArrayPP",
          contains = "eSet"
          )

# pepArrayPP Constructor Method
setMethod ("initialize", "pepArrayPP",
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


#--------------------------------------------------------------------------------------------


# pepArray Class
setClass ("pepArray",
          contains = "eSet"
          )


# pepArray Constructor Method
setMethod ("initialize", "pepArray",
           function(.Object,
                    assayData = assayDataNew(exprs = exprs),
                    exprs = new("matrix"),
                    exprs.se = new("matrix"),
                    ...
                    ) {
             
             if(!missing(assayData) && any(!missing(exprs), !missing(exprs.se))){
               warning("Using 'assayData'; ignoring 'exprs', 'exprs.se'")
             }
             callNextMethod(.Object, 
                            assayData = assayData,
                            ...)
           })
