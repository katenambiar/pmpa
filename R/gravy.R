gravy <- function(feature.names){
  hydropathy.index <- data.frame(AA = c("A","R","N","D","C","E","Q","G","H","I","L","K","M","F","P","S","T","W","Y","V"),
                                 hydropathy = c(1.8, -4.5, -3.5, -3.5, 2.5, -3.5, -3.5, -0.4, -3.2, 4.5, 3.8, -3.9, 1.9, 2.8, -1.6, -0.8, 0.7, -0.9, -1.3, 4.2),
                                 stringsAsFactors = FALSE) 

  aa.codes <- paste("(", paste(hydropathy.index$AA, collapse = "|"), "){15}", sep = "")
  feature.names[grep(aa.codes, feature.names, invert = TRUE)] <- NA
  feature.aa <- sapply(feature.names, function(x) strsplit(x, "")[[1]])
  aa.hydropathy <- sapply(feature.aa, function(x) sapply(x, function(y) hydropathy.index[y == hydropathy.index$AA, 2]))
  gravy.value <- sapply(aa.hydropathy, mean)
  
  return(gravy.value)
}
