# Import target matrix
targets  <- read.table ("http://scamcompute.hpc.susx.ac.uk/datatrans/kate/LVP/GPR/targets.txt",
                        stringsAsFactors = FALSE,
                        header = TRUE,
                        check.names = FALSE
                        )

targets <- targets[2,]
targets <- t(targets)
targets <- targets[1:5]

# Define path for GPR directory                            
gpr.path <- "http://scamcompute.hpc.susx.ac.uk/datatrans/kate/LVP/GPR"

# Define GAL filename
gal.path <- "http://scamcompute.hpc.susx.ac.uk/datatrans/kate/CDIFF/GAL/JPT_cDiff.gal"

# Read GPR Files
R <- readGPR(targets, gpr.path, gal.path, col = "R")

# Get CV of replicates
R.CV <- arrayCV(R, ndups = 3, spacing = 5184)