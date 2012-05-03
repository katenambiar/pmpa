source('C:/Users/Kate/git/peparray/R/pepArray.R')
source('C:/Users/Kate/git/peparray/R/pepArrayRaw.R')
source('C:/Users/Kate/git/peparray/R/pepArrayNorm.R')

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

# IA norm
boxplot(log2(fg(R)))
controlSeq <- c("MYPIYNTPDNLWFGY", "SRRYVPNQLTKVRLQ", "GHYSWIAKAVLQGEG")

RNorm <- intraArrayNormLM(R, controlSeq, method = "pls")
boxplot(log2(fg(R))[,1] ~ peptideAnnotation(R)$Block)  
boxplot(RNorm[,1] ~ peptideAnnotation(R)$Block)
