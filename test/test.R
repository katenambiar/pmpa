library(devtools)
pkg <- as.package("../peparray")
load_all(pkg)
# reload(pkg)
# document(pkg)
# install_github("peparray", username = "katenambiar")


files <- "C:/Users/Kate/git/peparray/test/fileNames.txt"
protocol <- "C:/Users/Kate/git/peparray/test/protocolAnnotation.txt"
pheno <- "C:/Users/Kate/git/peparray/test/sampleAnnotation.txt"

# Read GPR Files
R <- readArrays(files, col = "R")

# Add pheno annotation
R <- readAnnotation(R, pheno = pheno, protocol = protocol)

# Get CV of replicates
R.CV <- arrayCV(R, ndups = 3, spacing = 5184)

# BG correction
R.BG <- arrayBGcorr(R, method = "none")

# IntraArray Norm
controlSeq <- c("MYPIYNTPDNLWFGY", "SRRYVPNQLTKVRLQ", "GHYSWIAKAVLQGEG")
R.Norm <- intraArrayNorm(R.BG, controlSeq, method = "lm")

# Average intraArray Replicates
R.ave <- arrayAve(R.Norm)


