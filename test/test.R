library(devtools)
pkg <- as.package("../peparray")
load_all(pkg)
# reload(pkg)
# document(pkg)

files <- "./test/fileNames.txt"
protocol <- "./test/protocolAnnotation.txt"
pheno <- "./test/sampleAnnotation.txt"

# Read GPR Files
R <- readArrays(files, col = "R")

# Add pheno annotation
R <- readAnnotation(R, pheno = pheno, protocol = protocol)

# Get CV of replicates
R.CV <- arrayCV(R, ndups = 3, spacing = 5184)

# BG correction
R.BG <- arrayBGcorr(R, method = "none")



# Average intraArray Replicates
R.ave <- arrayAve(R.BG)

# IA norm
boxplot(log2(fg(R)))
controlSeq <- c("MYPIYNTPDNLWFGY", "SRRYVPNQLTKVRLQ", "GHYSWIAKAVLQGEG")

RNorm <- intraArrayNormLM(R, controlSeq, method = "pls")
boxplot(log2(fg(R))[,1] ~ peptideAnnotation(R)$Block)  
boxplot(RNorm[,1] ~ peptideAnnotation(R)$Block)
