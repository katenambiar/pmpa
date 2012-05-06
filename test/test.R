library(devtools)
load_all(as.package("../peparray"))


files <- "C:/Users/Kate/git/peparray/test/fileNames.txt"
protocol <- "C:/Users/Kate/git/peparray/test/protocolAnnotation.txt"
pheno <- "C:/Users/Kate/git/peparray/test/sampleAnnotation.txt"

# Read GPR Files
R <- readArrays(files, col = "R")

# Add pheno annotation
R <- readAnnotation(R, pheno)


# Get CV of replicates
R.CV <- arrayCV(R, ndups = 3, spacing = 5184)

# IA norm
boxplot(log2(fg(R)))
controlSeq <- c("MYPIYNTPDNLWFGY", "SRRYVPNQLTKVRLQ", "GHYSWIAKAVLQGEG")

RNorm <- intraArrayNormLM(R, controlSeq, method = "pls")
boxplot(log2(fg(R))[,1] ~ peptideAnnotation(R)$Block)  
boxplot(RNorm[,1] ~ peptideAnnotation(R)$Block)
