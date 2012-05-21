library(devtools)
pkg <- as.package("../peparray")
load_all(pkg)
# reload(pkg)
# document(pkg)
# install_github("peparray", username = "katenambiar")
library(limma)

files <- "../peparray/test/mab_fileNames.txt"
# protocol <- "../peparray/test/protocolAnnotation.txt"
# pheno <- "../peparray/test/sampleAnnotation.txt"
feature <- "../peparray/test/peptideAnnotation.txt"

# Read GPR Files
R <- readArrays(files, col = "R")
boxplot(R, col = "orange")

# Add pheno annotation
R <- readAnnotation(R, feature = feature)

# Get CV of replicates
R.CV <- arrayCV(R, ndups = 3, spacing = 5184)

# BG correction
R.BG <- arrayBGcorr(R, method = "none")

# IntraArray Norm
controlSeq <- c("MYPIYNTPDNLWFGY", "SRRYVPNQLTKVRLQ", "GHYSWIAKAVLQGEG")
R.Norm <- intraArrayNorm(R.BG, controlSeq, method = "lm")
boxplot(R.Norm)

# Average intraArray Replicates
R.ave <- arrayAve(R.BG)
boxplot(R.ave, transform = "log2", col = "orange")

# TcdB plots
TcdB <- grep("TcdB", fData(R.ave)$Name)
tstRaw <- R.ave[TcdB, ]

tstRaw1 <- tstRaw[ ,1]
tstRaw1.df <- data.frame (exprs = assayDataElement(tstRaw1, "exprs"), pos = fData(tstRaw1)$pos630)
tstRaw1.df <- na.omit(tstRaw1.df)
tstRaw1.df <- tstRaw1.df[order(tstRaw1.df[,2]), ]
plot(exprs ~ pos, data = tstRaw1.df, type = "l")

tstRaw1 <- tstRaw[ ,5]
tstRaw1.df <- data.frame (exprs = assayDataElement(tstRaw1, "exprs"), pos = fData(tstRaw1)$pos630)
tstRaw1.df <- na.omit(tstRaw1.df)
tstRaw1.df <- tstRaw1.df[order(tstRaw1.df[,2]), ]
plot(exprs ~ pos, data = tstRaw1.df, type = "l")



# Secab filtering
R.secab <- arraySecAb(R.ave, secabID = "NCTRL", transform = "log2")

norm <- log2(assayDataElement(R.ave, "exprs"))
norm.mean <- apply(norm, 2, mean, na.rm = TRUE)
norm.sd <- apply(norm, 2, sd, na.rm = TRUE)

norm.meancentre <- sweep(norm, 2, norm.mean, FUN = '-')
norm.meancentre.unitvar <- sweep(norm.meancentre, 2, norm.sd, FUN = '/')

R.secabNorm <- R.secab
assayDataElement(R.secabNorm, "exprs") <- norm.meancentre.unitvar

TcdB <- grep("TcdB", fData(R.secabNorm)$Name)
tst <- R.secabNorm[TcdB, ]


tst1 <- tst[ ,1]
tst1.df <- data.frame (exprs = assayDataElement(tst1, "exprs"), pos = fData(tst1)$pos630)
tst1.df <- na.omit(tst1.df)
tst1.df <- tst1.df[order(tst1.df[,2]), ]

tst2 <- tst[ ,2]
tst2.df <- data.frame (exprs = assayDataElement(tst2, "exprs"), pos = fData(tst2)$pos630)
tst2.df <- na.omit(tst2.df)
tst2.df <- tst2.df[order(tst2.df[,2]), ]

tst3 <- tst[ ,3]
tst3.df <- data.frame (exprs = assayDataElement(tst3, "exprs"), pos = fData(tst3)$pos630)
tst3.df <- na.omit(tst3.df)
tst3.df <- tst3.df[order(tst3.df[,2]), ]

tst4 <- tst[ ,4]
tst4.df <- data.frame (exprs = assayDataElement(tst4, "exprs"), pos = fData(tst4)$pos630)
tst4.df <- na.omit(tst4.df)
tst4.df <- tst4.df[order(tst4.df[,2]), ]


layout(matrix(1:4, 4, 1))
plot(2^exprs ~ pos, data = tst1.df, type = "l", ylim = c(0, 36))
plot(2^exprs ~ pos, data = tst2.df, type = "l", ylim = c(0, 36))
plot(2^exprs ~ pos, data = tst3.df, type = "l", ylim = c(0, 36))
plot(2^exprs ~ pos, data = tst4.df, type = "l", ylim = c(0, 36))

# Secab Plots
plotdata <- assayDataElement(R.ave, "exprs")
plotdata <- log2(plotdata)
Cdiff <- grep("TcdA|TcdB|FbpA|FliC|FliD|SlpA|CdtA|CdtB|Cwp66|Cwp84", fData(R.ave)$Name)
plotdata.cdiff <- plotdata[Cdiff, ]
plot(plotdata.cdiff[,5], plotdata.cdiff[,2])

TcdB <- grep("TcdB", fData(R.ave)$Name)
plot.tcdB <- plotdata[TcdB,]
points(plot.tcdB[,5], plot.tcdB[,2], col = "red", pch = 20)

TcdA <- grep("TcdA", fData(R.ave)$Name)
plot.tcdA <- plotdata[TcdA,]
points(plot.tcdA[,3], plot.tcdA[,2], col = "blue", pch = 20)

FbpA <- grep("FbpA", fData(R.ave)$Name)
plot.FbpA <- plotdata[FbpA,]
points(plot.FbpA[,3], plot.FbpA[,2], col = "green", pch = 20)

FliC <- grep("FliC", fData(R.ave)$Name)
plot.FliC <- plotdata[FliC,]
points(plot.FliC[,3], plot.FliC[,2], col = "purple", pch = 20)

FliD <- grep("FliD", fData(R.ave)$Name)
plot.FliD <- plotdata[FliD,]
points(plot.FliD[,3], plot.FliD[,2], col = "pink", pch = 20)

SlpA <- grep("SlpA", fData(R.ave)$Name)
plot.SlpA <- plotdata[SlpA,]
points(plot.SlpA[,3], plot.SlpA[,2], col = "orange", pch = 20)

CdtA <- grep("CdtA", fData(R.ave)$Name)
plot.CdtA <- plotdata[CdtA,]
points(plot.CdtA[,3], plot.CdtA[,2], col = "light blue", pch = 20)

CdtB <- grep("CdtB", fData(R.ave)$Name)
plot.CdtB <- plotdata[CdtB,]
points(plot.CdtB[,3], plot.CdtB[,2], col = "dark red", pch = 20)

Cwp66 <- grep("Cwp66", fData(R.ave)$Name)
plot.Cwp66 <- plotdata[Cwp66,]
points(plot.Cwp66[,3], plot.Cwp66[,2], col = "dark green", pch = 20)

Cwp84 <- grep("Cwp84", fData(R.ave)$Name)
plot.Cwp84 <- plotdata[Cwp66,]
points(plot.Cwp84[,3], plot.Cwp84[,2], col = "yellow", pch = 20)
