library(CASdatasets)

data(freMTPL2freq)
data(freMTPL2sev)
head(freMTPL2freq)

freMTPL2freq$ClaimNb <- as.integer(freMTPL2freq$ClaimNb)
summary(freMTPL2freq$ClaimNb)
