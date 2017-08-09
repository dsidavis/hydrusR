library(hyrdusR)

o = genSelector(P0 = -5, P2H = -999, P2L = -998, P3 = -8999, r2H = 1, r2L = .5, template = "SELECTOR.IN")
o$G
cat(o, file = "SELECTOR1.IN", sep = "\n")

o = genSelector(TPrint = seq(1000, by = 500, length = 15), template = "SELECTOR.IN")
o$C


nmat = 11
b = matrix(rnorm(9 *nmat), , 9)
colnames(b) = c("thr", "ths", "Alfa", "n", "Ks", "l",  "thrIm", "thsIm", "Omega")
b = as.data.frame(b)

 # 2 solutes. So 2 x NMat values.
fks = 1:22

o = genSelector(b17 = b, TPrint = seq(1000, by = 500, length = 9))



o = genSelector(b17 = b, TPrint = seq(1000, by = 500, length = 9))



o = genSelector(F11)

