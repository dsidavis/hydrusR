library(hydrusR)

o = genSelector(P0 = -5, P2H = -999, P2L = -998, P3 = -8999, r2H = 1, r2L = .5, template = "SELECTOR.IN")
o$G
cat(o, file = "SELECTOR1.IN", sep = "\n")

o = genSelector(TPrint = seq(1000, by = 500, length = 15), template = "SELECTOR.IN")
o$C


orig = genSelector()
nmat = as.integer(orig[["NMat"]])
b = matrix(rnorm(9 *nmat), , 9)
colnames(b) = c("thr", "ths", "Alfa", "n", "Ks", "l",  "thrIm", "thsIm", "Omega")
b = as.data.frame(b)

o = genSelector(b17 = b, TPrint = seq(1000, by = 500, length = 9), template = "SELECTOR.IN")
o$B
o[["NMat"]]


nmat = 20
x = as.data.frame( matrix(rnorm(nmat * 4), , 4, dimnames = list(NULL, c("Bulk.d.", "DisperL.", "Frac", "Mobile WC"))))
o = genSelector(chpar = x)


# Change the number of materials from 20 to 9
nmat = 11
b = matrix(rnorm(9 *nmat), , 9)
colnames(b) = c("thr", "ths", "Alfa", "n", "Ks", "l",  "thrIm", "thsIm", "Omega")
b = as.data.frame(b)

o = genSelector(b17 = b)
o[["NMat"]]
o$B

 # 2 solutes. So 2 x NMat values.
fks = 1:22
snk = 1:22 + 100

o1 = genSelector(b17 = b, F.Ks = fks, F.SnkL1p = snk, txt = o)
o1$F
# Note that this has left the 9 rows from the original NMat = 20 at the end of each block.

# So we should be setting the entire data frame.



nmat = 13
vars = c("Ks", "Nu", "Beta", "Henry", "SnkL1", "SnkS1", "SnkG1", "SnkL1'", "SnkS1'", "SnkG1'", "SnkL0", "SnkS0", "SnkG0", "Alfa"  )
nsol = 2
F11 = as.data.frame(matrix(rnorm(length(vars)*nmat*nsol), nmat*nsol, length(vars), dimnames = list(NULL, vars)))

# We'll pretend we have updated NMat in the call.
o = genSelector(NMat = nmat)
o1 = genSelector(F11 = F11, txt = o)
o1$F
