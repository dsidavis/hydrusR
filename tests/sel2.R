library(hydrusR)

nmat = 11
b = matrix(rnorm(9 *nmat), , 9)
colnames(b) = c("thr", "ths", "Alfa", "n", "Ks", "l",  "thrIm", "thsIm", "Omega")
b = as.data.frame(b)


chpar = as.data.frame( matrix(rnorm(nmat * 4), , 4, dimnames = list(NULL, c("Bulk.d.", "DisperL.", "Frac", "Mobile WC"))))

vars = c("Ks", "Nu", "Beta", "Henry", "SnkL1", "SnkS1", "SnkG1", "SnkL1'", "SnkS1'", "SnkG1'", "SnkL0", "SnkS0", "SnkG0", "Alfa"  )
nsol = 2
F11 = as.data.frame(matrix(rnorm(length(vars)*nmat*nsol), nmat*nsol, length(vars), dimnames = list(NULL, vars)))

tprint = seq(500, by = 250, length = 9)

#
o = genSelector(b17 = b, TPrint = tprint, chpar = chpar, F11 = F11, DifW = c(1, 2), DifG = c(.5, 1.5), P0 = -5, P2H = -999, P2L = -998, P3 = -8999, r2H = 1, r2L = .5)
o[["NMat"]]
o[["r2L"]]

o$B
o$C
o$F

fks = 1:22
snk = 1:22 + 100
o1 = genSelector(F.Ks = fks, F.SnkL1p = snk, txt = o)

