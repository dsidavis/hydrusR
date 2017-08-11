library(hydrusR)

o = genSelector()

# Should warn

o = genSelector(Model = 8)

# Should list multiple
o = genSelector(DrainF = TRUE, Model = 8)
