# Block B:
#  Number of rows in the dataframe for block B should equal NMat in Block A,
#
#  What should be in the data frame depends on the value of Model.
#
# If Hysteresis is > 0, need to add comment line and then a line for an integer value for iKappa  after Hystertesis
#
#
#

genSelector =
#
#'@title  Modify a SELECTOR.IN template file with data for some of the records in the different blocks    
#'@param  b17  A data frame with NMat rows, for Block B record 17.
#'@param  TPrint a vector of times to print the results. For Block C, record 9
#'@param  chpar  Data frame giving solute transport parameters, 1 row per solute Block F, record 7
#'@param  F.Ks a vector giving the Ks columns for Block F, record 11.
#'@param  F.SnkL1p  a vector giving the SnkL1' columns for Block F, record 11
#   Both F.Ks and F.SnkL1p should have No.Solutes * NMat values.
#'@param  F11  a data frame providing all of the columns for each solute. Block F, record 11
#'    F11 should have NMat * No.Solutes rows.
#@param  DifW, DifG - vectors which will be replicated to have length No.Solutes
    #
    #
#'@description This does not check the conditional logic based on the
    #   values of parameters in the file such as Hysterisis, iModel, etc. which
    #   change the structure of what is expected in the inputs.
    #   This only allows the caller to specify a few records in the selector file.
    #   However, the tools \code{replaceValues}, \code{replaceColumn}, \code{replaceData}
    #   allow one to write code reasonably easily to handle other records.
    
#@return  a character vector containing the updated contents of the input file.
    #    You need to write this to a file.  This differs from the original functions
    # in hydrusR.
    # Note that if you are using RHydrus, one can name the files differently from SELECTOR.IN, etc.
    # and specify the input files to use directly. This means that the names of the input files
    # can indicate their purpose/characteristics.
#
#@export
#
#
function(b17 = NULL,
         TPrint = NULL,             
         chpar = NULL,
         F.Ks = NULL,
         F.SnkL1p = NULL,
         F11 = NULL,
         DifW = 0, DifG = 0,
         ...,
         template = system.file("templates", "SELECTOR.IN", package = "hydrusR"),
         txt = readLines(template))
{
    origNMat = getValue(txt, "NMat", "integer")

    txt = replaceValues(txt, .values = list(...))
    
    blocks = getBlocks(txt)    
    
    if(!is.null(b17)) {
          # See p225 of HYDRUS1D manual
        NMat = nrow(b17)
        blocks$A = replaceValues(blocks$A, "NMat" = NMat)
        blocks$B = replaceData(blocks$B, "thr", origNMat, b17)
    }

    if(!is.null(TPrint)) 
        blocks$C = replaceValues(blocks$C, "MPL" = length(TPrint), TPrint = TPrint) #  matrix(TPrint,,6, byrow = TRUE))

    if(!is.null(chpar)) {
        # nchpar = getValue(blocks$A, "NMat", "integer")
        # blocks$F = replaceValues(blocks$F, "nChPar" = nrow(chpar))
        blocks$F = replaceData(blocks$F, "Bulk.d.", origNMat, chpar)
    }

    if(!is.null(F.Ks)) 
        blocks$F = replaceColumn(blocks$F, "Ks", origNMat, F.Ks)  # as.integer(getValue(blocks$A, "NMat"))

    if(!is.null(F.SnkL1p)) 
        blocks$F = replaceColumn(blocks$F, "SnkL1'", origNMat, F.SnkL1p) # as.integer(getValue(blocks$A, "NMat"))

    if(!is.null(F11)) {
        #
        # This adds the Block F records 9 and 11 for each solute
        # i.e. the DifW and DifG lines and then the columns of parameters for each solute
        #    
        #
        # Assumes a data.frame
        F = blocks$F
        s = grep("DifW", F)[1]
        e = grep("kTopSolute", F)
        nmat = as.integer(getValue(blocks$A, "NMat"))
        nsol = nrow(F11)/nmat
        if(floor(nsol) != nsol)
            warning("numer of rows in F11 not a multiple of NMat")

        F = replaceValues(F, "No.Solutes" = nsol)
        
        solID = rep(1:nsol, each = nmat)        
        DifW = rep(DifW, length = nsol)
        DifG = rep(DifG, length = nsol)
        ll = lapply(1:nsol, function(i)
                       mkSoluteInfo(F11[solID == i, ], DifW[i], DifG[i]))

        F = c(F[seq(1, s - 1)], unlist(ll), F[seq(e, length(F))])
        blocks$F = F
    }
    
    structure(unlist(blocks), class = "SELECTOR")
}

mkSoluteInfo =
function(d, difw, difg)    
{
     c("    DifW     DifG      n-th solute",
       sprintf("      %f    %f", difw, difg),
       showDataFrame(d)
      )
}

`$.SELECTOR` =
function(x, name)
{
  structure(getBlocks(x)[[name]], class = "BLOCK")
}

`[[.SELECTOR` =
function(x, i, j, ...)
{
  getValue(x, i)
}

`[[.BLOCK` =
function(x, i, j, ...)
{
  getValue(x, i)
}



getBlocks =
function(txt)
{
    b = split(txt, cumsum(grepl("*** BLOCK", txt, fixed = TRUE)))
    names(b)[-1] = sapply(b[-1], function(x) gsub(".*BLOCK ([A-G]).*", "\\1", x[1]))
    names(b)[1] = "Version"
    lapply(b, unname)
}

trim =
function (x) 
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)

getValue =
function(block, varName, coerce = NULL)
{
    if(length(varName) > 1)
       return(sapply(varName, getValue))

    loc = findValue(block, varName)
    if(length(loc) == 1 &&  grepl(sprintf("%s\\(", varName), block[loc-1]) ) {
        # so a vector of values.
        ans = strsplit(block[loc], "[[:space:]]+")[[1]]
        ans = ans[ans != ""]
    } else
       ans = strsplit(trim(block[loc["line"]]), "[[:space:]]+")[[1]][ loc["element" ] ]
    if(is.character(coerce))
        as(ans, coerce)
    else if(is.function(coerce))
        coerce(ans)
    else
        ans
}

findValue =
function(block, varName)
{
    i = grep(sprintf("(^| +)%s( |\\(|$)", varName), block)
    if(length(i) == 0)
       stop("No variable named ", varName, " found in this block")

    els = strsplit(trim(block[i]), "[[:space:]]+")
    w = which(els[[1]] == varName)
    c(line = i+1, element = w)
}


replaceColumn =
function(block, var, nrow, values)
{
    loc = findValue(block, var)
    col = loc[length(loc)]
    loc = loc[-length(loc)]

    values = matrix(values, nrow)
    for(i in 1:nrow) {
        for(b in seq(along = loc)) {
            ln = loc[b] + i - 1
            block[ln] = replaceWord(block[ln], col, values[i, b])
        }
    }
    block
}

replaceData =
function(block, var, nrow, data, ...)
{
    loc = findValue(block, var)

    top = block[seq(1, length = loc[1] - 2)]
    rest = if(length(block) > loc[1] + nrow)
              block[seq(loc[1] + nrow,  length(block))]
           else
              character()
    
    rows = showDataFrame(data)
    c(top, rows, rest)
}

showDataFrame =
function(data, ...)
{
    odig = options()$width
    options(width = 10000)
    on.exit(options(width = odig))
    rows = capture.output(print(data, row.names = FALSE, ...))
}

replaceValues =
function(block, ..., .values = list(...))
{
    # should set all the scalars first.
    
    for(i in names(.values)) {
        val = .values[[i]]
        loc = findValue(block, i)
        if(is.matrix(val) || is.data.frame(val)) {
            # have to replace the existing data.
        } else if(length(val) > 1) {
            w = strsplit(block [ loc[1]-1 ], ",")[[1]]
            numVar = gsub(sprintf("%s\\(([^)]+)\\)", i), "\\1", w[length(w)])
            len = as.integer(getValue(block, numVar))
            if(length(val) != len)
                stop("number of values for ", i, " should have length ", len, " corresponding to ", numVar)
            g = rep(1:ceiling(length(val)/6), each = 6, length = length(val))
            if(length(val) <= 6)
                block[loc[1]] = paste(val, collapse = "     ")
            else {
                ll = tapply(val, g, paste, collapse = " ")
                block = c(block[seq(1, length = loc[1]-1)],
                          ll,
                          if(loc[1] < length(block))
                            block[seq(loc[1]+1, length(block))])
            }

        } else {
              # scalar
            block[loc[1]] = replaceWord(block[loc[1]], loc[2], val)
        }
    }
    
    block
}


replaceWord =
    #
    #  l = " t     t      -1       f"
    #  replaceWord(l, 3, 100)
    #
function(line, wordNum, value)
{
    if(is.logical(value))
       value = substring(tolower(as.character(value)), 1, 1)

    m = gregexpr("[-.a-zA-Z0-9]+", line)[[1]]
    s = m[wordNum] 
    e = s + attr(m, "match.length")[wordNum]
    tmp = c(substring(line, 1, s - 1), value, " ", substring(line, e + 1))
    paste(tmp, collapse = "")
}
             
