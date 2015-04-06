# Phil's most useful and simple utility functions version 0.1, 7/8/13 
# (Especially "op")


# Future plans: I hope to make a shareable version of the stat functions for things like t tests and correlations.
# I use those stats all the time, and what my function does is changes the normal R output from them into one line of a data frame
# so it shows the t value, p value, SD, the means, etc. all on one line. It's just not ready to share as I am in the process of
# improving it.


# cat convenience aliases
ca <- function(..., sep="") {
  cat(..., sep=sep)
}
cal <- function(...) ca(..., "\n")

# paste convenience aliases
pa <- function(..., sep="") paste(..., sep=sep)
"%_%" <- pa # You can use this like, 'hi'%_%'there'%_%'dude' ... equivalent to paste('hi', 'there', 'dude', sep="").
            # I use this ALL OVER my code all the time, very handy.


# Outputs a data frame to CSV and runs it... so runs Excel assuming you have Excel.
# Creates and a folder called "op", puts stuff in there automatically
# FYI, the "oe" abbrevation is what I call a data frame, for reasons having to do with my keyboard layout.
op <- function (oe, fname="", runFile=T, na="") {
  opTo <- "csv"
  wdSaved <- getwd()
  if (nchar(fname) == 0) {
    fname.core <- substitute(oe)
    fname.core <- paste(fname.core, collapse="\'")
    fname.parts <- c(path="op/auto",
                     core=str.make.legal.filename(as.character(as.list(fname.core))),
                     ext=opTo)
    fname.parsed <- parse.fname.makeFromParts(fname.parts)
    if (!file.exists("op/auto")) {
      dir.create("op")
      dir.create("op/auto")
    }
    if (file.exists(fname.parsed[["whole"]])) {
      originalWhole <- fname.parsed[["whole"]]
      randomString <- rndString(4)
      fname.parsed[["core"]] <- pa(fname.parsed[["core"]], "-", randomString)
      fname.parsed <- parse.fname.makeFromParts(fname.parsed)
      ca("op(): Since \"", originalWhole, "\" exists already, creating new file with random suffix (", fname.parsed[["whole"]], ").")
    }
  } else {
    if (!strs.detect(fname, opTo))
      fname <- pa(fname, ".", opTo)
    fname.parsed <- parse.fname(fname)
  }
  if (opTo=="csv") {
    outputFullPath <- fname.parsed[["whole"]]
    write.csv(oe, outputFullPath, row.names=F, na=na)
    PATH.OP.LAST <<- outputFullPath
    if (runFile==T)
      run.file(outputFullPath)
  }
}
op.oe <- op

# Outputs data frame to SPSS, creating a CSV and syntax based on your fname.
# This is the most complex of any of the functions. There is no other proper converter out there on the internet that I could find.
# (By the way, going the OTHER direction, opening SPSS data in R, is pretty easy, just do library(foreign) then read.spss("C:\yourfile.sav"))
# When running the .sps in SPSS, you'll see you need to type into the syntax there the correct file path to the CSV.

# Before running op.oe.SPSS, it's nice but not required to run oe.convert.numericToInteger() on your data frame first, to get rid of trailing .00 in SPSS.

# Behavior notes:
#  1. You must not use Excel to modify your CSV before opening via SPSS, because Excel messes it up by unquoting all text.
#  2. SPSS seems to have no way of understanding missing values in String variables, so any NAs from oe will become "" in SPSS
#     * That is, they will not properly become "MISSING" ... you should set the SPSS MISSING parameter in SPSS variable view to blank text ("" basically)
#  3. You should convert any date columns to proper R dates, with as.Date, before using this export
op.oe.SPSS <- function (oe, fname="") {
  # Here's will need to turn dates into yyyy/mm/dd because the default output from R, of yyyy-mm-dd, SPSS won't take it
  # for classes, see list of basic types allowed in R data frames... also Date POSIXct...
  classes <- lapply(oe, class)
  classes <- lapply(classes, FUN=function (elem) { elem[[1]] }) # shorten to only first classes
  
  # Some classes translate into SPSS format with no changes to the data. These I call "easy" classes.
  ## Translate the easy class names into SPSS format names
  sTypes <- classes
  sTypes[classes == 'numeric']   <- 'F8.2'
  sTypes[classes == 'integer']   <- 'F8.0'
  sTypes[classes == 'logical']   <- 'F8.0'
  sTypes[classes == 'factor']    <- 'F8.0'
  
  toWrite.labels <- toWrite.nominals <- NULL
  # The "difficult" classes require changes to the actual data cols and specifying SPSS widths for Strings
  for (iCol in seq(ncol(oe))) {
    # Convert actual col data
    col.class <- classes[iCol]
    if (col.class == 'logical') {
      oe[[iCol]] <- as.integer(oe[[iCol]])
    } else if (col.class == 'POSIXct' | col.class == 'Date') {
      oe[[iCol]] <- format(oe[[iCol]], format="%Y/%m/%d")
    } else if (col.class == 'factor') {
      toWrite.labels <- c(toWrite.labels,
                          'VALUE LABELS '%_%names(oe)[iCol],
                          pa(1:length(levels(oe[[iCol]])), ' "', levels(oe[[iCol]]), '"'),
                          '.')
      toWrite.nominals <- c(toWrite.nominals, names(oe)[iCol])
      oe[[iCol]] <- as.integer(oe[[iCol]])
    }
    if (col.class == "character") {
      sTypes[iCol] <- 'A'%_%max(nchar(oe[[iCol]]))
    } else if (col.class == 'POSIXct' | col.class == 'Date') {
      sTypes[iCol] <- 'SDATE10'
    }
  }
  
  op.oe(oe, fname=fname, run=F) # creates output file and sets PATH.OP.LAST to that path
  fconn <- file(PATH.OP.LAST%_%'.sps', 'w')
  PATH.OP.SPSS <- getwd()%_%'/'%_%PATH.OP.LAST
  PATH.OP.SPSS <- strs.replace(PATH.OP.SPSS, '/', '\\')
  toWrite <- c(
    "*** This syntax file was generated by Phil's output to SPSS function (written 4/20/13) ***.",
    '******* Load from CSV file *******.',
    'GET DATA',
    '  /TYPE=TXT',
    '  /FILE="'%_%PATH.OP.SPSS%_%'"',
    '  /DELCASE=LINE',
    '  /DELIMITERS=","',
    "  /QUALIFIER='\"'",
    '  /ARRANGEMENT=DELIMITED',
    '  /FIRSTCASE=2',
    '  /IMPORTCASE=ALL',
    '  /VARIABLES=')
  
  
  toWrite <- c(toWrite,
               pa('    ', names(oe), ' ', sTypes),
               '.')
  
  toWrite <- c(toWrite,
               '******* Set value labels for factors *******.',
               toWrite.labels)
  
  toWrite <- c(toWrite,
               '******* Wrapping up *******.',
               'DATASET NAME DataSetFromR WINDOW=FRONT.',
               'VARIABLE WIDTH ALL (8).', 
               'VARIABLE LEVEL ALL (SCALE).',
               pa('VARIABLE LEVEL ', pa(collapse=' ', toWrite.nominals), ' (NOMINAL).'))
  
  writeLines(toWrite, con=fconn)
  close(fconn)
}

# Converts numeric cols to integer, if all values are equal to integers
# Helpful to run before outputting to SPSS
oe.convert.numericToInteger <- function (oe) {
  for (iCol in 1:ncol(oe)) {
    if (class(oe[[iCol]])[1] == 'numeric') {
      col.noNAs <- na.omit(oe[[iCol]])
      if (all(col.noNAs == as.integer(col.noNAs)))
        oe[[iCol]] <- as.integer(oe[[iCol]])
    }
  }
  oe
}

# Does a shell.exec, so you can open a file (or folder)... you can actually have R open your folder in Windows for you
# It replaces slashes with double-backslashes, because if you don't do that then relative paths
# that start with a folder name, like shell.exec("opn/file.txt"), don't work 
run.file <- function (path, ...) {
  shell.exec(strs.replace(path, "/", "\\"), ...)
}


# Creates a list and data frame for you, as global variables, to play with... it's like a play sketchpad.
# I use this because one is often testing how things work with data frames and list, how to manipulate...
# So you need a scratch one often.
# The numbers and values in it are arbitrary... I tried to do a variety of data types (integer, numeric, logical, character, with an NA in there too)
sandbox <- function () {
  slist <<- list(5, 10, 12)
  sdf <<- data.frame(i=c(3, 5, 7), n=c(9.5, 10.99, 200), f=c("item1", NA, "item3"), l=c(TRUE, TRUE, FALSE), s=c("hi", "there", "how are you"))
  sdf$i <- as.integer(sdf$i)
  sdf$s <- as.character(sdf$s)
  cal("SANDBOX... Created: ")
  cat ("slist: \n"); print(slist); cal ("data frame sdf: ");print(sdf)
}


# For an example of how the sandbox is useful, run this code (must run it after you already ran the function definitions below:
# sandbox()   # makes the play sandbox list and df
# str(slist)  # shows the internal structure of them
# str(sdf)
# op(sdf)     # demonstrates the op() function's capability... should open in excel
# op.oe.SPSS(sdf) # Exports sdf to Excel






############ The rest are not super useful, just are needed for op functions....

strs.replace <- function(haystack, needle.old, needle.new) {
  gsub(pattern=needle.old, replacement=needle.new, x=haystack, fixed=T)
}
# Returns a random alphanumeric string of specified length
rndString <- function (len) {
  paste(sample(c(letters, 0:9), len, replace=T), collapse="")
}
# Takes a filename or path such as C:/Dropbox/aoe.csv and returns a named character vector with whole, path (just the directory part), core, and ext
parse.fname <- function (whole) {
  whole <- str_replace_all(whole, "/", "\\\\")
  if (strs.detect(whole, ".")) {
    locMatrix <- str_locate_all(whole, "\\.")[[1]]
    lastDotLoc <- locMatrix[[nrow(locMatrix), 1]]
    ext <- substr(whole, lastDotLoc + 1, nchar(whole))
  } else
    ext <- ""
  if (strs.detect(whole, "\\")) {
    locMatrix <- str_locate_all(whole, "\\\\")[[1]]
    lastBackslashLoc <- locMatrix[[nrow(locMatrix), 1]]
    path <- substr(whole, 1, lastBackslashLoc - 1)
  } else
    path <- "."
  core <- substr(whole,
                 ifelse(path == ".", 1, nchar(path) + 2),
                 ifelse(ext == "", nchar(whole), nchar(whole) - nchar(ext) - 1))
  fname.parsed <- parse.fname.makeFromParts(c(path=path, core=core, ext=ext))
  if (whole != fname.parsed[["whole"]])
    stop(pa("parse.fname(): parsing failed... the recombined whole, \"", fname.parsed[["whole"]], "\", does not match the original whole, \"", whole, "\""))
  fname.parsed
}
# Makes or remakes the "whole" element... you can pass it a complete fname.parsed or just the parts, i.e. a named char vector with path, core, and ext
parse.fname.makeFromParts <- function (fname.parts) {
  if (fname.parts[["path"]] == "")
    stop("parse.fname.remakeWhole(): fname.parsed[[\"path\"]] can't be empty... you should make it \".\"")
  fname.parts[["proper"]] <- pa(fname.parts[["core"]], ifelse(fname.parts[["ext"]] == "", "", pa(".", fname.parts[["ext"]])))
  fname.parts[["whole"]] <- pa(ifelse(fname.parts[["path"]] == ".", "", pa(fname.parts[["path"]], "\\")),
                               fname.parts[["core"]],
                               ifelse(fname.parts[["ext"]] == "", "", pa(".", fname.parts[["ext"]])))
  fname.parts
}
# Returns start position of (first occurrence of) string needle in haystack, NOT using regex
# Returns FALSE if not found
strs.pos <- function (haystack, needle, caseSens=T, findLastOne=F) {
  if ( (is.null(haystack) | is.na(haystack) | length(haystack) != 1) )
    stop("strs.pos(): Your haystack arg is not legit -Phil")
  else if (is.null(needle) | is.na(needle) | length(needle) != 1)
    stop("strs.pos(): Your needle arg is not legit -Phil")
  else if (nchar(haystack) < nchar(needle))
    return (FALSE)
  else if (needle == "")
    return (ifelse(haystack == "", 1, FALSE))
  if (caseSens==F) {
    haystack <- tolower(haystack)
    needle <- tolower(needle)
  }
  res <- gregexpr(pattern=needle, text=haystack, fixed=T)
  if (res[[1]][1] == -1) {
    FALSE
  } else {
    if (findLastOne==F) {
      res[[1]][1]
    } else {
      res[[1]][length(res[[1]])]
    }
  }
}
strs.detect <- function (haystack, needle, caseSens=T) {
  strs.pos(haystack, needle, caseSens=T) != FALSE
}
# Returns legal varname.
# You can use this to check if text is legal by checking nchar(str.make.legal.varname(text)) == nchar(text)
str.make.legal.varname <- function (name) {
  str_replace_all(name, "[- \\[\\$@\\\\/:\\*<>\\|\\\"]", "")
}
str.is.legal.varname <- function (name) { str.make.legal.varname(name) == name }

# Returns legal filename.
str.make.legal.filename <- function (name) {
  name <- gsub(pattern="[\\[\\$@\\\\/:\\*<>\\|\\\"]", replacement="'", x=name, fixed=F)
  name
}

