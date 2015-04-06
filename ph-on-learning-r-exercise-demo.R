

source('ph-shareable-functions.R')


# This is a selection of quick and dirty solutions to some of the "Fun R exercises for you"
# from the Phil on Learning R: The Training Sequence guide.
# Sorry, it's not annotated nor very readable, just something I ploughed through quickly.

##### The Code Ninja main practice example

sandbox()
df <- sdf

t.test(df[['i']])
t.test(df$n)


# Runs t tests on specified columns (currentVars) in df
# Outputs via printing p and whether significant
runttests <- function (df, currentVars) {
  currentVars <- names(df)
  
  for (var in currentVars) {
    if (!is.numeric(df[[var]]))
      next
    tRes <- t.test(df[[var]])
    ca('p = ', tRes$p.value, ' for column ', var)
    
    if (tRes$p.value < .05) {
      cal(", so it's significant!")
    } else {
      cal(", meh, it's nonsignificant.")
    }
  }  
}

runttests(df=sdf, currentVars=names(sdf))



# Grandmaster Level 1... I made some tweaks for elegance too
runttests.gmaster.1 <- function (df, cols=names(df)) {
  cols <- cols[sapply(df[cols], is.numeric)] # narrow cols down to just numeric cols
  outdf <- data.frame()
  
  for (col in cols) {
    tRes <- t.test(df[[col]])
    outRow <- data.frame(colName=col,
                         p=tRes$p.value,
                         sig=ifelse(tRes$p.value < .05, 'sig', 'nonsig'))
    outdf <- rbind(outdf, outRow)
  }
  return (outdf)
}

op(runttests.gmaster.1(df))


# Grandmaster Level 2, lapply
runttests.gmaster.2 <- function (df, cols=names(df)) {
  cols <- cols[sapply(df[cols], is.numeric)] # narrow cols down to just numeric cols
  
  # private function just processes one column
  priv.oneCol <- function (colVec) {
    tRes <- t.test(colVec)
    outRow <- data.frame(colName='dontknow',
                         p=tRes$p.value,
                         sig=ifelse(tRes$p.value < .05, 'sig', 'nonsig'))
    return (outRow)
  }

  outList <- apply(df[cols], MARGIN=2, FUN = priv.oneCol) # keep in mind, lapply will set each list item's key to the df col name
  
  print(outList)
  
  outdf <- do.call(rbind, outList) # rbind makes those list item keys into row names
  outdf$colName <- row.names(outdf) # set the colName col to the correct names
  row.names(outdf) <- NULL # clear row names... I don't like using them usually
  
  return (outdf)
}

op(runttests.gmaster.2(df))






##### Fun R exercises from the Training Sequence... select ones

# 1. Add two numbers and say the number is cool by using the ca() function
ca(5+7, " is cool")

# 2. Do the same thing but create a variable called toPrint that you'll print via ca(toPrint) or similarly
# print(toPrint). In making this variable, use pa() (see my PH functions). Then, re-write it using my
# special syntax ("%_%").
toPrint <- (5+7) %_% " is cool"
print(toPrint)

# 4. Convert a data frame to a list and then back to a data frame. Write down your understanding: Compare
# and contrast data frames and lists. Lists can hold anything in them. But what limits are there to what
# data frames can hold?
sandbox()
l <- as.list(sdf)
l
l$cantgoindf <- list("yup", 5)
l
df <- as.data.frame(l)
df

# 5. Replace sdf$i with random numbers.
sdf$i
sdf$i <- sample(1:100, 3, replace=T)
sdf$i

# 6. By the way, how many ways can you say "sdf$i" more or less equivalently? (Different syntaxes.)
sdf$i
sdf[['i']]
sdf[[1]]
subset(sdf, T, select='i') # but this is of type data frame

# 7. Triple the number of rows in the "sdf" data frame. Do it by rbind() it over and over, by adding lines of NA.
sdf
for (nRowsToAdd in 1:(2*nrow(sdf))) {
  sdf <- rbind(sdf, NA)
}
sdf

# 8. "i" is the first column of sdf. Make it the last column. Hint: remove the column temporarily and then bind it back on.
sdf <- sdf[c(2:5, 1)]
sdf


# 9. using with()... rewritten
with(sdf, expr = {
  print(t.test(i))
  print(t.test(n))
})

# 10. Make your own function, a t.testWithNiceOutput() for example. it takes the output of t.test and turns it
# into your own custom format of a one-row data frame. That way, it looks organized, fits on one line.
t.testwithNiceOutput <- function (vec) {
  tRes <- t.test(vec)
  data.frame(t = tRes$statistic,
             df = tRes$parameter,
             p = tRes$p.value)
}
t.testwithNiceOutput(sdf$i)
t.testwithNiceOutput(sdf$n)


# 11. Take that function and make a new one that can run 10 t.tests. You can rbind() together the outputs, so
# you have just one output data frame with all the info, sort by p values and op() to Excel to play with.
manyts <- function (df, cols) {
  outList <- lapply(df[cols], FUN = t.testwithNiceOutput)
  return (do.call(rbind, outList))
}
manyts(df, c('i', 'n'))



