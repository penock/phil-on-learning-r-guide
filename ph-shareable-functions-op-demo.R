# Phil's PH demo of some of the functions, version 0.1, 7/8/13 

# INSTRUCTIONS:
# First, open the PH shareable functions .R file and use Ctrl-Shift-S in RStudio to source() that, which
#   runs all the code in that file, thus enabling my functions below.
# Alternatively, run it by using setwd() to get to the right path, then execute: source('ph-shareable-functions.R')

# Then, each line of code gradually here with Ctrl-Enter and see what it does.
# Also, move your typing cursor over the word "sandbox" and press F2. You'll see RStudio jumps you to see the source code.
# Take a look at the source code there, as sandbox() is a function of mine you can understand easily, as are ca, cal, pa(), and %_%
# F2 works for many functions, certainly your own that you create, so it's very useful to refer back to a function you wrote.

# For a demo of how the sandbox is useful, run this code
sandbox()   # makes the sandbox slist and sdf to play with
str(slist)  # shows the internal structure of them
str(sdf)
op(sdf)     # outputs sdf to a CSV... should open in excel
op.oe.SPSS(sdf) # Exports sdf to CSV and makes an SPSS .sps syntax file to open that file properly with SPSS

ca('Your output is now in the folder "' %_% getwd() %_% '/op, go take a look yourself...')

shell.exec(getwd()) # Opens the folder in Windows

