# Phil on Learning R: The Training Sequence
by Phil Enock, written July 2013, updated July 2014

## Introduction
This guide I wrote should be useful for anyone just getting started with R who is willing to commit to investing time in the learning process. It's not a rush-to-get-something-running guide. It's more "how to go in depth" with your learning.

Much of its message is geared toward those who aren't experienced programmers. If you were trained in SPSS or similar statistical software with a GUI, and you're transitioning to writing raw code, this may help you understand your options for how to weave together programming code and analyses.

I guess I wrote it as a reaction to how many intros to R (whether in a guide, class, or one-day workshop) show you how to run stats with line-by-line syntax, but there's no attention paid to how to topics like manipulating data, organizing and structuring your code, and harnessing the power of programming for your analyses.

## Files in this repository
They key files here are:

  - [phil-on-learning-r-guide.pdf]: The guide, a 21-page PDF
  - [ph-shareable-functions.R]: A few useful functions, most notably:
   * op(dataframe): Outputs a data frame to CSV and runs it... so, this will view the data in Excel if you have Excel
   * op.oe.SPSS(dataframe): Outputs data frame from R to SPSS, creating a CSV and syntax to run from SPSS... this is crucial if you're working with data in both SPSS and R, and I had to write it from scratch as existing solutions didn't work
  - [ph-shareable-functions-op-demo.R]: Demonstrates the ph-shareable-functions
