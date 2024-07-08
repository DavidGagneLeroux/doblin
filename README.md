# Doblin

## Developed by the <http://www.serohijoslab.org/>

## [Download the Doblin user guide](https://github.com/dagagf/doblin/blob/master/vignettes/doblin.pdf) for a complete step-by-step on how to use our tool.

## Overview

As microbial populations exhibit complex lineage dynamics, interpreting such data poses considerable challenges. In response, we developed *Doblin*, an R-based pipeline designed to capture the essence of complex DNA barcoding time series by identifying their dominant and persistent tendencies. The primary purpose of *Doblin* is to furnish an open-source toolkit for the preliminary analysis of abundance time series obtained via *Next Generation Sequencing* (NGS), thereby laying the groundwork for ecological and evolutionary studies of microbial populations.

![](https://github.com/dagagf/doblin/blob/master/vignettes/images/doblin_readme.jpg?raw=true)



## Quick Setup Guide

*Before installing the pipeline, make sure R is installed on your device.*

**Step 1:** Open Terminal (compatible on both Linux & Windows Terminal).

**Step 2:** Change the current working directory to the location where you want to clone the *Doblin* repository.

**Step 3:** Clone the repository using git command. Then, set your working directory to the `doblin/` folder.

```
    ~$ git clone https://github.com/dagagf/doblin
    ~$ cd doblin
```
    
## Quick Start Guide

*Make sure you have already gone through the **Quick Setup Guide** above.*


Commad line:
`Rscript ./main.R -t [MIN_FREQUENCY] -o [OUTPUT_DIR] -n [INPUT_FILE_NAME] -i [INPUT_FILE] -c [TIME_CUTOFF]`

 Here's an example of how to use the command line:
`Rscript ./main.R -t 0.0005 -o ~/Documents/ -n test -i ~/Documents/test.csv -c 12`

Where
```
-t: Minimum frequency above which barcodes are assigned colors [default: 0.0005].
The barcodes that do not reach the minimum frequency are colored in grey. 
This argument is used when plotting the dynamics. 
-o: Output directory [default: current working directory].
-n: Input file name.
-i: Input file.
-c: Minimum duration, in terms of time points, for which lineages must persist to be eligible for clustering.
```



