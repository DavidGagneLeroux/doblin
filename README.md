# Doblin

## Developed by the <http://www.serohijoslab.org/>

## [Download the Doblin user guide](https://github.com/dagagf/doblin/blob/master/vignettes/doblin.pdf) for a complete step-by-step on how to use our tool.

## Overview

As microbial populations exhibit complex lineage dynamics, interpreting such data poses considerable challenges. In response, we developed *Doblin*, a tool designed to capture the essence of complex DNA barcoding time series by inferring their dominant and persistent behaviors. The primary purpose of *Doblin* is to furnish an open-source toolkit for the preliminary analysis of abundance time series obtained via *Next Generation Sequencing* (NGS), thereby laying the groundwork for ecological and evolutionary studies of microbial populations.

![](https://github.com/dagagf/doblin/blob/master/vignettes/images/doblin_readme.jpg?raw=true)



## Installation

1. Open Terminal
2. Change the current working directory to the location where you want to clone the *Doblin* repository.
3. Type `git clone`, and then paste *Doblin*'s URL.

    `git clone https://github.com/dagagf/doblin`

4. Press **Enter** to create your local clone.
5. In Terminal, set your working directory to the `doblin/` folder.

## Usage
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
-c: Time point threshold. We cluster lineages that persist for at least '-c' time points.
```



