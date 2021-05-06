# R script to generate code to insert images intoto overleaf/latex
library(tidyverse)
library(glue)

# setwd("~/Google Drive/study/GT/ISYE 6740/Project/cda_proj/gravity_covid_TL/draft report/chart pack")

file_names <- list.files()[1:(length(list.files())-1)][1]
glue(
"
\\begin{figure}
    \\centering
    \\includegraphics[width=330px]{chartpack/{{ file_names }}
    \\caption{{ paste0('{',str_extract(file_names, '(?<= ).*(?=[.])')) }}
    \\label{fig: {{ str_extract(file_names, '[^ ]+') }}}
\\end{figure}

", .open = "{{")
