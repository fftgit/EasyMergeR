# *EasyMergeR* <img src="https://user-images.githubusercontent.com/58737211/208740639-6851af19-2cf1-4b0a-90e1-91d0b1c51a94.png" style='float: right' height="139" />

A Shiny-based R package supporting XLSX files merging and providing basic data manipulation abilities. This Shiny application is based on userful functions from R packages openxlsx, readxl, skimr, and tidyverse ecosystem R packages (dplyr, tidyr, lubridate, etc.). EasyMergeR is built using the golem framework.

# Installation
```{r}

devtools::install_git("https://github.com/ai4kids/EasyMergeR")

# local installation
devtools::install_local(**path_to_your_file**)
```

# Usage
```{r}
library(EasyMergeR) 
EasyMergeR::run_app()
```

