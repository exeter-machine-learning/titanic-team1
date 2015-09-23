# Predict survival on the Titanic
A Kaggle challenge implemented in R

## Directory Layout


|--titanic-team1/  
|----r/  
|----data/  
|----figs/  
|----output/  


* The `r` directory contains various files with function definitions (but only function definitions - no code that actually runs).

* The `data` directory contains data used in the analysis. This is treated as read only; in paricular the R files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.

* The `figs` directory contains the figures. This directory only contains generated files; that is, I should always be able to delete the contents and regenerate them.

* The `output` directory contains simuation output, processed datasets, logs, or other processed things.
