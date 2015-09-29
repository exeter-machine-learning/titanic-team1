# Predicting Survival on the Titanic
### A Kaggle Challenge Implemented in R

---

### Directory Layout

|--titanic-team1/  
|----analysis.r/  
|----data/    
|----output/   
|----README.md  


* `analysis.r` is our main program. It reads files from the `data` directory and edits and/or writes files into the `output` directory. It can be run by calling `source("analysis.r")` from the console, provided you are in the correct directory.

* The `data` directory contains data used in the analysis. This is treated as read only; in paricular the R files are never allowed to write to the files in here. In this project, .csv files are used.

* The `output` directory contains all files produced by `analysis.r`. In this case, it contains a collection of .csv files which contain the predictions produced by our logistic regression – one file for each regression performed.

### A Walkthrough of Our Analysis

First, we extract the data almost entirely raw from the .csv files and store it into two dataframes, one for training data and one for testing data.  
Next, `getTitle`, `changeTitle`, `consolidatedTitles`, and `imputeMedian` serve as helper functions for `featureNorm,` in which we find the title for each passenger (e.x. "Mrs.") and consolidate similar titles into groups, fix some of the inconsistencies in the data, and estimate missing ages and fare prices based off of title and passenger class, respectively.  
In our next main function, `featureEng`, we continue to process the data, creating features that tell us if the passenger is a child, how many family members they have aboard the ship, and which deck and side of the boat their rooms lie on.  
Then we actually call those functions and create a partition in the data which the logistic regression function uses to test its hypotheses.   
What follows are a series of logistic regression functions, each one trying out a slightly different combination of features to focus on. The commented percentages are how successful the prediction model that is given by that function is when compared against Kaggle's test data.  
The last lines of code make the predictions based on whichever model we are currenlty using – in this case, it is glm.tune.9. The predictions are written to a .csv file that can be submitted to Kaggle for evaluation.