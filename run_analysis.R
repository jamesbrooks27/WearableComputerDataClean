run_analysis <- function() {
    ## Takes the data in UCI HAR Dataset and converts it to a tidy dataset
    
    ## First we'll get the index info so we can translate id's to their meanings
    activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
    names(activity_labels) <- c("activity_id", "activity_name")
    
    features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
    names(features) <- c("feature_id", "feature_name")

    
    ## Now get the training data
    train_set <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
    names(train_set) <- features[,"feature_name"]       # The features now have their proper names
    
    train_labels <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
    names(train_labels) <- "activity_id"

    train_set <- cbind(train_set, train_labels)                         # Tack on the labels column (activity_id)
    train_set <- merge(activity_labels, train_set, by = "activity_id")     #Add in the Activity Names
    
    subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
    names(subject_train) <- "subject_id"
    train_set <- cbind(subject_train, train_set)            # Tack on the subject id
    
    #Now add a column called Source to show that these records came from the Training Set    
    orig_source <- rep("train", nrow(train_set))
    source_col <- as.data.frame(orig_source)
    train_set <- cbind(train_set, source_col)            # Tack on this column

    
    ## Now for the test data
    test_set <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
    names(test_set) <- features[,"feature_name"]       # The features now have their proper names
    
    test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
    names(test_labels) <- "activity_id"
    
    test_set <- cbind(test_set, test_labels)                         # Tack on the labels column (activity_id)
    test_set <- merge(activity_labels, test_set, by = "activity_id")     #Add in the Activity Names

    subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
    names(subject_test) <- "subject_id"
    test_set <- cbind(subject_test, test_set)            # Tack on the subject id
    
    #Now add a column called Source to show that these records came from the Training Set    
    orig_source <- rep("test", nrow(test_set))
    source_col <- as.data.frame(orig_source)
    test_set <- cbind(test_set, source_col)            # Tack on this column
    
    
    # Now row bind the two sets together:
    full_set <- rbind(train_set, test_set)
    
    # Now we'll pare down the columns we don't need.  First, those that include "mean" or "std"
    cols_needed <- grep("-mean()|-std()", names(full_set))
    # Then another set that includes the ones we just added:
    cols_needed <- c(cols_needed, match("subject_id",names(full_set)))
    cols_needed <- c(cols_needed, match("activity_name",names(full_set)))

    trimmed_set <- full_set[ ,cols_needed]
    
    s1 <- split(trimmed_set, list(trimmed_set$activity_name, trimmed_set$subject_id))
    
    takeMeans <- function(x) colMeans(x[, 1:79], na.rm = TRUE)    #want to get numerics for all the values
    ll <- lapply(s1, takeMeans)
    
    
    avg_df <- t(as.data.frame(ll))                  # transform the data frame so it's got the experiments as rows and variables as columns
    
    cases <- row.names(avg_df)             #get the list of activities and subjects that make up the row names
    split_cases <- strsplit(cases,"\\.")   # split out by the period
    sc_df <- t(as.data.frame(split_cases))
    colnames(sc_df) <- c("activity_name", "subject_id")   #rename them properly    
    
    avg_df <- cbind(avg_df, sc_df)
    
    #avg_df[c(81, 80, 1:79)]              #reorder so the identifiers come first
    avg_df
}