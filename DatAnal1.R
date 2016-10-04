# purpose -> to learn R syntax
# problem -> To create a machine learning algorithm that can correctly predict who would
# die or live on the Titanic

### Part 1: Visualizing the Data

## Formatting the Data

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)
# adds the column 'survived' to test, by updating the data fram for all of test[,]
# In the Survived column, "None" is repeated for the number of rows in test
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# trying to make the survived column appear after Passeger Id, like it is in the train csv
test.survived <- test.survived[,c(2,1,3:ncol(test.survived))]

# now that both test.survived and train have the same number of variables, we 
# can combine them by row
# essentially we're adding the rows of test.survived to train and storing this new
# table in data.combined

 data.combined <- rbind(train, test.survived)

 
 # Ascertain the structure of an object
 str(data.combined)
 # TYPING the above will return the things commented below
   # 'data.frame':	1309 obs. of  12 variables:
   #  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
   #  $ Survived   : chr  "0" "1" "1" "1" ...
   #  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
   #  $ Name       : Factor w/ 1307 levels "Abbing, Mr. Anthony",..: 109 191 358 277 16 559 520 629 417 581 ...
   #  $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
   #  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
   #  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
   #  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
   #  $ Ticket     : Factor w/ 929 levels "110152","110413",..: 524 597 670 50 473 276 86 396 345 133 ...
   #  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
   #  $ Cabin      : Factor w/ 187 levels "","A10","A14",..: 1 83 1 57 1 1 131 1 1 1 ...
   #  $ Embarked   : Factor w/ 4 levels "","C","Q","S": 4 2 4 4 4 3 4 4 4 2 ...
  # The function factor is used to encode a vector as a factor (the terms 'category' and 'enumerated type' are also used for factors).
 
 # turns pclass and survived to factor, instead of int and chr
 data.combined$Survived <- as.factor(data.combined$Survived)
 data.combined$Pclass <- as.factor(data.combined$Pclass)
 # they are both factors now
 str(data.combined)
 
 # look at gross survival rates
 table(data.combined$Survived)
 
 # look at distribution accross classes
 table(data.combined$Pclass)
 
## Representing Data: Survival rates v. Class
 
 # load ggplot2 library after installing
 library(ggplot2)
 
 # Hypothesis: rich people survived at a higher rate
 # 1) factor train's pclass
train$Pclass <- as.factor(train$Pclass)
 # 2) ggplot
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  geom_histogram(binwidth = 0.5) + 
  xlab("Pclass") + 
  ylab("Total Count") +
  labs(fill = "Survived")
 
# So now we get a powerful representation. If you are rich you are more 
# likely to survive than die and if your were poor you were more likely 
# to die than survive.

#____________________________________________________________________________________________
## Formatting Data: Survival rates v. Name/Title

# But we've only looked at class and survived fields to make this assumption.
# Let's see how if your name has any correlation to whether you survived or not

# Examine the first few names (called by head) which have been made as characters
head(as.character(train$Name))

# Let's make sure there aren't any statistical anomalies like duplicate entries.
  # How many unique elements are there in traina nd test
  # (return the length of(return only the uniqe ones (Converted to character type(all the names))))
  length(unique(as.character(data.combined$Name)))

  # so maybe 2 duplicates. How do we know that two people don't have the same name?
  # Normal English
  # Store in dup.names what is returned when you do the following:
  # (as character) all such names (as character) in the "name" column from data.combined
  # Code like English
  # Store in dup.names what is returned when you do the following:
  # as character (from data.combined [which are (that are duplicates (as character (all names))), in name column])
  dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

  # look at these records in the combined set
  # Code version: return the rows in data.combined
  data.combined[which(data.combined$Name %in% dup.names),]
  # We see that Kate Connolly 1 and 2 both had 3rd class tickets, with same entries for name, sex, sibsp, parch, and embarked
  # but different age and ticket number suggest they're different people
  # Same with Both the James Kelly 1 and 2
  # So they're not really duplicates

# No statistical anomalies. Let's proceed to make predictions based on name.
  
 # Using the titles in the names, like Mr. and Mrs. and Miss, we can try to see if there is a relation between title and sibsp for example
 misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
 misses[1:5,] 
 # just looking at 1st 5, 4/5 survived and were in 3rd class, which is amazing for being in 3rd class.
 # Only 1 (4 year old) has 1 sibsp and 1 parch entry, so she probably has a sibling and 1 parent
 
 # Look at title v. age
 mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
 mrses[1:5,]
 # just looking first 5, we see 3/5 with 1 sibsp (prbably spouse if they're married), and only 1/5 with a parch entry
 
 males <- data.combined[which(train$Sex == "male"),]
 males[1:5,]
  
 # So, cool data.
 # Let's expand upon the relationship between survived and pclass by adding title
 # Let's create a utility function to help us.
 
      # How to define a function in R
      # functionName <- extract(var) {
      #    var <- (as.character or as.factor(var))
      #    ......body...
      #      }
 
      extractTitle <- function(name) {
        name <- as.character(name)
        # grep is pattern matching
        if (length(grep("Miss.", name)) > 0) {
          return ("Miss.")
        } else if (length(grep("Master", name)) >0) {
          return ("Master.")
        } else if (length(grep("Mrs.", name)) >0) {
          return ("Mrs.")
        } else if (length(grep("Mr.", name)) >0) {
          return ("Mr.")
        } else {
          return ("Other")
        }
      }
      
 titles <- NULL
 for (i in 1:nrow(data.combined)) {
   titles <- c(titles, extractTitle(data.combined[i, "Name"]))
 }
      
 data.combined$title <- as.factor(titles) 
 
## Representing Data: Survival rates v. Name/Title per Class
 
 # Relationship between pclass, title, and survived
 ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
   stat_count(width = 0.5)  +
   facet_wrap(~Pclass) +
   ggtitle("Pclass") +
   xlab("Title") +
   ylab("Total Count") +
   labs(fill = "Survived")
 # So now we have a visualization model to help us understand the data. Mrs in First class  = most
 # likely to survive. Mr. in Third Class = leas likely to sruvive. 
 
## Representing Data: Survival rates v. Sex per Class
 
 # Relationship between pclass, title, and survived
 ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
   stat_count(width = 0.5)  +
   facet_wrap(~Pclass) +
   ggtitle("PClass") +
   xlab("Sex") +
   ylab("Total Count") +
   labs(fill = "Survived")
 
 ## Representing Data: Survival rates v. Sex per Class
 
  summary(data.combined$Age)
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #  0.17   21.00   28.00   29.88   39.00   80.00     263 
  # 263 null values.
  # Lots of missing values will make it hard to use Age as a variable.
 
 # These data representations will help us on the machine-learning model.
 # ______________________________________________________________________
 
  
 