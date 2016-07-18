####
# R tutorial


# Loading data from a csv file
champion_facts <- 
  read.table(
    "champion_facts.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) 

##################
# Basic Data types

# Integers
2
is(2)

# Strings

"This is an R tutorial"
is("This is an R tutorial")

# Numeric

3.14
is(3.14)

# Logical operations

T
F
!T # ! negates logicals, turning T to F and vice versa
T | F # | stands for "or"
T & F
T & T 
is(F)

combine_Cru <- F # try switching to T and re-running the following 2 lines
if(combine_Cru)
{print("Cru is combined")} else {print("Not combined")}

is(combine_Cru)

#####################
# Advanced data types

# Vectors

# Concatenation
v <- c(1,4,7) # Concatenation operator creates vectors
w <- c(v, 2, 3) # Concatenation operator accepts vector inputs, as well

# Sequences
v <- 1:10
v <- 5:20

  # Subsetting
  # Subsetting by specifying element positions
  v <- c(1,4,7)
  v[2]
  v[c(2,3)] 
  # Subsetting with logical operations
  v == 4    # Double-equals operator returns T for all elements of v that equal 4
  v %in% c(7, 4, 18) # %in% operator returns T for all elements of v that appear in the vector c(7,4,18)
  v[c(F, T, F)]
  v[v==4]
  v[!(v==4)] # Remember !? Pretty handy
  v[v==4 | v == 7] 
  v[v %in% c(7,4,18)]

# Coercion.
v <- c(2, T, F, F, T) # R "coerces" the T and F values to integer values so that v only contains one 'data type'
is(v)
x <- c(2, T, F, F, "hello")
is(x)

# Functions
v <- c(7,4,18)
length(v)
mean(v)
sd(v) # Standard deviation

v + c(3,6,2)
v + 3
v + c(3, 6) # What went wrong?
v <- c(v, 22)
v + c(3, 6)       # If one is shorter, R 'recycles' the shorter vector, as long as
                  # it can use the whole thing. (Go back and read the last warning message.)
v + c(3, 6, 3, 6) # Same result
v + 3             # This is actually another example of recycling. 3 is stored as a vector of length 1.


length(w)


# Lists - can contain different data types
l <- list(1, T, F)
l <- list(1, T, F, v)
l[[4]]
l[4]

l2 <- list(element_1 = 1, element_2 = v) # Named list. Note that we use 'single equals' to assign names, not 'double equals'.
l2$element_1
l2$element_2

# Data Frames - Lists of identical-length vectors. (Essentially)
  champion_facts
  View(champion_facts)
  head(champion_facts)
  colnames(champion_facts)
  is(colnames(champion_facts))
  
  champion_facts[1,3] # Find element in 1st row and 3rd column
  champion_facts[7,] # 7th row
  champion_facts[,3] # 3rd column
  
  # How many ways are there to select the 4th column?
  champion_facts$dont.exclude
  champion_facts[,4]
  champion_facts[[4]]
  champion_facts[,colnames(champion_facts) == "dont.exclude"]
  champion_facts %>% select(dont.exclude)
  
  # How can you select both the 4th and 5th column?
  champion_facts[,4:5]
  champion_facts[[4:5]]
  champion_facts[,colnames(champion_facts) %in% c("dont.exclude", "champion_organization")]
  champion_facts %>% select(dont.exclude, champion_organization)
  
  # What is UMI Connection's champion ID?
  library(dplyr)
  
  champion_facts[champion_facts$champion_name == "UMI Connection",]
  
  champion_facts %>% 
    filter(champion_name == "UMI Connection") %>%
    select(champion_id) # Filtering with dplyr and the pipe operator (%>%)
  
  filter(champion_facts, champion_name == "UMI Connection") 
  select(filter(champion_facts, champion_name == "UMI Connection"), champion_id)
  # Notice how much harder this code is to read and write without the pipe operator
  # Check out 
  vignette('magrittr')
  # to learn about the pipe operator.
  
  # What are the champion IDs for all champions with 'TYRO' in their name?
  # Use the 'grep' and 'grepl' functions to search for inexact string matches
  
  grep("TYRO", champion_facts$champion_name) #This call returns the positions of all
  grepl("TYRO", champion_facts$champion_name) #This call produces a logical vector for subsetting. ('l' for 'logical')
  
  champion_facts[grepl("TYRO", champion_facts$champion_name),] # The base R way
  champion_facts[grep("TYRO", champion_facts$champion_name),]
  
  champion_facts %>% filter(grepl("TYRO", champion_name)) # The dplyr way.
  
  champion_facts %>% 
    filter(grepl("TYRO", champion_name)) # Run both lines at once. Still works.
  
  # 

# Functions
  
# Make your own
  
return_odd <- function(v){ # Return all odd elements of v
  v[v %% 2 == 1]
}


# There's also a data type called 'factors' which you should be aware of. They
# show up quite often, but they have some confusing behaviors that make them
# frustrating to work with. I try to avoid them as much as possible. (Notice
# the option stringsasfactor = F in the load.csv calls at the top.)