### Assignment 1
## Libraries
library(tidyr)
library(readr)
library(magrittr)
library(stringr)
library(dplyr)
library(ggplot2)

## Problem 1
# For problem 1 I manually opened the README.MD file and filled the information.
# I then saved the file.

## Problem 2
vec <- read_lines("suites_dw_Table1.txt")  # Create a vector of all lines 

# Find row line where separator line occur
L <-                                       
  vec %>% 
  # Here it loops through each line and returns the row number of the first line
  # which starts with "--"
  {function(v) for (i in length(v):1) if (substr(v[i], start=1,stop=2) == "--") return(i)} ()

# Find column names
cols <- str_split(vec[L-1], "\\|") %>% # Line which comes before separator 
  unlist() %>% # Unlist to create a vector of names
  str_trim()   # Remove unnecessary spaces in column names

# Create dataframe of the data
df <- data.frame(vec[(L+1):length(vec)]) # Will be every line after separator

# df only contains one column, rename it for easier access later
colnames(df) <- c("all")

# Create final df with correct columns
df <-
  df %>% 
  # Separate the column into more, with names defined by "cols"
  separate(all, cols, sep = "\\|") %>% 
  # Make every column except "name" and "md" numeric
  mutate(across(all_of(cols[!cols %in% c("name", "md")]), as.numeric)) 

## Problem 3
## Smaller objects are more difficult to find if they are far away, but they
## should be as many as smaller objects that are close by. Therefore, to  
## conclude if smaller objects are underrepresented, we can study if there is a  
## existence bias towards smaller objects when they are far away.
## To achieve this we want to plot the size of an object (here in stellar mass), 
## on it's distance from us (the observer). We can also add brightness (color)
## and diameter (size) to make the plot more interesting. 
## We obtain the following plot:

df %>% 
  # Filter to only study stellar mass below 3000, this is an arbitrary number.
  # I am no astronomer. 
  filter(exp(log_lk) <= 3000) %>% 
  # Plot the resulting df
  ggplot() +
  geom_point(aes(x=D, y=exp(log_lk), size=a_26, color=m_b)) + # Points
  geom_smooth(aes(x=D, y=exp(log_lk)), method="lm") +       # Linear regression
  scale_size_continuous(range = c(0, 3)) +  # Scale sizes to fit visually nice
  # Set understandable names 
  labs(color="Brightness (m_b)", size="Diameter (a_26)",
       title="Galaxies size over distance (stellar mass <= 3000)",
       x="Distance from earth in Mpc",
       y="Stellar mass in solar units")

## Here we can see that there is a clear trend to finding many more small
## objects closer than far away. We study the regression results below.

reg <- summary(lm(exp(log_lk) ~ D, data=filter(df, exp(log_lk) <= 3000)))
reg

## From this we see that for every Mpc away from us, the objects we find 
## increase with a stellar mass of ~115.2. This is not very likely as there 
## should be at least as many small objects far away as is close by. In other
## words, we would expect this coefficient to be ~0. But, from the regression
## we find that this value is significant with 0.001 significance. 
## This indicates that small objects are underrepresented, if we assume uniform
## distribution of smaller objects over distance. 
## Again, this might be because it is harder to spot and find smaller objects
## that are far away than close ones.
