(function(x){x+1})(2)

# three parts of a function
# arguments, body, environment
# return value is the last executed expression, or the first executed return () statement

args(sd)  # see the argment of sd function

#When overriding a default value, it's good practice to use the name.

args(mean)
?mean()


mean(c(1:9, NA),trim=0.1, na.rm=TRUE)
#the fraction (0 to 0.5) of observations to be trimmed from each end of x 
#before the mean is computed.


f <- function(x) {
  if (TRUE) {
    return(x + 1)
  }
  x
}
f(2)
#The body of the conditional is always evaluated and the function returns early without ever running x.

f= function() {
  x=1
  y=2
  c(x,y)
}
f()


x=2
g=function(){
  y=1
  c(x,y)
}
g()

rm(x)
g()  # rm x, x is not defined, error



f=function() x

x=15
f()

x=20
f()


l=function(x) x+1

m = function() {
  l = function(x)  x*2
  l(10)
}

m()


# each call to a function has its own clean enviroment
rm(a)
j = function(){
  
  if(!exists("a")){
    a=1
  }else{
    a=a+1
  }
  print(a)
}

j()
j()


y <- 10
f <- function(x) {
  x + y
}
f(10)
# Because y is not passed in as an argument to the function, R looks outside of the function environment

y <- 10
f <- function(x) {
  y <- 5
  x + y
}
f(10)

#The value of x is passed in as an argument to the function and the value of y is defined inside of the function


f <- function(x) {
  y <- 5
  x + y
}
f(5)
y


#two types of vectors in R
# atomic vectors of six types: logical, integer, double, character, complext, and raw
# only need to know the first 4

#list, recursive vectors, list can contain another list
#atomic vectors are homogeious
#list are hetergenous

typeof(letters)==typeof("letters")
typeof(1:10)

#Even though y is set equal to 5 within the body of the function, 
#the object does not exist in the global environment.


typeof(NULL)
length(NULL)
typeof(NA)
length(NA)
# NULL and NA are not the same


x=c(1,2,3,NA,5)
is.na(x)

#missing values are contagious

NA+10
NA/2
NA>5
10==NA
NA==NA

# list
# complited return objects are ofthen lists, i.e, from lm()
# created with list()
# subset with [, [[, or$
# [ extracts a sublist
# [[ and $ extract elements, remove a level of hierachy

a =list(
  a=1:3,
  b="a string",
  c=pi,
  d=list(-1,-5)
)
str(a[4])  # a list containing the d element

str(a[[4]])  #remove hirechachy, simply return a list of two lelments

a[[4]][1]
a[[4]][[1]]



#we'll mostly use double bracket ([[]]) subsetting by index and by name.

#That is, my_list[[1]] extracts the first element of the list my_list, 
#and my_list[["name"]] extracts the element in my_list that is called name. 
#If the list is nested you can travel down the heirarchy by recursive subsetting. 
#For example, mylist[[1]][["name"]] is the element called name inside the first element of my_list.

#A data frame is just a special kind of list, so you can use double bracket subsetting on data frames too. 
#my_df[[1]] will extract the first column of a data frame and my_df[["name"]] 
#will extract the column named name from the data frame
data(mtcars)
mtcars[[1]]


#Extracting elements from the output of the names() and str() functions is a great way to explore the structure of a list.

#Calling names() on a list will give you names at the top level of the list 
#and str() will give you a full description of the entire list (which can sometimes be a little overwhelming).

df= data.frame(
  a=rnorm(10),
  b=rnorm(10),
  c=rnorm(10),
  d=rnorm(10)
  
)

for (i in 1:ncol(df)){
  print (median(df[[i]]))
}

# Replace the 1:ncol(df) sequence
for (i in seq_along(df)) {
  print(median(df[[i]]))
}

# Change the value of df
df = data.frame()

# Repeat for loop to verify there is no error

for (i in seq_along(df)) {
  print(median(df[[i]]))
}


#Before you start the loop, you must always allocate sufficient space for the output, let's say an object called output. 
#This is very important for efficiency: if you grow the for loop at each iteration (e.g. using c()), your for loop will be very slow.

#A general way of creating an empty vector of given length is the vector() function. 
#It has two arguments: the type of the vector ("logical", "integer", "double", "character", etc.) and the length of the vector.

?vector()

# Create new double vector: output
output=vector(mode="double", length=ncol(df))

# Alter the loop
for (i in seq_along(df)) {
  # Change code to store result in output
  output[i]= median(df[[i]])
}

# Print output
output


(df$a - min(df$a, na.rm = TRUE)) /  
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))

# Define example vector x
x<- 1:10

# Define rng
rng = range(x, na.rm=TRUE)
# Rewrite this snippet to refer to the elements of rng
(x - rng[1]) /
  (rng[2] - rng[1])

# Define example vector x
x <- 1:10 

# Use the function template to create the rescale01 function
rescale01<- function(x) {
  rng = range(x, rm.na =TRUE)
  y= (x-rng[1])/(rng[2]-rng[1])
  return (y)
  
}

# Test your function, call rescale01 using the vector x as the argument
rescale01(x)

?arrange()


mean_ci <- function(level, x) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - level
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

alpha=1-0.05
qnorm(c(alpha / 2, 1 - alpha / 2))

?qnorm()

# Alter the arguments to mean_ci
mean_ci <- function(x,level=0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - level
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

# Alter the mean_ci function
mean_ci <- function(x, level = 0.95) {
  if(length(x)!=0) {
    se <- sd(x) / sqrt(length(x))
    alpha <- 1 - level
    mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2)) }
  
  else {
    warning("`x` was empty", call. = FALSE)
    interval <- c(-Inf, Inf)
    interval
  }
}




#It is also bad practice to use cat() for anything other than a print() method 
#(a function designed just to display output). Having an important message 
#just print to the screen makes it very hard for other people who might be 
#programming with your function to capture the output and handle it appropriately.

#The official R way to supply simple diagnostic information is the message() function. 
#The unnamed arguments are pasted together with no separator 
#(and no need for a newline at the end) and by default are printed to the screen.

replace_missings <- function(x, replacement) {
  is_miss <- is.na(x)
  x[is_miss] <- replacement
  
  # Rewrite to use message()
  #cat(sum(is_miss), replacement, "\n")
  message( "sum(is_miss) missings replaced by the value replacement")
  x
}

# Check your new function by running on df$z
df$z = replace_missings(df$z,replacement=0)

#functional programming

# two for loops


library(purrr)
##########################################
#funcitonal programming: functions can be arguments to other funcitons
##########################################

###############################
#this is the one with for loop
###################################
# Initialize output vector

data("mtcars")
df=mtcars
output <- vector("double", ncol(df))  
# Fill in the body of the for loop
for (i in seq_along(df)) {            
  output[i] = median(df[[i]])
}
# View the result
output

class(df[[1]]) ## numeric vector
class(df[1])   ## data frame

#############################################
#wrap in to a function, then several function, then put mean or mean into the arguemnt part
#############################################
# Turn this code into col_median()
col_median = function(df) {
  output <- vector("double", ncol(df))  
  for (i in seq_along(df)) {            
    output[[i]] <- median(df[[i]])      # single bracket for both works too
  }
  output
}

# Create col_mean() function to find column means
col_mean <- function(df) {
  output <- numeric(length(df))
  for (i in seq_along(df)) {
    output[[i]] <- mean(df[[i]])
  }
  output
}

# Define col_sd() function
col_sd = function(df){
  output <- numeric(length(df))
  for (i in seq_along(df)) {
    output[[i]] <- sd(df[[i]])
  }
  output
}

col_summary <- function(df, fun) {
  output <- vector("numeric", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- fun(df[[i]])
  }
  output
}

# Find the column medians using col_median() and col_summary()
col_median(df)
col_summary(df,median)

# Find the column means using col_mean() and col_summary()
col_summary(df,mean)

# Find the column IQRs using col_summary()
col_summary(df,IQR)













library("purrr")
medians= map_dbl(mtcars,median)
medians


?mp_dbl()

#functions can be arguments for other functions
df <- data.frame(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# Initialize output vector
output <- vector("double", ncol(df))  
df

# Fill in the body of the for loop
for (i in seq_along(df)) {            
  output[i] = median(df[[i]])
}
# View the result
output

# Turn this code into col_median()
col_median = function(df) {
  output <- vector("double", ncol(df))  
  for (i in seq_along(df)) {            
    output[[i]] <- median(df[[i]])      
  }
  output
}

# Create col_mean() function to find column means
col_mean <- function(df) {
  output <- numeric(length(df))
  for (i in seq_along(df)) {
    output[[i]] <- mean(df[[i]])
  }
  output
}

# Define col_sd() function
col_sd = function(df){
  output <- numeric(length(df))
  for (i in seq_along(df)) {
    output[[i]] <- sd(df[[i]])
  }
  output
  
  
}

# Add a second argument called power
f <- function(x, power) {
  # Edit the body to return absolute deviations raised to power
  ( abs(x - mean(x)))^power
}

col_summary <- function(df, fun) {
  output <- vector("numeric", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- fun(df[[i]])
  }
  output
}

# Find the column medians using col_median() and col_summary()
col_median(df)
col_summary(df,median)


# Find the column means using col_mean() and col_summary()
col_mean(df)
col_summary(df,mean)


# Find the column IQRs using col_summary()
col_summary(df,IQR)

################
# map_dbl in the purrr package is the same as sapply
# the above col_summary is a homemade version of sapply

#################
#introducing purrr
################
sapply(df,mean)
colsummary(df,mean)
map_dbl(df,mean)

# very map function works the same way
map_dbl(.x, .f, ...)
# loop over a vector .x
# Do something to each element .f
# Return the results

################################################
# the map function differ in their return type
###################################################
# map() returns a list
# map_dbl() returns a double vector
# map_lgl() logical
# map_int() integer
# map_chr() character

df = data.frame(a=1:10, b=11:20)
map(df,mean)  # when x is df, iterate over the column

l= list(a=1:10, b=11:20)
map(l,mean)  # lists, iterte over elements

vec=c(a=1,b=2)
map(vec,mean)  # atomic vector,

#####################
# advantage of map functions in purrr
#####################
# handy shortcuts for specifying .f
# more consistent than sapply() and lapply() functions
# Use map_dbl() to find column means
map_dbl(df,mean)

# Use map_dbl() to column medians
map_dbl(df,median)

# Use map_dbl() to find column standard deviations
map_dbl(df,sd)

#The map functions use the ... ("dot dot dot") argument to pass along additional arguments to .f each time it’s called. 
#For example, we can pass the trim argument to the mean() function:

map_dbl(df, mean, trim = 0.5)
#Multiple arguments can be passed along using commas to separate them. 
#For example, we can also pass the na.rm argument to mean():
map_dbl(df, mean, trim = 0.5, na.rm = TRUE)
  
  map_dbl(df, mean, trim = 0.5, na.rm = TRUE)
  
install.packages("nycflights13")
library(nycflights13)
data(planes)
map_dbl(planes,mean,na.rm=TRUE)


# Find the mean of each column
map_dbl(planes, mean)


# Find the mean of each column, excluding missing values
map_dbl(planes, mean, na.rm=TRUE)

# Find the 5th percentile of each column, excluding missing values
map_dbl(planes, quantile, 0.05, na.rm=TRUE)

# Find the columns that are numeric
#sapply(mtcars,class)
map_lgl(mtcars, is.numeric)

# Find the type of each column
map_chr(mtcars,typeof)

# Find a summary of each column
map(mtcars,summary)


##############
##speifying .f
##############

# define an anonamous function on the fly
#####################################################
#find how many missing values are in each column
####################################################
map(df, function(x) sum(is.na(x )))
map(df, ~ sum(is.na(.)))   # pass a formular, ~ tell us it is a formular


#################
##shortcuts when .f is [[
#################

list_of_results = list( list(a=1,b="A"),
                        list(a=2,b="C"),
                        list(a=3,b="D"))

list_of_results

map_dbl(list_of_results, function(x) x[["a"]])  # subsets all a elemnts
map_dbl(list_of_results, "a")
map_dbl(list_of_results, 1)


###########################
# practice on a list of dat frames, mtcars
###########################
cyl = split(mtcars, mtcars$cyl)
str(cyl)

# Examine the structure of cyl
str(cyl)

# Extract the first element into four_cyls
four_cyls=cyl[[1]]

# Fit a linear regression of mpg on wt using four_cyls
lm(mpg~wt, data = four_cyls)

######################
#make a function fit_reg
fit_reg <- function(df) {
  lm(mpg ~ wt, data = df)
}
#Then pass this function into map():
  
map(cyl, fit_reg)
# Rewrite to call an anonymous function
map(cyl, function (df)  lm (mpg~wt, data=df))


#Writing anonymous functions takes a lot of extra key strokes, so purrr provides a shortcut that allows you to write an anonymous function as a one-sided formula instead.

#In R, a one-sided formula starts with a ~, 
#followed by an R expression. In purrr's map functions, the R expression can refer 
#to an element of the .x argument using the . character.

#Let's take a look at an example. Imagine, instead of a regression on each data frame in cyl, we wanted to know the mean displacement for each data frame.
#One way to do this would be to use an anonymous function:

map_dbl(cyl, function(df) mean(df$disp))

#To perform the same operation using the formula shortcut, 
#we replace the function definition (function(df)) with the ~, 
#then when we need to refer to the element of cyl the function operates on (in this case df), we use a .

map_dbl(cyl, ~mean(.$disp))
# Rewrite to use the formula shortcut instead
#map(cyl, function(df) lm(mpg ~ wt, data = df))

map(cyl, ~ lm(mpg~wt, .))

# Save the result from the previous exercise to the variable models
models=map(cyl, ~ lm(mpg ~ wt, data = .))

# Use map and coef to get the coefficients for each model: coefs
coefs = map(models, "coefficients")

# Use string shortcut to extract the wt coefficient 
map(coefs,"wt")


#Using a numeric vector
#############################
#Another useful shortcut for subetting is to pass a numeric vector as the .f argument. This works just like passing a string but subsets by index rather than name. For example, with your previous list_of_results:
  
  list_of_results <- list(
    list(a = 1, b = "A"), 
    list(a = 2, b = "C"), 
    list(a = 3, b = "D")
  )
#Another way to pull out the a element from each list, is to pull out the first element:
  
  map(list_of_results, 1)
  
  coefs <- map(models, coef)
  
  # use map_dbl with the numeric shortcut to pull out the second element
  map_dbl(coefs, 2)
  
  
  
  ##########################################
  #put it together with pipes
  ##############################################
  
  x %>% f(y) 
  #is another way of writing 
  f(x, y)
  
  cyl <- split(mtcars, mtcars$cyl) 
  map(cyl, ~ lm(mpg ~ wt, data = .))
  ##We split the data frame mtcars and save it as the variable cyl. 
  #We then pass cyl as the first argument to map to fit the models. 
  #We could rewrite this using the pipe operator as:
  split(mtcars, mtcars$cyl) %>% 
    map(~ lm(mpg ~ wt, data = .))
  #We read this as "split the data frame mtcars on cyl, then use map() on the result."
  
  mtcars %>% 
    split(mtcars$cyl) %>%
    map(~ lm(mpg ~ wt, data = .)) %>%
    map(coef) %>% 
    map_dbl("wt")
  
  
  # Define models 
  models <- mtcars %>% 
    split(mtcars$cyl) %>%
    map(~ lm(mpg ~ wt, data = .))
  
  # Rewrite to be a single command using pipes 
  summaries <- map(models, summary)
  #summaries
  map_dbl(summaries, "r.squared")
  
  models %>%
    map(summary) %>%
    map_dbl("r.squared")
  
  
  ####################################
  #introducing safely()
  ######################################
  library(purrr)
  safe_log = safely(log)
  safe_log
  
  safe_log(10)
  safe_log("a")
  
  # safe_log always returns  a list of two components
  # result and error
  
  
#safely() is an adverb; it takes a verb and modifies it. That is, it takes a function as an argument and 
  #it returns a function as its output. The function that is returned is modified 
  #so it never throws an error (and never stops the rest of your computation!).
  
#Instead, it always returns a list with two elements:
    
#result is the original result. If there was an error, this will be NULL.error is an error object. 
  #If the operation was successful this will be NULL.
  # Create safe_readLines() by passing readLines() to safely()
  safe_readLines <- safely(readLines)
  
  # Call safe_readLines() on "http://example.org"
  safe_readLines("http://example.org")
  
  # Call safe_readLines() on "http://asdfasdasdkfjlda"
  safe_readLines("http://asdfasdasdkfjlda")
  
  urls <- list(
    example = "http://example.org",
    rproj = "http://www.r-project.org",
    asdf = "http://asdfasdasdkfjlda"
  )
  
  # Define safe_readLines()
  safe_readLines <- safely(readLines)
  
  # Use the safe_readLines() function with map(): html
  html=map(urls, safe_readLines)
  
  # Call str() on html
  str(html)
  
  # Extract the result from one of the successful elements
  html[[1]][[1]]
  
  # Extract the error from the element that was unsuccessful
  html[[3]][[2]]
  
  
  #purrr provides a function transpose() that reshapes a list so the inner-most level becomes the outer-most level.
  #In otherwords, it turns a list-of-lists "inside-out". 
  #Consider the following list:
  
  nested_list <- list(
    x1 = list(a = 1, b = 2),
    x2 = list(a = 3, b = 4)
  )
  
  #If I need to extract the a element in x1, I could do nested_list[["x1"]][["a"]]. 
  #However, if I transpose the list first, the order of subsetting reverses. 
  #That is, to extract the same element I could also do transpose(nested_list)[["a"]][["x1"]]

  # Initialize some objects
  safe_readLines <- safely(readLines)
  html <- map(urls, safe_readLines)
  res <- transpose(html)[["result"]]
  errs <- transpose(html)[["error"]]
  
  # Create a logical vector is_ok
  is_ok = map_lgl(transpose(html)[["error"]], is_null)
  
  
  # Extract the successful results
  res[is_ok]
  
  
  # Extract the input from the unsuccessful results
  urls[!is_ok]
  
  
######################
# drawing samples from a Norm
########################
rnorm(5)
rnorm(10)
rnorm(20)
rnorm(n,mean=0, sd=1)

map(list(5,10,20),rnorm)


###########
#if the mean is different, then we use map2

rnorm(5,mean=1)
rnorm(10,mean=5)
rnorm(20,mean=10)
map2(list(5,10,20), list(1,5,10), rnorm)


#####################################
# there is no map3, rather there is pmap()

#pmap() to iterate over many arguments
rnorm(5,mean=1,sd=0.1)
rnorm(10,mean=5,sd=0.5)
rnorm(20, mean=10, sd=0.1)
pmap(list(n=list(5,10,20),
          mean=list(1,5,20),
          sd=list(0.1,0.5,0.1)), rnorm)


#######################################
#invoke_map() to iterate over functions
#########################################
rnorm(5)
runif(5)
rexp(5)

invoke_map(list(rnorm, runif, rexp), n=5)


##################################
#summary mapping over many arguments
#######################################
#map2() iterate over two arguments
#pmap() iterate over many arguments
#invoke_map() iterate over functions and arguments
# like map(), each was a whole family of functions:
#map2_dbl(), map2_lgl(), pmap_dbl()

# Create a list n containing the values: 5, 10, and 20
n = list(5,10,20)

# Call map() on n with rnorm() to simulate three samples
map(n, rnorm)

# Initialize n
n <- list(5, 10, 20)

# Create a list mu containing the values: 1, 5, and 10
mu= list(1,5,10)

# Edit to call map2() on n and mu with rnorm() to simulate three samples
map2(n, mu,rnorm)

# Initialize n and mu
n <- list(5, 10, 20)
mu <- list(1, 5, 10)
# Create a sd list with the values: 0.1, 1 and 0.1
sd = list(0.1,1,0.1)
# Edit this call to pmap() to iterate over the sd list as well
pmap(list(n, mu,sd), rnorm)

# Name the elements of the argument list
pmap(list(mean=mu, n=n, sd=sd), rnorm)

# Name the elements of the argument list
# n =5 is global argument
pmap(list(mean=mu, n=n, sd=sd), rnorm)

library(purrr)
#########################
#introducing walk()
############################
x=list(1,"a",3)
x%>% walk(print)

# Define list of functions
f <- list(Normal = "rnorm", Uniform = "runif", Exp = "rexp")

# Define params
params <- list(
  Normal = list(mean = 10),
  Uniform = list(min = 0, max = 5),
  Exp = list(rate = 5)
)

# Assign the simulated samples to sims
sims=invoke_map(f, params, n = 50)

# Use walk() to make a histogram of each element in sims
#walk(print, hist(sims))
sims%>% walk(print)

# Replace "Sturges" with reasonable breaks for each sample
breaks_list <- list(
  Normal = seq(6, 16, 0.5),
  Uniform = seq(0, 5, 0.25),
  Exp = seq(0, 1.5, 0.1)
)

# Use walk2() to make histograms with the right breaks
walk2(sims, breaks_list, hist)

# Turn this snippet into find_breaks()

rng <- range(sims[[1]], na.rm = TRUE)
seq(rng[1], rng[2], length.out = 30)

find_breaks = function(x){
  rng = range(x, na.rm=TRUE)
  seq(rng[1],rng[2], length.out=30)
}

# Call find_breaks() on sims[[1]]
find_breaks(sims[[1]])

# Increase sample size to 1000
sims <- invoke_map(f, params, n = 1000)

# Compute nice_breaks (don't change this)
nice_breaks <- map(sims, find_breaks)

# Create a vector nice_titles
nice_titles = c("Normal(10, 1)","Uniform(0, 5)","Exp(5)")

# Use pwalk() instead of walk2()
walk2(sims, nice_breaks, hist, xlab = "")

pwalk(list(x=sims, breaks=nice_breaks, main=nice_titles),hist,xlab="")



#############################
#Robust functions
##########################
x=1:10
stopifnot(is.character(x))

if(condition){
  stop("Error",call. = FALSE)
}


if(!is.character(x)){
  stop("x should be a charactor vector", call. = FALSE)
}

# Define troublesome x and y
x <- c(NA, NA, NA)
y <- c( 1, NA, NA, NA)

both_na <- function(x, y) {
  # Add stopifnot() to check length of x and y
  stopifnot(length(x)==length(y))
  sum(is.na(x) & is.na(y))
}

# Call both_na() on x and y
both_na(x, y)


# Define troublesome x and y
x <- c(NA, NA, NA)
y <- c( 1, NA, NA, NA)

both_na <- function(x, y) {
  # Replace condition with logical
  if (length(x)!=length(y)) {
    # Replace "Error" with better message
    stop("x and y must have the same length", call. = FALSE)
  }  
  
  sum(is.na(x) & is.na(y))
}

# Call both_na() 
both_na(x, y)


last_row=function(df){
  df[nrow(df),, drop=FALSE]
}

df = data.frame(x=1:3)

str(last_row(df))

df <- data.frame(
  a = 1L,
  b = 1.5,
  y = Sys.time(),
  z = ordered(1)
)

A <- sapply(df[1:4], class) 
B <- sapply(df[3:4], class)
A
B

#ou shouldn't rely on sapply() inside your own functions.

#So, what do you do? Use alternate functions that are type consistent! And you already know a whole set: 
#the map() functions in purrr
library(purrr)
df <- data.frame(
  a = 1L,
  b = 1.5,
  y = Sys.time(),
  z = ordered(1)
)

A <- map_chr(df[1:4], class) 
B <- map_chr(df[3:4], class)
#gives us errors. This is a good thing! It alerts us that our assumption (that class() 
#would return purely character output) is wrong.


# sapply calls
A <- sapply(df[1:4], class) 
B <- sapply(df[3:4], class)
C <- sapply(df[1:2], class) 

# Demonstrate type inconsistency
str(A)
str(B)
str(C)

# Use map() to define X, Y and Z
X <- map(df[1:4], class) 
Y <- map(df[3:4], class)
Z <- map(df[1:2], class) 




# Use str() to check type consistency
str(X)
str(Y)
str(Z)

col_classes <- function(df) {
  # Assign list output to class_list
  class_list= map(df, class)
  map_chr(class_list,1)
  # Use map_chr() to extract first element in class_list
}

# Check that our new function is type consistent
df %>% col_classes() %>% str()
df[3:4] %>% col_classes() %>% str()
df[1:2] %>% col_classes() %>% str()

getwd()
data(iris)
write.csv(iris, "iris.csv")

data(mtcars)
dim(mtcars)
names(mtcars)
write.csv(mtcars, "mtcars.csv")
data("mpg")
dim(mpg)
names(mpg)
write.csv(mpg, "mpg.csv")

library(dplyr)
library(reshape2)
data(tips)
dim(tips)
write.csv(tips, "tips.csv")


data("Titanic")
write.csv(Titanic, "Titannic.csv")

data(Gapminder)

options()
getOption("digits")
options(digits=6)
getOption("digits")


# Fit a regression model
fit <- lm(mpg ~ wt, data = mtcars)

# Look at the summary of the model
summary(fit)


# Set the global digits option to 2
options(digits=2)

# Take another look at the summary
summary(fit)

options(digits=7)
