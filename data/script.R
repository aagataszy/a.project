read.csv(file = "data/inflammation-01.csv", header = FALSE)
weight_kg <- 55
2.2 * weight_kg
dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
head(dat)

class(dat)
dim(dat)
dat[1, 1]
dat[30, 20]
dat[c(1, 3, 5), c(10, 20)]
1:5
3:12
dat[1:4, 1:10]
dat[5:10, 1:10]
dat[5, ]
dat[, 16:18]
patient_1 <- dat[1, ]
max(patient_1)
max(dat[2, ])
min(dat[, 7])
mean(dat[, 7])
median(dat[, 7])
sd(dat[, 7])
summary(dat[, 1:4])
avg_patient_inflammation <- apply(dat, 1, mean)
avg_day_inflammation <- apply(dat, 2, mean)
plot(avg_day_inflammation)
max_day_inflammation <- apply(dat, 2, max)
plot(max_day_inflammation)
min_day_inflammation <- apply(dat, 2, min)
plot(min_day_inflammation)
animal <- c("m", "o", "n", "k", "e", "y")
animal[1:3]
animal[4:6]
animal[4:1]
animal[1:6]
whichPatients <- seq(2, 60, 2)
whichDays <- seq(1, 5)
dat2 <- dat
dim(dat2[whichPatients, whichDays])
dat2[whichPatients, whichDays] <- dat2[whichPatients, whichDays] / 2
dat2

dat [seq(2, 60,2), 1:5]

# 1.
apply(dat[1:5, ], 1, mean)
# 2.
apply(dat[, 1:10], 2, mean)
# 3.
apply(dat[, seq(2, 40, by = 2)], 2, mean)

sd_day_inflammation <- apply(dat, 2, sd)
plot(sd_day_inflammation)

fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}
# freezing point of water
fahrenheit_to_celsius(32)
# boiling point of water
fahrenheit_to_celsius(212)
celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}

# freezing point of water in Kelvin
celsius_to_kelvin(0)

# freezing point of water in Kelvin
fahrenheit_to_kelvin(32.0)
fahrenheit_to_kelvin <- function(temp_F) {
  temp_C <- fahrenheit_to_celsius(temp_F)
  temp_K <- celsius_to_kelvin(temp_C)
  return(temp_K)

}
  

fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

# freezing point of water
fahrenheit_to_celsius(32)  

# boiling point of water
fahrenheit_to_celsius(212)

celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
  
}
# freezing point of water in Kelvin
celsius_to_kelvin(0)

fahrenheit_to_kelvin <- function(temp_F) {
  temp_C <- fahrenheit_to_celsius(temp_F)
  temp_K <- celsius_to_kelvin(temp_C)
  return(temp_K)
}
# freezing point of water in Kelvin
fahrenheit_to_kelvin(32.0)

# freezing point of water in Fahrenheit
celsius_to_kelvin(fahrenheit_to_celsius(32.0))

best_practice <- c("Write", "programs", "for", "people", "not", "computers")
asterisk <- "***"  # R interprets a variable with a single value as a vector
# with one element.
highlight(best_practice, asterisk)
highlight <- function(content, wrapper) {
  answer <- c(wrapper, content, wrapper)
  return(answer)
}

edges <- function(v) {
  first <- v[1]
  last <- v[length(v)]
  answer <- c(first, last)
  return(answer)
}
dry_principle <- c("Don't", "repeat", "yourself", "or", "others")
edges (dry_principle)

highlight <- function(content, wrapper) {
  answer <- c(wrapper, content, wrapper)
  return(answer)
}
best_practice <- c("Write", "programs", "for", "people", "not", "computers")
asterisk <- "***"  # R interprets a variable with a single value as a vector
# with one element.
highlight(best_practice, asterisk)

edges <- function(v) {
  first <- v[1]
  last <- v[length(v)]
  answer <- c(first, last)
  return(answer)
}  
dry_principle <- c("Don't", "repeat", "yourself", "or", "others")
edges(dry_principle)

center <- function(data, midpoint) {
  new_data <- (data - mean(data)) + midpoint
  return(new_data)
}
z <- c(0, 0, 0, 0)
z
center(z, 3)

dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
centered <- center(dat[, 4], 0)
head(centered)
# original mean
mean(dat[, 4])
# centered mean
mean(centered)
# original standard deviation
sd(dat[, 4])
# centered standard deviation
sd(centered)
# difference in standard deviations before and after
sd(dat[, 4]) - sd(centered)
all.equal(sd(dat[, 4]), sd(centered))

# new data object and set one value in column 4 to NA
datNA <- dat
datNA[10,4] <- NA

# returns all NA values
center(datNA[,4], 0)

center <- function(data, midpoint = 0) {
  new_data <- (data - mean(data)) + midpoint
  return(new_data)
}

analyze <- function(filename) {
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}

rescale <- function(v) {
  L <- min(v)
  H <- max(v)
  result <- (v - L) / (H - L)
  return(result)
}
dat <- read.csv("data/inflammation-01.csv", FALSE)
dat <- read.csv(header = FALSE, file = "data/inflammation-01.csv")
dat <- read.csv(FALSE, "data/inflammation-01.csv")
test_data <- c(0, 0, 0, 0)
center(test_data, 3)
more_data <- 5 + test_data
more_data
center(more_data)

display <- function(a = 1, b = 2, c = 3) {
  result <- c(a, b, c)
  names(result) <- c("a", "b", "c")
  return(result)
}
# no arguments
display()

# one argument
display(55)

# two arguments
display(55, 66)

# three arguments
display(55, 66, 77)

# only setting the value of c
display(c = 77)

?read.csv
read.csv(file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", ...)
dat <- read.csv(FALSE, "data/inflammation-01.csv")         

rescale <- function(v, lower = 0, upper = 1) {
  # Rescales a vector, v, to lie in the range lower to upper.
  L <- min(v)
  H <- max(v)
  result <- (v - L) / (H - L) * (upper - lower) + lower
  return(result)
}
seq(3)
print_N(3)

analyze <- function(filename) {
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}
analyze("data/inflammation-01.csv")
analyze("data/inflammation-02.csv")
best_practice <- c("Let", "the", "computer", "do", "the", "work")
print_words <- function(sentence) {
  print(sentence[1])
  print(sentence[2])
  print(sentence[3])
  print(sentence[4])
  print(sentence[5])
  print(sentence[6])
}
print_words(best_practice)

best_practice[-6]
print_words(best_practice[-6])
print_words <- function(sentence) {
  for (word in sentence) {
    print(word)
  }
}
print_words(best_practice)
print_words(best_practice[-6])

len <- 0
vowels <- c("a", "e", "i", "o", "u")
for (v in vowels) {
  len <- len + 1
}
len

letter <- "z"
for (letter in c("a", "b", "c")) {
  print(letter)
}
letter
length(vowels)

seq(3)
print_N(3)

ex_vec <- c(4, 8, 15, 16, 23, 42)
total(ex_vec)

expo <- function(base, power) {
  result <- 1
  for (i in seq(power)) {
    result <- result * base
  }
  return(result)
}

list.files(path = "data", pattern = "csv")
list.files(path = "data", pattern = "inflammation")
list.files(path = "data", pattern = "csv", full.names = TRUE)
list.files(path = "data", pattern = "inflammation", full.names = TRUE)

filenames <- list.files(path = "data",
                        pattern = "inflammation-[0-9]{2}.csv",
                        full.names = TRUE)
filenames <- filenames[1:3]
for (f in filenames) {
  print(f)   
  analyze(f)
}

analyze_all <- function(folder = "data", pattern) {
  filenames <- list.files(path = folder, pattern = pattern, full.names = TRUE)
  for (f in filenames) {
    analyze(f)
  }
}

analyze <- function(filename) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}

analyze_all <- function(folder = "data", pattern) {
  # Runs the function analyze for each file in the given folder
  # that contains the given pattern.
  filenames <- list.files(path = folder, pattern = pattern, full.names = TRUE)
  for (f in filenames) {
    analyze(f)
  }
}

pdf("inflammation-01.pdf")
analyze("data/inflammation-01.csv")
dev.off()

num <- 37
num > 100
num < 100

num <- 37
if (num > 100) {
  print("greater")
} else {
  print("not greater")
}
print("done")

num <- 53
if (num > 100) {
  print("num is greater than 100")
}
sign <- function(num) {
  if (num > 0) {
    return(1)
  } else if (num == 0) {
    return(0)
  } else {
    return(-1)
  }
}
sign(-3)
sign(0)
sign(2/3)

if (1 > 0 && -1 > 0) {
  print("both parts are true")
} else {
  print("at least one part is not true")
}
if (1 > 0 || -1 > 0) {
  print("at least one part is true")
} else {
  print("neither part is true")
}

dat <- read.csv("data/inflammation-01.csv", header = FALSE)
plot_dist(dat[, 10], threshold = 10)
plot_dist(dat[1:5, 10], threshold = 10)  # samples (rows) 1-5 on day (column) 10
plot_dist <- function(x, threshold) {
  if (length(x) > threshold) {
    boxplot(x)
  } else {
    stripchart(x)
  }
}

dat <- read.csv("data/inflammation-01.csv", header = FALSE)
plot_dist(dat[, 10], threshold = 10, use_boxplot = TRUE)   # day (column) 10 - create boxplot
plot_dist(dat[, 10], threshold = 10, use_boxplot = FALSE)  # day (column) 10 - create histogram

filenames <- list.files(path = "data", pattern = "inflammation-[0-9]{2}.csv", full.names = TRUE)
filename_max <- "" # filename where the maximum average inflammation patient is found
patient_max <- 0 # index (row number) for this patient in this file
average_inf_max <- 0 # value of the average inflammation score for this patient
for (f in filenames) {
  dat <- read.csv(file = f, header = FALSE)
  dat.means <- apply(dat, 1, mean)
  for (patient_index in 1:length(dat.means)){
    patient_average_inf <- dat.means[patient_index]
    # Add your code here ...
    if (patient_average_inf > average_inf_max) {
      average_inf_max <- patient_average_inf
      filename_max <- f
      patient_max <- patient_index
    }
  }
}
print(filename_max)
print(patient_max)
print(average_inf_max)

analyze <- function(filename, output = NULL) {
  # Plots the average, min, and max inflammation over time.
  # Input:
  #    filename: character string of a csv file
  #    output: character string of pdf file for saving
  if (!is.null(output)) {
    pdf(output)
  }
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
  if (!is.null(output)) {
    dev.off()
  }
}
output <- NULL
is.null(output)    
!is.null(output)

analyze("data/inflammation-01.csv")
analyze("data/inflammation-01.csv", output = "inflammation-01.pdf")
dir.create("results")
analyze("data/inflammation-01.csv", output = "results/inflammation-01.pdf")
f <- "inflammation-01.csv"
sub("csv", "pdf", f)
file.path("results", sub("csv", "pdf", f))

analyze_all <- function(pattern) {
  # Directory name containing the data
  data_dir <- "data"
  # Directory name for results
  results_dir <- "results"
  # Runs the function analyze for each file in the current working directory
  # that contains the given pattern.
  filenames <- list.files(path = data_dir, pattern = pattern)
  for (f in filenames) {
    pdf_name <- file.path(results_dir, sub("csv", "pdf", f))
    analyze(file.path(data_dir, f), output = pdf_name)
  }
  analyze_all("inflammation.*csv")  

  analyze <- function(filename, output = NULL) {
    # Plots the average, min, and max inflammation over time.
    # Input:
    #    filename: character string of a csv file
    #    output: character string of pdf file for saving
    if (!is.null(output)) {
      pdf(output)
    }
    dat <- read.csv(file = filename, header = FALSE)
    avg_day_inflammation <- apply(dat, 2, mean)
    plot(avg_day_inflammation, type = "l")
    max_day_inflammation <- apply(dat, 2, max)
    plot(max_day_inflammation, type = "l")
    min_day_inflammation <- apply(dat, 2, min)
    plot(min_day_inflammation, type = "l")
    if (!is.null(output)) {
      dev.off()
    }
  }  
  install.packages("quarto")
  quarto::quarto_render("hello.qmd")

  install.packages('tinytex')
  tinytex::install_tinytex()
  # to uninstall TinyTeX, run tinytex::uninstall_tinytex()   
  # writeLines(c(
  #   '\\documentclass{article}',
  #   '\\begin{document}', 'Hello world!', '\\end{document}'
  # ), 'test.tex')
  tinytex::pdflatex('test.tex')  
  
  $ cd
  $ cd Desktop
  