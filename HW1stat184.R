# PROBLEM 1 SETUP

student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)


# PART A: Building Data Structures

# Convert section to a factor with levels A and B
section <- factor(section, levels = c("A", "B"))

# Create the students data frame
students <- data.frame(
  student_id = student_id,
  section = section,
  quiz1 = quiz1,
  quiz2 = quiz2,
  passed = passed
)

# Create score_matrix
score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")

# Create course_record
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)

# Inspect the objects
typeof(section)
class(section)
length(section)

typeof(students)
class(students)
str(students)
dim(students)

typeof(score_matrix)
class(score_matrix)
str(score_matrix)
dim(score_matrix)

typeof(course_record)
class(course_record)
length(course_record)
str(course_record)

# A vector stores elements of the same basic type.
# A list can contain objects of different types and structures.
# A matrix is a two-dimensional structure whose elements have the same type.
# A factor represents categorical data, while a data frame stores tabular data and can contain columns of different types.


# PART B: Subsetting

# Extract S04's second quiz score
score_matrix["S04", "quiz2"]

# Output:
# [1] 92


# Extract the first two rows while preserving matrix dimensions
score_matrix[1:2, , drop = FALSE]

# Output:
#     quiz1 quiz2
# S01    82    85
# S02    91    89


# Extract course using [
course_record["course"]

# Extract course using [[
course_record[["course"]]

# Extract course using $
course_record$course


# [ returns a subset while preserving the container structure.
# [[ extracts a single element from a list.
# $ extracts a named element from a list or data frame.

# PART C: Vectorized Calculations

# Add the row-wise mean of quiz1 and quiz2
students$average <- rowMeans(students[, c("quiz1", "quiz2")])

# Add excellent
students$excellent <- students$average >= 90

# Select section A students whose average is at least 80
section_A_80 <- students[
  students$section == "A" & students$average >= 80,
  c("student_id", "section", "average")
]

section_A_80

# Output:
#   student_id section average
# 1        S01       A    83.5
# 5        S05       A    94.5


# Create named numeric vector of all student averages
student_averages <- students$average
names(student_averages) <- students$student_id

student_averages

# Output:
#  S01  S02  S03  S04  S05  S06
# 83.5 90.0 78.0 90.0 94.5 71.5


# These calculations are vectorized because operations are applied to
# entire vectors or rows at once rather than processing each student with a loop.


# PROBLEM 2 SETUP

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"


# PART A: Importing Data

# Import the data and treat blanks and "NA" as missing
measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA")
)

# Inspect the data
head(measurements)
str(measurements)
dim(measurements)
names(measurements)


# Count missing values in each column
colSums(is.na(measurements))

# Output:
# sample_id site temp_c ph status
#         0    0      1  2      0


# Keep only complete observations
measurements_complete <- measurements[complete.cases(measurements), ]

measurements_complete


# Report sample IDs removed by complete-case filtering
removed_ids <- measurements$sample_id[!complete.cases(measurements)]
removed_ids

# Output:
# [1] "M02" "M03" "M06"


# x == NA is not a valid missing-value test because comparisons with NA
# return NA rather than TRUE or FALSE. Use is.na(x) to test for missing values.


# PART B: Transform, Filter, and Summarize

# Convert site and status to factors
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

# Report their levels
levels(measurements$site)
# [1] "East" "North" "South"

levels(measurements$status)
# [1] "ok" "review"


# Convert Celsius to Fahrenheit
measurements$temp_f <- measurements$temp_c * 9 / 5 + 32


# Create ph_below_7 while preserving missing values
measurements$ph_below_7 <- measurements$ph < 7

measurements$ph_below_7

# Output:
# [1] FALSE    NA  TRUE FALSE FALSE    NA  TRUE FALSE


# Select complete observations from North or South with status "ok"
selected_measurements <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
  c("sample_id", "site", "temp_c", "temp_f", "ph")
]

selected_measurements

# Output:
#   sample_id  site temp_c temp_f  ph
# 1       M01 North   18.2  64.76 7.1
# 7       M07 North   17.8  64.04 6.9
# 8       M08 South   21.2  70.16 7.2


# Overall mean Celsius temperature, ignoring missing values
overall_mean <- mean(measurements$temp_c, na.rm = TRUE)
overall_mean

# Output:
# [1] 20.35714


# South-site mean temperature, ignoring missing values
south_mean <- mean(
  measurements$temp_c[measurements$site == "South"],
  na.rm = TRUE
)

south_mean

# Output:
# [1] 20.46667


# PART C: Matrix Operations

A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

A
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

B
#      [,1] [,2]
# [1,]    5    7
# [2,]    6    8


# Element-wise multiplication
element_result <- A * B
element_result

# Output:
#      [,1] [,2]
# [1,]    5   21
# [2,]   12   32


# Matrix multiplication
matrix_result <- A %*% B
matrix_result

# Output:
#      [,1] [,2]
# [1,]   23   31
# [2,]   34   46


# Dimensions
dim(element_result)
# [1] 2 2

dim(matrix_result)
# [1] 2 2


# A * B multiplies corresponding elements of A and B.
# A %*% B performs matrix multiplication using row-by-column products.


# PROBLEM 3 SETUP

student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)


# PART A: Write a Grading Function

grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}


# Test the function

grade_one(NA)
# [1] NA

grade_one(90)
# [1] "A"

grade_one(80)
# [1] "B"

grade_one(85)
# [1] "B"

grade_one(74)
# [1] "C"


# PART B: Applying Functions with Loop

grades <- rep(NA_character_, length(scores))

for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id

grades

# Output:
# P01 P02 P03 P04 P05 P06 P07 P08
# "A" "B"  NA  "D" "C" "B" "F" "A"


# During each iteration, i represents the position of the current score in the scores vector.


# PART C-1: summarize_scores Function

summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  
  total_count <- length(x)
  missing_count <- sum(is.na(x))
  
  mean_score <- round(mean(x, na.rm = na.rm), digits)
  sd_score <- round(sd(x, na.rm = na.rm), digits)
  min_score <- round(min(x, na.rm = na.rm), digits)
  max_score <- round(max(x, na.rm = na.rm), digits)
  
  c(
    total = total_count,
    missing = missing_count,
    mean = mean_score,
    sd = sd_score,
    min = min_score,
    max = max_score
  )
}


# Call with defaults
summarize_scores(scores)

# Output:
#   total missing    mean      sd     min     max
#     8.0     1.0    79.4    13.3    59.0    95.0


# Call with named arguments as requested
summarize_scores(x = scores, na.rm = TRUE, digits = 2)

# Output:
#   total missing    mean      sd     min     max
#    8.00    1.00   79.43   13.28   59.00   95.00



# PART C-2: plot_scores Function

plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}


# Test the function
plot_scores(
  scores,
  type = "b",
  pch = 19,
  xlab = "Position",
  ylab = "Score",
  main = "Student Scores"
)

# The function plots each score against its position in the vector.
# The ... argument passes additional graphical arguments directly to plot().
# The missing score at position 3 will not have a plotted point.