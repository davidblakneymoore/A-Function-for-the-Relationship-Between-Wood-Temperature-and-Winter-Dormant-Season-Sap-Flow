
# Fitting a Vertical Line to Sap Flow and Wood Temperature Data

# David Moore
# University of New Hampshire Ecohydrology Lab
# May 2023
# davidblakneymoore@gmail.com


# The Explanation

# There are situations where data points, when plotted, cluster around the
# horizontal axis except when they're near a particular value along that axis.
# A picture will help describe this situation.

Predictor_Variable <- c(rnorm(1000, 0, 25), rnorm(1000, 20, 2), rnorm(100, 20, 10))
Response_Variable <- c(rnorm(1000, 0, 2), rnorm(1000, 0, 25), rnorm(100, 0, 10))
Data_Frame <- data.frame(Predictor_Variable = Predictor_Variable, Response_Variable = Response_Variable)
plot(Response_Variable ~ Predictor_Variable, Data_Frame, main = 'Example Plot', xlab = 'Wood Temperature', ylab = 'Sap Flow')
abline(h = 0, col = 4)

# I included this figure in this repository - you can see it if you click on
# 'Example Plot 1'.

# This made-up data mimics what I've seen in my studies on winter-dormant-
# season sap flow, where wood temperature is the predictor variable and sap
# flow is the response variable. At extreme wood temperature values, there is
# essentially no sap flow, but when wood temperature values approach a
# particular value (which is often species-specific), sap flows either upward
# or downward in deciduous hardwoods.

# In these scenarios, the value of the predictor variable at which the response
# variable is most variable is unknown. This value, which I'll call the
# 'critical value', must be identified and likely has physiological
# significance for the tree. Here, I propose a method to estimate these
# critical values.

# To estimate a critical value, I propose to split the plane of the plot up
# into four quadrants. The quadrants will come together at a point on the
# horizontal axis somewhere between the minimum and maximum values of the
# predictor variable.  These quadrants will be delineated by two lines that
# cross on the horizontal axis and that have slopes of equal magnitude but of
# different signs (in other words, the lines that delineate the quadrants are
# reflections of each other about the vertical line that passes through the
# point where the lines delineating the quadrant cross). Again, a figure will
# help to illustrate this concept.

Horizontal_Axis_Intercept_Interval <- 0.1
Slope_Interval <- 0.01
Angles <- seq((0 + Slope_Interval), ((pi / 2) - Slope_Interval), Slope_Interval)
Horizontal_Axis_Intercepts <- seq(min(Data_Frame$Predictor_Variable), max(Data_Frame$Predictor_Variable), by = Horizontal_Axis_Intercept_Interval)
i <- sample(seq_len(length((Horizontal_Axis_Intercepts))), 1)
j <- sample(seq_len(length(Angles)), 1)
Bottommost_Points <- ifelse(Data_Frame$Response_Variable < (tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))) & Data_Frame$Response_Variable < -(tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))), T, F)
Leftmost_Regions <- ifelse(Data_Frame$Response_Variable > (tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))) & Data_Frame$Response_Variable < -(tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))), T, F)
Topmost_Points <- ifelse(Data_Frame$Response_Variable > (tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))) & Data_Frame$Response_Variable > -(tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))), T, F)
Rightmost_Points <- ifelse(Data_Frame$Response_Variable < (tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))) & Data_Frame$Response_Variable > -(tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))), T, F)
Bottommost_Points <- Data_Frame[Bottommost_Points, ]
Leftmost_Regions <- Data_Frame[Leftmost_Regions, ]
Topmost_Points <- Data_Frame[Topmost_Points, ]
Rightmost_Points <- Data_Frame[Rightmost_Points, ]
plot(Response_Variable ~ Predictor_Variable, Data_Frame, main = 'Example Plot', xlab = 'Wood Temperature', ylab = 'Sap Flow')
points(Bottommost_Points$Predictor_Variable, Bottommost_Points$Response_Variable, col = 2)
points(Leftmost_Regions$Predictor_Variable, Leftmost_Regions$Response_Variable, col = 3)
points(Topmost_Points$Predictor_Variable, Topmost_Points$Response_Variable, col = 4)
points(Rightmost_Points$Predictor_Variable, Rightmost_Points$Response_Variable, col = 5)
abline(a = (-Horizontal_Axis_Intercepts[i] * tan(Angles[j])), b = (tan(Angles[j])))
abline(a = -(-Horizontal_Axis_Intercepts[i] * tan(Angles[j])), b = -(tan(Angles[j])))
abline(h = 0)

# To find the critical value, two processes will happen. First, the lines
# delineating the quadrants will start at the leftmost data point and
# incrementally move rightward along the horizontal axis. Second, at each of
# the horizontal axis intercepts where these two lines meet, the angles of
# these lines will vary incrementally from 0 to pi / 2. For each combination of
# horizontal axis intercept and angle, a least-squares value will be
# calculated. This least-squares value will be the sum of the squared residuals
# from the topmost and bottommost quadrants (these residuals are the difference
# between the data points in these two quadrants and the vertical line that
# passes through the point where the lines delineating the quadrants cross)
# taken together and from the leftmost and rightmost quadrants (these residuals
# are the difference between the data points in these two quadrants and the
# horizontal axis) taken together. The critical point will be the value of the
# predictor variable that generates the minimum least-squares value.

# This function takes five arguments. The first two are required.

# The 'Predictor_Variable' argument specifies which variable will be treated as
# the variable on the horizontal axis. If the 'Data_Frame' argument is used to
# specify a data frame containing the predictor variable column, the
# 'Predictor_Variable' argument should not be written as a character string (it
# should not be surrounded by quotation marks). If the 'Data_Frame' argument is
# not used, the 'Predictor_Variable' argument should  be written as a character
# string, surrounded by quotation marks. This argument must be numeric and of
# the same length as the 'Response_Variable' argument.

# The 'Response_Variable' argument specifies which variable will be treated as
# the variable on the vertical axis. If the 'Data_Frame' argument is used to
# specify a data frame containing the response variable column, the
# 'Response_Variable' argument should not be written as a character string (it
# should not be surrounded by quotation marks). If the 'Data_Frame' argument is
# not used, the 'Response_Variable' argument should  be written as a character
# string, surrounded by quotation marks. This argument must be numeric and of
# the same length as the 'Predictor_Variable' argument.

# The 'Data_Frame' argument is optional and can be used to identify an object
# of class 'data.frame' that contains both the predictor and response
# variables.

# The 'Slope_Interval' argument specifies, in radians, how gradually slopes
# (angles) should increase from 0 to pi / 2. Smaller 'Slope_Interval' values
# will cause the function to take longer to run but will lead to more accurate
# critical values. This argument must be numeric, of length 1, and between 0
# and pi / 2.

# The 'Horizontal_Axis_Intercept_Interval' argument specifies, in whatever
# units the predictor variable is in, how gradually the quadrants shift from
# right to left. Smaller 'Horizontal_Axis_Intercept_Interval' values will cause
# the function to take longer to run but will lead to more accurate critical
# values. This argument must be numeric, of length 1, and between 0
# and the domain of the predictor variable.


# The Function

Function_for_Finding_the_Critical_Point <- function (Predictor_Variable, Response_Variable, Data_Frame, Slope_Interval = 0.01, Horizontal_Axis_Intercept_Interval = 0.1) {
  if (is.null(Data_Frame)) {
    if (!is.numeric(Predictor_Variable) | !is.numeric(Response_Variable)) {
      stop ("'Predictor_Variable' and 'Response_Variable' must be numeric variables.")
    }
    if (length(Predictor_Variable) != length(Response_Variable)) {
      stop ("'Predictor_Variable' and 'Response_Variable' must be of the same length.")
    }
    Predictor_Variable_Name <- deparse(substitute(Predictor_Variable))
    Response_Variable_Name <- deparse(substitute(Response_Variable))
    Data_Frame <- data.frame(Predictor_Variable = Predictor_Variable, Response_Variable = Response_Variable)
  } else if (!is.null(Data_Frame)) {
    if (class(Data_Frame) != 'data.frame') {
      stop ("'Data_Frame' must be of class 'data.frame'.")
    }
    Predictor_Variable_Name <- deparse(substitute(Predictor_Variable))
    Response_Variable_Name <- deparse(substitute(Response_Variable))
    Predictor_Variable <- Data_Frame[, deparse(substitute(Predictor_Variable))]
    Response_Variable <- Data_Frame[, deparse(substitute(Response_Variable))]
    if (!is.numeric(Predictor_Variable) | !is.numeric(Response_Variable)) {
      stop ("'Predictor_Variable' and 'Response_Variable' must be numeric variables.")
    }
    if (length(Predictor_Variable) != length(Response_Variable)) {
      stop ("'Predictor_Variable' and 'Response_Variable' must be of the same length.")
    }
    Data_Frame <- data.frame(Predictor_Variable = Predictor_Variable, Response_Variable = Response_Variable)
  }
  if (!is.numeric(Horizontal_Axis_Intercept_Interval)) {
    stop ("'Horizontal_Axis_Intercept_Interval' must be numeric.")
  }
  if (length(Horizontal_Axis_Intercept_Interval) != 1) {
    stop ("'Horizontal_Axis_Intercept_Interval' must be a single value.")
  }
  if (Horizontal_Axis_Intercept_Interval <= 0 | Horizontal_Axis_Intercept_Interval >= (max(Data_Frame$Predictor_Variable) - min(Data_Frame$Predictor_Variable))) {
    stop ("'Intercept_Interval' must be greater than 0 and smaller than the difference between the maximum and minimum values of the predictor variable.")
  }
  if (!is.numeric(Slope_Interval)) {
    stop ("'Slope_Interval' must be numeric.")
  }
  if (length(Slope_Interval) != 1) {
    stop ("'Slope_Interval' must be a single value.")
  }
  if (Slope_Interval <= 0 | Slope_Interval >= pi / 2) {
    stop ("'Slope_Interval' must be greater than 0 and less than pi / 2.")
  }
  Angles <- seq((0 + Slope_Interval), ((pi / 2) - Slope_Interval), Slope_Interval)
  Horizontal_Axis_Intercepts <- seq(min(Data_Frame$Predictor_Variable), max(Data_Frame$Predictor_Variable), by = Horizontal_Axis_Intercept_Interval)
  Sums_of_Squared_Residuals <- list(NULL)
  k <- 1
  for (i in seq_len(length(Horizontal_Axis_Intercepts))) {
    for (j in seq_len(length(Angles))) {
      Bottommost_Points <- ifelse(Data_Frame$Response_Variable < (tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))) & Data_Frame$Response_Variable < -(tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))), T, F)
      Leftmost_Regions <- ifelse(Data_Frame$Response_Variable > (tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))) & Data_Frame$Response_Variable < -(tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))), T, F)
      Topmost_Points <- ifelse(Data_Frame$Response_Variable > (tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))) & Data_Frame$Response_Variable > -(tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))), T, F)
      Rightmost_Points <- ifelse(Data_Frame$Response_Variable < (tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))) & Data_Frame$Response_Variable > -(tan(Angles[j]) * Data_Frame$Predictor_Variable + (-Horizontal_Axis_Intercepts[i] * tan(Angles[j]))), T, F)
      Vertical_Points <- Data_Frame[Bottommost_Points | Topmost_Points, ]
      Vertical_Sum_of_Squared_Residuals <- sum((Vertical_Points$Predictor_Variable - Horizontal_Axis_Intercepts[i]) ^ 2)
      Horizontal_Points <- Data_Frame[Leftmost_Regions | Rightmost_Points, ]
      Horizontal_Sum_of_Squared_Residuals <- sum((Horizontal_Points$Response_Variable) ^ 2)
      Overall_Sum_of_Squared_Residuals <- Vertical_Sum_of_Squared_Residuals + Horizontal_Sum_of_Squared_Residuals
      Sums_of_Squared_Residuals[[k]] <- list(Horizontal_Axis_Intercept = Horizontal_Axis_Intercepts[i], Angle = Angles[j], Overall_Sum_of_Squared_Residuals = Overall_Sum_of_Squared_Residuals)
      k <- k + 1
    }
  }
  Critical_Point_Intercept <- Sums_of_Squared_Residuals[[which.min(sapply(Sums_of_Squared_Residuals, `[`, 'Overall_Sum_of_Squared_Residuals'))]]$Horizontal_Axis_Intercept
  Critical_Point_Angle <- Sums_of_Squared_Residuals[[which.min(sapply(Sums_of_Squared_Residuals, `[`, 'Overall_Sum_of_Squared_Residuals'))]]$Angle
  Critical_Point_Information <- c(Intercept = Critical_Point_Intercept, Angle = Critical_Point_Angle)
  Critical_Point_Information
}


# An Example

# Let's see if we can identify the critical point for the made-up data we
# generated in the 'The Explanation' section.

(Critical_Point <- Function_for_Finding_the_Critical_Point(Predictor_Variable, Response_Variable, Data_Frame))
plot(Response_Variable ~ Predictor_Variable, Data_Frame, main = 'Example Plot', xlab = 'Wood Temperature', ylab = 'Sap Flow')
abline(v = Critical_Point['Intercept'], col = 4)

# I think the function did a nice job!
