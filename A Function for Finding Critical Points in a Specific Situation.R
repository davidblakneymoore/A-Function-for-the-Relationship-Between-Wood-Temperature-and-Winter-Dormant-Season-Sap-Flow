
# Finding Critical Points for the Relationship Between Wood Temperature and
# Winter-Dormant-Season Sap Flow

# David Moore
# University of New Hampshire Ecohydrology Lab
# July 2023
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

# This figure is included in this repository - it's called 'Example Plot 1'.

# This made-up data mimics what I've seen in my studies on winter-dormant-
# season sap flow, where wood temperature is the predictor variable and sap
# flow is the response variable. At extreme wood temperature values, there is
# essentially no sap flow, but when wood temperature values approach a
# particular value (which may be species-specific) from above or below, sap
# flows either downward or upward in deciduous hardwoods.

# In these scenarios, the value of the predictor variable at which the response
# variable is most variable is unknown. This value, which I'll call the
# 'critical value', must be identified and likely has physiological
# significance for the tree. Here, I propose a method to estimate these
# critical values.

# To estimate a critical value, I propose to split the plane of the plot up
# into four quadrants. The quadrants will come together at a point on the
# horizontal axis somewhere between the minimum and maximum values of the
# predictor variable. These quadrants will be delineated by two lines that
# cross on the horizontal axis and that have slopes of equal magnitude but of
# different signs (in other words, the lines that delineate the quadrants are
# reflections of each other about the vertical line that passes through the
# point where the lines delineating the quadrant cross). Again, a figure will
# help to illustrate this concept.

Horizontal_Axis_Intercept_Interval <- 0.1
Vertical_Axis_Intercept_Interval <- 0.1
Slope_Interval <- 0.01
Angles <- seq((0 + Slope_Interval), ((pi / 2) - Slope_Interval), Slope_Interval)
Horizontal_Axis_Intercepts <- seq(min(Data_Frame$Predictor_Variable), max(Data_Frame$Predictor_Variable), by = Horizontal_Axis_Intercept_Interval)
Vertical_Axis_Intercepts <- seq(min(Data_Frame$Predictor_Variable), max(Data_Frame$Predictor_Variable), by = Vertical_Axis_Intercept_Interval)
i <- sample(seq_len(length((Horizontal_Axis_Intercepts))), 1)
j <- sample(seq_len(length((Vertical_Axis_Intercepts))), 1)
k <- sample(seq_len(length(Angles)), 1)
Bottommost_Points <- ifelse((Data_Frame$Response_Variable < ((tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] - (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))) & (Data_Frame$Response_Variable < ((-tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] + (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))), T, F)
Leftmost_Regions <- ifelse((Data_Frame$Response_Variable > ((tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] - (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))) & (Data_Frame$Response_Variable < ((-tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] + (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))), T, F)
Topmost_Points <- ifelse((Data_Frame$Response_Variable > ((tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] - (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))) & (Data_Frame$Response_Variable > ((-tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] + (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))), T, F)
Rightmost_Points <- ifelse((Data_Frame$Response_Variable < ((tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] - (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))) & (Data_Frame$Response_Variable > ((-tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] + (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))), T, F)
Bottommost_Points <- Data_Frame[Bottommost_Points, ]
Leftmost_Regions <- Data_Frame[Leftmost_Regions, ]
Topmost_Points <- Data_Frame[Topmost_Points, ]
Rightmost_Points <- Data_Frame[Rightmost_Points, ]
plot(Response_Variable ~ Predictor_Variable, Data_Frame, type = 'n', main = 'Example Plot', xlab = 'Wood Temperature', ylab = 'Sap Flow')
points(Bottommost_Points$Predictor_Variable, Bottommost_Points$Response_Variable, pch = 20, col = 2)
points(Leftmost_Regions$Predictor_Variable, Leftmost_Regions$Response_Variable, pch = 20, col = 3)
points(Topmost_Points$Predictor_Variable, Topmost_Points$Response_Variable, pch = 20, col = 4)
points(Rightmost_Points$Predictor_Variable, Rightmost_Points$Response_Variable, pch = 20, col = 5)
abline(a = (Vertical_Axis_Intercepts[j] - (Horizontal_Axis_Intercepts[i] * tan(Angles[k]))), b = (tan(Angles[k])))
abline(a = (Vertical_Axis_Intercepts[j] + (Horizontal_Axis_Intercepts[i] * tan(Angles[k]))), b = (-tan(Angles[k])))
abline(h = 0)

# To find the critical value, two major processes will happen. First, the lines
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

# An even better and more descriptive figure that helps to explain this process
# is included in this repository - it's called 'Example Plot 2.jpeg'.

# Actually, a third minor process will also happen concurrently. The vertical
# position of this critical point will be allowed to vary as well. Although, in
# theory, sap flow will approach 0 at extreme temperatures, in practice, sap
# flow values are typically off by a small amount due to probe misalignment
# (details can be found in Burgess et al., 2001). Therefore, in order to
# determine the empirical value for no sap flow, this critical point will be
# allowed to vary vertically, and the vertical position of this critical point
# will be subtracted from all sap flow values to reset the baseline to 0.

# This function takes six arguments. The first two are required.

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

# The 'Vertical_Axis_Intercept_Interval' argument specifies, in whatever units
# the response variable is in, how gradually the quadrants shift from top to
# bottom. Smaller 'Vertical_Axis_Intercept_Interval' values will cause the
# function to take longer to run but will lead to more accurate critical
# values. This argument must be numeric, of length 1, and between 0 and the
# domain of the response variable.


# The Function

Function_for_Finding_the_Critical_Point <- function (Predictor_Variable, Response_Variable, Data_Frame, Slope_Interval = 0.01, Horizontal_Axis_Intercept_Interval = 0.1, Vertical_Axis_Intercept_Interval = 0.1) {
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
    stop ("'Horizontal_Axis_Intercept_Interval' must be greater than 0 and smaller than the difference between the maximum and minimum values of the predictor variable.")
  }
  if (!is.numeric(Vertical_Axis_Intercept_Interval)) {
    stop ("'Vertical_Axis_Intercept_Interval' must be numeric.")
  }
  if (length(Vertical_Axis_Intercept_Interval) != 1) {
    stop ("'Vertical_Axis_Intercept_Interval' must be a single value.")
  }
  if (Vertical_Axis_Intercept_Interval <= 0 | Vertical_Axis_Intercept_Interval >= (max(Data_Frame$Response_Variable) - min(Data_Frame$Response_Variable))) {
    stop ("'Vertical_Axis_Intercept_Interval' must be greater than 0 and smaller than the difference between the maximum and minimum values of the response variable.")
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
  Vertical_Axis_Intercepts <- seq(min(Data_Frame$Response_Variable), max(Data_Frame$Response_Variable), by = Vertical_Axis_Intercept_Interval)
  Output <- list(NULL)
  Counter <- 1
  for (i in seq_len(length(Horizontal_Axis_Intercepts))) {
    for (j in seq_len(length(Vertical_Axis_Intercepts))) {
      for (k in seq_len(length(Angles))) {
        Bottommost_Points <- ifelse((Data_Frame$Response_Variable < ((tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] - (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))) & (Data_Frame$Response_Variable < ((-tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] + (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))), T, F)
        Leftmost_Regions <- ifelse((Data_Frame$Response_Variable > ((tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] - (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))) & (Data_Frame$Response_Variable < ((-tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] + (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))), T, F)
        Topmost_Points <- ifelse((Data_Frame$Response_Variable > ((tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] - (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))) & (Data_Frame$Response_Variable > ((-tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] + (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))), T, F)
        Rightmost_Points <- ifelse((Data_Frame$Response_Variable < ((tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] - (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))) & (Data_Frame$Response_Variable > ((-tan(Angles[k]) * Data_Frame$Predictor_Variable) + Vertical_Axis_Intercepts[j] + (Horizontal_Axis_Intercepts[i] * tan(Angles[k])))), T, F)
        Vertical_Points <- Data_Frame[Bottommost_Points | Topmost_Points, ]
        Vertical_Sum_of_Squared_Residuals <- sum((Vertical_Points$Predictor_Variable - Horizontal_Axis_Intercepts[i]) ^ 2)
        Horizontal_Points <- Data_Frame[Leftmost_Regions | Rightmost_Points, ]
        Horizontal_Sum_of_Squared_Residuals <- sum((Horizontal_Points$Response_Variable) ^ 2)
        Overall_Sum_of_Squared_Residuals <- Vertical_Sum_of_Squared_Residuals + Horizontal_Sum_of_Squared_Residuals
        Output[[Counter]] <- list(Horizontal_Axis_Intercept = Horizontal_Axis_Intercepts[i], Vertical_Axis_Intercept = Vertical_Axis_Intercepts[j], Angle = Angles[k], Overall_Sum_of_Squared_Residuals = Overall_Sum_of_Squared_Residuals, Bottommost_Points_List = Data_Frame[Bottommost_Points, ], Leftmost_Points_List = Data_Frame[Leftmost_Regions, ], Topmost_Points_List = Data_Frame[Topmost_Points, ], Rightmost_Points_List = Data_Frame[Rightmost_Points, ], Vertical_Points = Vertical_Points, Horizontal_Points = Horizontal_Points)
        Counter <- Counter + 1
      }
    }
  }
  Output[[which.min(sapply(Output, `[`, 'Overall_Sum_of_Squared_Residuals'))]]
}


# An Example

# Let's see if we can identify the critical point for the made-up data we
# generated in the 'The Explanation' section.

(Output <- Function_for_Finding_the_Critical_Point(Predictor_Variable, Response_Variable, Data_Frame))
plot(Response_Variable ~ Predictor_Variable, Data_Frame, main = 'Example Plot', xlab = 'Wood Temperature', ylab = 'Sap Flow')
abline(h = Output$Vertical_Axis_Intercept, col = 4)
abline(v = Output$Horizontal_Axis_Intercept, col = 4)

# I think the function did a nice job!


# Works Cited

# Burgess, S.S.O., M.A. Adams, N.C. Turner, C.R. Beverly, C.K. Ong, A.A.H.
# Khan, and T.M. Bleby. 2001. An improved heat pulse method to measure low and
# reverse rates of sap flow in woody plants. Tree Physiol. 21:589-598.
