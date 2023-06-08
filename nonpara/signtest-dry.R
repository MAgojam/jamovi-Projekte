# Options init
self <- list()

self$data <- data.frame(   
  Blutdruck = c(130.2, 180.2, 149.6, 153.2, 162.6, 160.1,
                136.9, 201.4, 166.8, 150.0, 173.2, 169.3), 
  Horrorfilm = factor(rep(c("vorher","nachher"), each = 6),
                      levels = c("vorher", "nachher")))
self$options$exact <- TRUE
self$options$app <- TRUE
self$options$nsamples <- 10000
self$options$asy <- TRUE
self$options$dep <- "Blutdruck"
self$options$group <- "Horrorfilm"
self$options$alternative <- "less"

self$options$descriptives <- FALSE
self$options$descplot <- FALSE
self$options$observed <- FALSE









########## start of data preparation
# get dep and group
dep <- self$options$dep
group <- self$options$group

if(is.null(dep) || is.null(group)) {
  
  return()  # do nothing, as long as not both of group and dep are specified
} 


# get formula
formula <- jmvcore::constructFormula(dep, group)
formula <- as.formula(formula)

# create and control data
data <- as.data.frame(self$data)

data[[dep]] <- jmvcore::toNumeric(data[[dep]])
data[[group]] <- factor(data[[group]], ordered = FALSE)

groupLevels <- base::levels(data[[group]])
if (length(groupLevels) != 2) {jmvcore::reject("Grouping variable must have exactly 2 levels",
                                               code = "grouping_var_must_have_2_levels")
}

data <- na.omit(data)

########## end of data preparation 



########## start of general statistics and descriptives

# exakt <- rstatix::sign_test(formula = formula,
#                              data = data,
#                              alternative = self$options$alternative)

descs2 <- coin::sign_test(formula = formula,
                          data = data,
                          alternative = self$options$alternative)

# s <- exakt$statistic
# df <- exakt$df
zval <- coin::statistic(descs2)

########## end of general statistics and descriptives



########## start of analysis

# of the dependent variables, take those that have level 1 for group as group 1
g1 <- data[[dep]][data[[group]] == groupLevels[1]]
g2 <- data[[dep]][data[[group]] == groupLevels[2]]

exakt <- coin::sign_test(formula = g1 ~ g2,
                         data = data,
                         distribution = "exact",
                         alternative = self$options$alternative)

mc <- coin::sign_test(formula = g1 ~ g2,
                      data = data,
                      distribution = "approximate",
                      nsamples = nsamples,
                      alternative = self$options$alternative)

asymp <- coin::sign_test(formula = g1 ~ g2,
                         data = data,
                         distribution = "asymptotic",
                         alternative = self$options$alternative)

########## start of analysis

