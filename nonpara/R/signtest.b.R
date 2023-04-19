
# This file is a generated template, your changes will not be overwritten

#' @export

signtestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "signtestClass",
  inherit = signtestBase,
  private = list(
    .run = function() {
      
      data <- self$data
      formula <- jmvcore::constructFormula(self$options$samp1, self$options$samp2)
      formula <- as.formula(formula)
      
      results <- coin::sign_test(formula = formula,
                                 data = data,
                                 distribution = "exact",
                                 alternative = self$options$alternative)
      
      self$results$text$setContent(results)
      
      table <- self$results$table
      table$setRow(rowNo = 1, 
                   values = list(
                     s1 = self$options$samp1,
                     alt = self$options$alternative,
                     s2 = self$options$samp2,
                     stat = coin::statistic(results),
                     p = coin::pvalue(results)
                   ))
      
    })
)
