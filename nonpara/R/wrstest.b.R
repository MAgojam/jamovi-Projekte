
# This file is a generated template, your changes will not be overwritten
#' @export

wrsTestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "wrsTestClass",
  inherit = wrsTestBase,
  private = list(
    .run = function() {
      
      # getformula
      formula <- jmvcore::constructFormula(self$options$dep, self$options$group)
      formula <- as.formula(formula)
      
      data <- as.data.frame(self$data) # das as.data.frame kann evtl. weg?
      dep <- self$options$dep
      group <- self$options$group
      
      data[[dep]] <- jmvcore::toNumeric(data[[dep]])
      data[[group]] <- factor(data[[group]], levels = c("2", "1"))
      data <- na.omit(data)
      
      distribution <- self$options$dist
      
      # Umkehrung der Alternative
      # Funktioniert für p-Wert, aber z-Wert ist ebenfalls verkehrt.
      # Könnte den ebenfalls manuell umkehren, aber ich gehe davon aus,
      # dass eher etwas mit den Daten nicht stimmt,
      # bzw. mit der Art wie jamovi die Daten aufnimmt und verarbeitet.
      # if (self$options$alternative == "less") {alternative <- "greater"}
      # else if (self$options$alternative == "greater") {alternative <- "less"}
      # else if (self$options$alternative == "two.sided") {alternative <- "two.sided"}
      
      ## if exact is selected
      if (distribution == "exact" || distribution == "approximate") {
        
        #... use coin::wilcox_test() 
        results <- coin::wilcox_test(formula = formula,
                                     data = data,
                                     distribution = distribution,
                                     alternative = self$options$alternative)
        
        # create table
        table <- self$results$wrs
        table$setRow(rowNo = 1,
                     values = list(
                       var = self$options$dep,
                       # method = self$options$dist,
                       stat = coin::statistic(results),
                       p = coin::pvalue(results)
                     ))
        
        
        # if approximate is selected
      } else if (distribution == "asymptotic") {
        
        # ... calculate the wilcoxon test with stats
        results <- stats::wilcox.test(formula = formula, 
                                      data = data,
                                      exact = FALSE,
                                      # paired = FALSE,
                                      correct = FALSE,
                                      alternative = self$options$alternative)
        
        # write table
        table <- self$results$wrs
        table$setRow(rowNo = 1,
                     values = list(
                       var = dep,
                       # method = distribution,
                       stat = results$statistic,
                       p = results$p.value
                     ))
        
        alter <- results$alternative
        table$addFootnote(rowNo = 1,
                          "p",
                          alter)
        
        # if asymptotic with CC is selected...
      } else if (distribution == "asymptoticCC") {
        
        # ... calculate the wilcoxon test with stats
        results <- stats::wilcox.test(formula = formula, 
                                      data = data,
                                      exact = FALSE,
                                      # paired = FALSE,
                                      correct = TRUE,
                                      alternative = self$options$alternative)
        
        # write table
        table <- self$results$wrs
        table$setRow(rowNo = 1,
                     values = list(
                       var = dep,
                       # method = self$options$dist,
                       stat = results$statistic,
                       p = results$p.value
                     ))
        
        alter <- results$alternative
        table$addFootnote(rowNo = 1,
                          "p",
                          alter)
      }
      
      # # Paste distribution + WRST for more detailed title
      # tabletitle <- stringr::str_to_title(paste(self$options$dist, "Wilcoxon Rank-Sum Test"))
      # table$setTitle(tabletitle)
      # Don't think that is necessary,
      # same can be done in r.yaml-file

    })
)














