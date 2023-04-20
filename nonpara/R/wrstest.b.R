
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
      
      ################## To do ############
      # Testen ob Gruppierungs-Variable integer oder character ist.
      # vlt. mit is.character(data[[group[1]]]) und dann wenn ja, alle die den gleichen Wert haben ersetzen durch 1,
      # also irgendwie eine integer-variante focieren. Vlt. gibt es auch bessere Alternativen
      #####################################
      
      data[[dep]] <- jmvcore::toNumeric(data[[dep]])
      data[[group]] <- factor(data[[group]], ordered = FALSE)
      
      data <- na.omit(data)
      
      distribution <- self$options$dist
      
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














