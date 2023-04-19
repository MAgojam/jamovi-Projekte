# This file is a generated template, your changes will not be overwritten

# #' @rdname jamovi  # das steht in der Anleitung so, aber ist im Template nicht so drin
#' @export

wilcoxRanksumClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "wilcoxRanksumClass",
  inherit = wilcoxRanksumBase,
  private = list(
    .run = function() {
      
      # getformula
      formula <- jmvcore::constructFormula(self$options$dep, self$options$group)
      formula <- as.formula(formula)
      
      data <- as.data.frame(self$data)
      # data[ , 2] <- factor(data[ , 2], levels = c("2", "1"))
      
      
      ### If CC is selected and distribution is asymptotic, ...
      if(self$options$correct && self$options$dist == "asymptotic"){
        
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
                       var = self$options$dep,
                       # method = self$options$dist,
                       stat = results$statistic,
                       p = results$p.value
                     ))
        
        alter <- results$alternative
        table$addFootnote(rowNo = 1,
                          "p",
                          alter)
        
        ### If CC is selected but distribution is NOT asymptotic...
      } else if(self$options$correct & self$options$dist != "asymptotic"){
        
        # ... use coin::wilcox_test() 
        # results = NULL
        results <- coin::wilcox_test(formula = formula,
                                     data = data,
                                     distribution = self$options$dist,
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
        
        
        
        ### Else (-> CC not selected)... 
      } else {
        
        # ... use coin::wilcox_test() 
        results <- coin::wilcox_test(formula = formula,
                                     data = data,
                                     distribution = self$options$dist,
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
        
        
      }
      
      
      # Warnings / remarks
      ## Some combinations of CC and dist are not possible, some are not recommended
      ### Filter the specifics and write a warning message
      if(self$options$correct & self$options$dist == "exact"){
        warn <- "
        Continuity correction has been selected, but is only available for the asymptotic test. <br>
        The test has been calculated without applying continuity correction.
        "
        table$setNote("remark", warn)
        
      } else if(self$options$correct & self$options$dist == "approximate"){
        warn <-  "
        Continuity correction has been selected, but is only available for the asymptotic test.
        The test has been calculated without applying continuity correction. <br>
        Monte Carlo Approximation with 10'000 draws was applied. p-value might differ for each execution.
        "
        table$setNote("remark", warn)
        
      } else if(self$options$correct & self$options$dist == "asymptotic"){
        warn <-  "
        The use of the continuity correction is generally not recommended, if an exact test is possible. <br>
        We recommend switching the argument 'Distribution' to 'Exact' instead.
        "
        table$setNote("remark", warn)
        
      } else if(!self$options$correct & self$options$dist == "approximate"){
        warn <-  "
        Monte Carlo Approximation with 10'000 draws was applied. p-value might differ for each execution.
        "
        table$setNote("remark", warn)
        
      }
      
      # Paste distribution + WRST for more detailed title
      # tabletitle <- stringr::str_to_title(paste(self$options$dist, "Wilcoxon Rank-Sum Test"))
      # table$setTitle(tabletitle)
      
    }
  )
)


###############
# Note: https://github.com/jamovi/jmv/blob/c495c7804acdd772b2eecc14bb7bbc103ff6ac78/R/ttestis.b.R#L324
# hier fügen sie eine Fussnote hinzu mit table$addFootnote(zeile, spalte, "Nachricht")
# Könnte in der Tabelle eine weitere Spalte hinzufügen Namens "CC" und dann in der Fussnote: "CC: Continuity correction",
# bzw. und dann die Warnings und etwas ähnliches mit exact vs. asymptotic vs. approximate.
#
# Und hier haben sie mit setNote etwas gemacht: 
# https://github.com/jamovi/jmv/blob/c495c7804acdd772b2eecc14bb7bbc103ff6ac78/R/ttestis.b.R#L520
###############




