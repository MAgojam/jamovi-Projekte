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
      
      data <- as.data.frame(self$data) # das as.data.frame kann evtl. weg?
      dep <- self$options$dep
      group <- self$options$group
      
      ############################### To Do ###############################
      # - Testen ob alle fehlerhaften Datenytypen abgefangen werden
      # - Bei den Berechnungen mit try() die allfälligen Fehler abfangen
      # - Bevor Daten eingefüllt werden, wird schon Fehlermeldung angezeigt und Tabelle sieht anders aus
      # - Statistic ist beim exakten Wert z aber beim asymptotischen keine Ahnung was.
      #   -> kann man evtl. immer den stats::wilcox.test() laufen lassen und dort W+ rausziehen?
      #   Wobei ich befürchte dass das auch nicht korrekt berechnet wird.
      # - Quelle hinzufügen für coin wahrscheinlich, evtl. stats, und sicher für den Hinweis,
      #   dass CC nicht empfohlen sei (siehe Zotero)
      #####################################################################
      
      data[[dep]] <- jmvcore::toNumeric(data[[dep]])
      data[[group]] <- factor(data[[group]], ordered = FALSE)
      
      data <- na.omit(data)
      
      
      
      ########## start of exact analysis
      if (self$options$exact) {
        
        results <- try(coin::wilcox_test(formula = formula,
                                         data = data,
                                         distribution = "exact",
                                         alternative = self$options$alternative),
                       silent = TRUE)
        
        if (jmvcore::isError(results)) {
          
          # table$setRow(rowKey = depName, 
          #              list(
          #                "stat[stud]" = NaN
          #              )) 
          # siehe hier: https://github.com/jamovi/jmv/blob/master/R/ttestis.b.R#L122
          
        } else {
          
          # create table
          table <- self$results$wrs
          table$setRow(rowNo = 1,
                       values = list(
                         var = self$options$dep,
                         "type[ex]" = "Exact",
                         "stat[ex]" = coin::statistic(results),
                         "p[ex]" = coin::pvalue(results)
                       ))
          
        }
        
      } 
      ########## End of exact analysis
      
      
      
      ########## Start of approximate analysis
      if (self$options$approximate) {
        
        results <- try(coin::wilcox_test(formula = formula,
                                         data = data,
                                         distribution = "approximate",
                                         alternative = self$options$alternative),
                       silent = TRUE)
        
        if (jmvcore::isError(results)) {
          
          # table$setRow(rowKey = depName, 
          #              list(
          #                "stat[stud]" = NaN
          #              )) 
          # siehe hier: https://github.com/jamovi/jmv/blob/master/R/ttestis.b.R#L122
          
        } else {
          
          # create table
          table <- self$results$wrs
          table$setRow(rowNo = 1,
                       values = list(
                         var = self$options$dep,
                         "type[app]" = "Approximate",
                         "stat[app]" = coin::statistic(results),
                         "p[app]" = coin::pvalue(results)
                       ))
          
        }
        
      }
      ########## End of approximate analysis
      
      
      
      ########## Start of asymptotic analysis WITHOUT CC
      if (self$options$asymptotic) {
        
        # ... calculate the wilcoxon test with stats
        results <- try(stats::wilcox.test(formula = formula, 
                                          data = data,
                                          exact = FALSE,
                                          # paired = FALSE,
                                          correct = FALSE,
                                          alternative = self$options$alternative),
                       silent = TRUE)
        
        if (jmvcore::isError(results)) {
          
          # table$setRow(rowKey = depName, 
          #              list(
          #                "stat[stud]" = NaN
          #              )) 
          # siehe hier: https://github.com/jamovi/jmv/blob/master/R/ttestis.b.R#L122
          
        } else {
          
          # write table
          table <- self$results$wrs
          table$setRow(rowNo = 1,
                       values = list(
                         var = dep,
                         "type[asy]" = "Asymptotic",
                         "stat[asy]" = results$statistic,
                         "p[asy]" = results$p.value
                       ))
          
        }
        
      }
      ########## End of asymptotic analysis WITHOUT CC
      
      
      
      ########## Start of asymptotic analysis WITH CC
      if (self$options$cc) {
        
        # ... calculate the wilcoxon test with stats
        results <- try(stats::wilcox.test(formula = formula, 
                                          data = data,
                                          exact = FALSE,
                                          # paired = FALSE,
                                          correct = TRUE,
                                          alternative = self$options$alternative),
                       silent = TRUE)
        
        if (jmvcore::isError(results)) {
          
          # table$setRow(rowKey = depName, 
          #              list(
          #                "stat[stud]" = NaN
          #              )) 
          # siehe hier: https://github.com/jamovi/jmv/blob/master/R/ttestis.b.R#L122
          
        } else {
          
          # write table
          table <- self$results$wrs
          table$setRow(rowNo = 1,
                       values = list(
                         var = dep,
                         "type[cc]" = "Asymptotic (CC)",
                         "stat[cc]" = results$statistic,
                         "p[cc]" = results$p.value
                       ))
          
        }
        
      }
      ########## End of asymptotic analysis WITH CC
      
      
      
      # Warnings / remarks
      ## Empty note-object
      note1 <- note2 <- c()
      
      ## Write a note, if these conditions are met
      if(self$options$approximate){
        note1 <-  "Monte Carlo Approximation with 10'000 draws was applied. <i>p</i>-value might differ for each execution."
      }
      
      if(self$options$cc){
        note2 <-  "
        The use of the continuity correction is generally not recommended, if an exact test is possible.
        We recommend heeding the exact test instead.
        "
      }
      
      ## Paste the notes together 
      ## ("" ) is so that the string is never empty, which would lead to 'character(0)'
      if(is.null(note1) & !is.null(note2)) {
        # print("note2")
        table$setNote("remark", note2)
      } else if(is.null(note2) & !is.null(note1)) {
        # print("note1")
        table$setNote("remark", note1)
      } else if(!is.null(c(note1, note2))) {
        # print("beide")
        note <- paste("a)", note1, "<br> b)", note2)
        table$setNote("remark", note)
      } else{print("keine")}
      
      
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












# ## if exact is selected
# if (distribution == "exact" || distribution == "approximate") {
#   
#   #... use coin::wilcox_test() 
#   results <- coin::wilcox_test(formula = formula,
#                                data = data,
#                                distribution = distribution,
#                                alternative = self$options$alternative)
#   
#   # create table
#   table <- self$results$wrs
#   table$setRow(rowNo = 1,
#                values = list(
#                  var = self$options$dep,
#                  # method = self$options$dist,
#                  stat = coin::statistic(results),
#                  p = coin::pvalue(results)
#                ))
#   
#   
#   # if approximate is selected
# } else if (distribution == "asymptotic") {
#   
#   # ... calculate the wilcoxon test with stats
#   results <- stats::wilcox.test(formula = formula, 
#                                 data = data,
#                                 exact = FALSE,
#                                 # paired = FALSE,
#                                 correct = FALSE,
#                                 alternative = self$options$alternative)
#   
#   # write table
#   table <- self$results$wrs
#   table$setRow(rowNo = 1,
#                values = list(
#                  var = dep,
#                  # method = distribution,
#                  stat = results$statistic,
#                  p = results$p.value
#                ))
#   
#   alter <- results$alternative
#   table$addFootnote(rowNo = 1,
#                     "p",
#                     alter)
#   
#   # if asymptotic with CC is selected...
# } else if (distribution == "asymptoticCC") {
#   
#   # ... calculate the wilcoxon test with stats
#   results <- stats::wilcox.test(formula = formula, 
#                                 data = data,
#                                 exact = FALSE,
#                                 # paired = FALSE,
#                                 correct = TRUE,
#                                 alternative = self$options$alternative)
#   
#   # write table
#   table <- self$results$wrs
#   table$setRow(rowNo = 1,
#                values = list(
#                  var = dep,
#                  # method = self$options$dist,
#                  stat = results$statistic,
#                  p = results$p.value
#                ))
#   
#   alter <- results$alternative
#   table$addFootnote(rowNo = 1,
#                     "p",
#                     alter)
# }
# 
# 
