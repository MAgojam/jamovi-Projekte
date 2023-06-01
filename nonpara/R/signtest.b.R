
# This file is a generated template, your changes will not be overwritten

#' @export

signtestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "signtestClass",
  inherit = signtestBase,
  private = list(
    .run = function() {
     
      ############################### To Do ###############################
      # - Testen ob alle fehlerhaften Datenytypen abgefangen werden
      # - Bei den Berechnungen mit try() die allfälligen Fehler abfangen
      # - Bevor Daten eingefüllt werden, wird schon Fehlermeldung angezeigt und Tabelle sieht anders aus
      # - Quelle hinzufügen für coin wahrscheinlich, evtl. stats, und sicher für den Hinweis,
      #   dass CC nicht empfohlen sei (siehe Zotero)
      #####################################################################
      
      
      
      ########## start of data preparation
      # get dep and group
      dep <- self$options$dep
      group <- self$options$group
      
      if(is.null(dep) || is.null(group)) {
        
        return()  # do nothing, as long as not both of group and dep are specified
      } 
      
      
      # get formula
      formula <- jmvcore::constructFormula(self$options$dep, self$options$group)
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

      rstatix::sign_test(Blutdruck ~ Horrorfilm, 
                         data = blood, 
                         alternative = "less")
      
      
      
      
      ########## end of general statistics and descriptives
     
      
      
      
      
      
      
      results <- coin::sign_test(formula = formula,
                                 data = data,
                                 distribution = "exact",
                                 alternative = self$options$alternative)
      
      self$results$text$setContent(results)
      
      table <- self$results$vzr
      table$setRow(rowNo = 1, 
                   values = list(
                     var = dep,
                     "type[exact]" = "Exact",
                     "stat[exact]" = zval,
                     "p[exact]" = coin::pvalue(results)
                   ))
      
    })
)
