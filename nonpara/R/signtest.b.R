
# This file is a generated template, your changes will not be overwritten

#' @export

signtestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "signtestClass",
  inherit = signtestBase,
  private = list(
    .run = function() {
      
      ############################### To Do ###############################
      # - Testen ob alle fehlerhaften Datenytypen abgefangen werden
      # - Quelle hinzufügen für coin wahrscheinlich, evtl. stats
      # - überprüfen ob df wirklich nur Integer sein kann
      # - evtl. S und df als additional statistics, wie beim WRS U und RS1
      # - bin gerade dran die ID als variable hinzuzufügen, weil sie für
      #   die grafik nötig ist.
      #####################################################################
      
      
      
      ########## start of data preparation
      # get dep and group
      dep <- self$options$dep
      group <- self$options$group
      
      if(is.null(dep) || is.null(group) || is.null(id)) {
        
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
      if (length(groupLevels) != 2) {
        jmvcore::reject("Grouping variable must have exactly 2 levels",
                        code = "grouping_var_must_have_2_levels")
      }
      
      data <- na.omit(data)
      
      # of the dependent variables, take those that have level 1 for group as group 1
      g1 <- data[[dep]][data[[group]] == groupLevels[1]]
      g2 <- data[[dep]][data[[group]] == groupLevels[2]]
      
      table <- self$results$vzr
      ########## end of data preparation 
      
      
      
      ########## start of general statistics and descriptives
      df = 0
      s = 0
      
      for(i in 1:length(g1)) {
        if(g1[i] != g2[i]) { df = df + 1 }
        if(g1[i]  > g2[i]) { s = s + 1 }
      }
      
      ## get descriptives if selected
      if(self$options$descriptives) {
        ### n per group
        nobs <- length(data[[group]])/2
        
        #### median per group
        median_g1 <- data |>
          dplyr::filter(!! dplyr::sym(group) == groupLevels[1]) |>
          dplyr::select(all_of(dep)) |>
          unlist() |>
          as.vector() |>
          median() |> 
          format(nsmall = 2)
        
        median_g2 <- data |>
          dplyr::filter(!! dplyr::sym(group) == groupLevels[2]) |> 
          dplyr::select(all_of(dep)) |>
          unlist() |>
          as.vector() |>
          median() |> 
          format(nsmall = 2)
        
        #### write table
        desk <- self$results$desc
        desk$setRow(rowNo = 1,
                    values = list(
                      "dep" = dep,
                      "nobs[1]" = nobs,
                      "nobs[2]" = nobs,
                      
                      "time[1]" = groupLevels[1],
                      "time[2]" = groupLevels[2],
                      
                      "median[1]" = median_g1,
                      "median[2]" = median_g2
                    ))
      }
      
      
      if(self$options$plot) {  
        
        # if plot is selected, send a simplified dataframe to self$results$plot
        plotData <- data.frame(value = data[[dep]],
                               group = data[[group]])
        
        image <- self$results$plot
        image$setState(plotData)
        
      }
      ########## end of general statistics and descriptives
      
      
      
      ########## start of analysis
      ####       Exakt
      exakt <- try(coin::sign_test(formula = g1 ~ g2,
                                   data = data,
                                   distribution = "exact",
                                   alternative = self$options$alternative),
                   silent = TRUE)
      
      if(jmvcore::isError(exakt)) {
        
        table <- self$results$wrs
        table$setRow(rowNo = 1,
                     values = list(
                       var = "",
                       "type[exact]" = "",
                       "stat[exact]" = "",
                       "s[exact]" = "",
                       "df[exact]" = "",
                       "p[exact]" = ""
                     ))
      } else {
        table$setRow(rowNo = 1, 
                     values = list(
                       var = dep,
                       "type[exact]" = "Exact",
                       "stat[exact]" = coin::statistic(exakt),
                       "s[exact]" = s,
                       "df[exact]" = df,
                       "p[exact]" = coin::pvalue(exakt)
                     ))
      }
      
      ####       Monte carlo
      mc <- try(coin::sign_test(formula = g1 ~ g2,
                                data = data,
                                distribution = "approximate",
                                nsamples = self$options$nsamples,
                                alternative = self$options$alternative),
                silent = TRUE)
      
      if(jmvcore::isError(exakt)) {
        
        table$setRow(rowNo = 1,
                     values = list(
                       var = "",
                       "type[exact]" = "",
                       "stat[exact]" = "",
                       "s[exact]" = "",
                       "df[exact]" = "",
                       "p[exact]" = ""
                     ))
      } else {
        table$setRow(rowNo = 1, 
                     values = list(
                       var = dep,
                       "type[app]" = "Monte-Carlo Approximation",
                       "stat[app]" = coin::statistic(mc),
                       "s[app]" = s,
                       "df[app]" = df,
                       "p[app]" = coin::pvalue(mc)
                     ))
      }
      
      
      ####       Asymptotisch
      asymp <- try(coin::sign_test(formula = g1 ~ g2,
                                   data = data,
                                   distribution = "asymptotic",
                                   alternative = self$options$alternative),
                   silent = TRUE)
      
      if(jmvcore::isError(exakt)) {
        
        table <- self$results$wrs
        table$setRow(rowNo = 1,
                     values = list(
                       var = "",
                       "type[exact]" = "",
                       "stat[exact]" = "",
                       "s[exact]" = "",
                       "df[exact]" = "",
                       "p[exact]" = ""
                     ))
      } else {
        table$setRow(rowNo = 1, 
                     values = list(
                       var = dep,
                       "type[asy]" = "Asymptotic",
                       "stat[asy]" = coin::statistic(asymp),
                       "s[asy]" = s,
                       "df[asy]" = df,
                       "p[asy]" = coin::pvalue(asymp)
                     ))
      }
      ########## end of analysis
      
    },
    
    
    
    
    
    .descplot = function(image, ggtheme, theme...) {
      
      if(is.null(self$options$dep) || is.null(self$options$group)) {
        
        return()  # do nothing, as long as not both of group and dep are specified
      }
      
      
      # transposed = as.data.frame(t(vzr[1:3, -1]))
      # colnames(transposed) = c("Person", "vorher", "nachher")
      # transposed = melt(transposed, id = "Person")
      # colnames(transposed) = c("Person", "Messzeitpunkt", "Blutdruck")
      # transposed$Blutdruck = as.numeric(transposed$Blutdruck)
      # 
      # ggplot(data = transposed, aes(x = Messzeitpunkt,
      #                               y = Blutdruck,
      #                               group = Person)) +
      #   geom_line(aes(color = Person)) +
      #   geom_point() +
      #   scale_y_continuous(breaks = seq(120, 220, 10)) +
      #   theme(text = element_text(size = 12))
      
      
      
      plot <- ggplot(data = image$state,
                     aes(x = group,
                         y = value, 
                         fill = group)) + 
        geom_boxplot(outlier.shape = 1,
                     outlier.size = 2) +
        labs(x = self$options$group, 
             y = self$options$dep) +
        theme_classic() +
        ggtheme +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 16),
              axis.ticks.length = unit(.2, "cm"),
              legend.position = "none",
              plot.margin = margin(5.5, 5.5, 5.5, 5.5))
      
      if(self$options$observed) {
        plot <- plot + 
          geom_jitter(color = "black", 
                      size = 1,
                      width = 0.1,
                      alpha = 0.9)
      }
      print(plot)
      TRUE
      
    }
    
  )
)
