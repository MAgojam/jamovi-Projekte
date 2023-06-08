
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
      # - evtl. bei der Aufteilung der Daten auch auf die ID achten?
      #   Reihenfolge der Daten ist ja relevant für den Test
      #####################################################################
      
      
      
      ########## start of data preparation
      # get dep and group
      dep <- self$options$dep
      group <- self$options$group
      id <- self$options$id
      
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
      # Daten sortieren zuerst nach Gruppe dann nach ID
      data <- dplyr::arrange(data, data[[group]], data[[id]])
      
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
        
        # copy of data, but easier to handle in ggplot
        plotData <- data.frame(id = data[[id]],
                               value = data[[dep]],
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
                       "type[approximate]" = "",
                       "stat[approximate]" = "",
                       "s[approximate]" = "",
                       "df[approximate]" = "",
                       "p[approximate]" = ""
                     ))
      } else {
        table$setRow(rowNo = 1, 
                     values = list(
                       var = dep,
                       "type[approximate]" = "Monte-Carlo Approximation",
                       "stat[approximate]" = coin::statistic(mc),
                       "s[approximate]" = s,
                       "df[approximate]" = df,
                       "p[approximate]" = coin::pvalue(mc)
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
                       "type[asymptotic]" = "",
                       "stat[asymptotic]" = "",
                       "s[asymptotic]" = "",
                       "df[asymptotic]" = "",
                       "p[asymptotic]" = ""
                     ))
      } else {
        table$setRow(rowNo = 1, 
                     values = list(
                       var = dep,
                       "type[asymptotic]" = "Asymptotic",
                       "stat[asymptotic]" = coin::statistic(asymp),
                       "s[asymptotic]" = s,
                       "df[asymptotic]" = df,
                       "p[asymptotic]" = coin::pvalue(asymp)
                     ))
      }
      ########## end of analysis
      
      
      
      ########## start of warnings
      # Warnings / remarks
      ## Empty note-object
      note <- c()
      
      ## Write a note, if these conditions are met
      if(self$options$approximate){
        note <-  paste('Monte Carlo Approximation with', 
                       self$options$nsamples, 
                       'samples was applied. <i>p</i>-value might differ for each execution.')
        table$setNote('remark', note)
      }
      ########## end of warnings
    },
    
    
    
    
    
    .descplot = function(image, ggtheme, theme...) {
      
      if(is.null(self$options$dep) || is.null(self$options$group)) {
        
        return()  # do nothing, as long as not both of group and dep are specified
      }
      
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
      
      # if(self$options$observed == "line"){
      #   plot <- plot + 
      #     geom_line(aes(color = id, group = id)) +
      #     geom_point()
      # } else if(self$options$observed == "jitter"){
      #   plot <- plot + 
      #     geom_jitter(color = "black", 
      #                 size = 1,
      #                 width = 0.1,
      #                 alpha = 0.9)
      # }
      
      print(plot)
      TRUE
      
    }
    
  )
)
