# This file is a generated template, your changes will not be overwritten

# #' @rdname jamovi  # das steht in der Anleitung so, aber ist im Template nicht so drin
#' @export

wilcoxRanksumClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "wilcoxRanksumClass",
  inherit = wilcoxRanksumBase,
  private = list(
    .run = function() {
      
      ############################### To Do ###############################
      # - Testen ob alle fehlerhaften Datenytypen abgefangen werden
      # - Quellen fixen - aktualisiert erst bei Neustart von jmv korrekt
      # - Hinweise zu MC und CC: entscheide ob beide Footnotes, Notes,
      #   oder ggf. MC in Note und CC in Footnote oder sonst als Hinweis,
      #   wie das Warning beim signtest.
      # - CC verstecken wenn asymptotisch nicht angewählt ist oder nicht?
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
      
      data <- jmvcore::naOmit(data)
      
      ########## end of data preparation
      
      
      
      ########## start of general statistics and descriptives
      # Create ranked dataframe
      data_ranked <- data.frame(Group = data[[group]],       # group to factor
                                Ranks = rank(data[[dep]]))   # values are ranked
      
      count <- data_ranked$Group |>                          # take group
        table() |>                                           # count observations per group
        as.data.frame()
      
      gr1 <- count |>                                        # format as df
        dplyr::filter(Freq == min(Freq)) |>                  # filter for the least observations
        dplyr::filter(Var1 == Var1[1]) |>                    # select the Variab
        dplyr::select(Var1) |>                               # select the group-variable
        unlist() |> 
        as.integer()                                         # this gets the name of the group with the least observations
      
      
      # get z-statistic
      results <- try(coin::wilcox_test(formula = formula,
                                       data = data,
                                       distribution = "exact",
                                       alternative = self$options$alternative),
                     silent = TRUE)
      
      if(jmvcore::isError(results)) {
        
        table <- self$results$wrs
        table$setRow(rowNo = 1,
                     values = list(
                       var = "",
                       "type[exact]" = "",
                       "stat[exact]" = "",
                       "rs1[exact]" = "",
                       "u[exact]" = "",
                       "p[exact]" = ""
                     ))
      } else {
        
        zval <- coin::statistic(results)
        
      }
      
      
      
      
      
      ## calculate RS 1
      RS1 <- data_ranked |>                           # get data
        dplyr::filter(Group == gr1) |>                # filter for first level
        dplyr::select(Ranks) |>                       # select ranks
        sum()                                         # sum of ranks
      
      
      ## calculate Mann-Whitney U
      n1 <- count[order(count$Freq), 2][1]            # frequency of group, order them ascending, count,
      n2 <- count[order(count$Freq), 2][2]            # assign the lower value to n1, the higher value to n2
      u <- RS1 - ((n1+1)*n1/2)
      
      
      
      ## get descriptives if selected
      #### mean ranks per group
      if(self$options$descriptives) {
        rankmean_R1 <- data_ranked |>
          dplyr::filter(Group == gr1) |>
          dplyr::select(Ranks) |>
          colMeans() |>
          as.vector()
        
        rankmean_R2 <- data_ranked |>
          dplyr::filter(Group != gr1) |>
          dplyr::select(Ranks) |>
          colMeans() |>
          as.vector()
        
        
        #### median per group
        median_g1 <- data |>
          dplyr::filter(!! dplyr::sym(group) == gr1) |>
          dplyr::select(all_of(dep)) |>
          unlist() |>
          as.vector() |>
          median()
        
        median_g2 <- data |>
          dplyr::filter(!! dplyr::sym(group) != gr1) |>      # dplyr::filter(data[[group]] != gr1) |>  
          dplyr::select(all_of(dep)) |>
          unlist() |>
          as.vector() |>
          median()
        
        #### write table
        desk <- self$results$desc
        desk$setRow(rowNo = 1,
                    values = list(
                      "dep" = dep,
                      "group[1]" = "1",
                      "group[2]" = "2",
                      
                      "num[1]" = n1,
                      "num[2]" = n2,
                      
                      "median[1]" = median_g1,
                      "median[2]" = median_g2,
                      
                      "rankmean[1]" = rankmean_R1,
                      "rankmean[2]" = rankmean_R2
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
      
      
      ########## start of default table
      
      
      
      
      
      ##########
      
      
      ########## start of exact analysis
      if (self$options$exact) {
        
        # create table (calculations were already done in the section above)
        table <- self$results$wrs
        table$setRow(rowNo = 1,
                     values = list(
                       var = self$options$dep,
                       "type[exact]" = "Exact",
                       "stat[exact]" = zval,
                       "rs1[exact]" = RS1,
                       "u[exact]" = u,
                       "p[exact]" = coin::pvalue(results)
                     ))
        
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
          
          # create table
          table <- self$results$wrs
          table$setRow(rowNo = 1,
                       values = list(
                         var = self$options$dep,
                         "type[approximate]" = "",
                         "stat[approximate]" = "",
                         "rs1[approximate]" = "",
                         "u[approximate]" = "",
                         "p[approximate]" = ""
                       ))
          
        } else {
          
          # create table
          table <- self$results$wrs
          table$setRow(rowNo = 1,
                       values = list(
                         var = self$options$dep,
                         "type[approximate]" = 'Monte-Carlo Approximation',
                         "stat[approximate]" = zval,
                         "rs1[approximate]" = RS1,
                         "u[approximate]" = u,
                         "p[approximate]" = coin::pvalue(results)
                       ))
          
          footnote1 <- paste('Monte Carlo Approximation with',
                             self$options$nsamples, 
                             'samples was applied. <i>p</i>-value might differ for each execution.')
          table$addFootnote(rowNo=1, col="type[approximate]", footnote1)
          
        }
        
      }
      ########## End of approximate analysis
      
      
      
      ########## Start of asymptotic analysis WITHOUT CC
      if (self$options$asymptotic) {
        
        # ... calculate the wilcoxon test with stats
        results <- try(stats::wilcox.test(formula = formula,
                                          data = data,
                                          exact = FALSE,
                                          paired = FALSE,
                                          correct = FALSE,
                                          alternative = self$options$alternative),
                       silent = TRUE)
        
        if (jmvcore::isError(results)) {
          
          # write table
          table <- self$results$wrs
          table$setRow(rowNo = 1,
                       values = list(
                         var = dep,
                         "type[asymptotic]" = "",
                         "stat[asymptotic]" = "",
                         "rs1[asymptotic]" = "",
                         "u[asymptotic]" = "",
                         "p[asymptotic]" = ""
                       ))
          
        } else {
          
          # write table
          table <- self$results$wrs
          table$setRow(rowNo = 1,
                       values = list(
                         var = dep,
                         "type[asymptotic]" = 'Asymptotic',
                         "stat[asymptotic]" = zval,
                         "rs1[asymptotic]" = RS1,
                         "u[asymptotic]" = u,
                         "p[asymptotic]" = results$p.value
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
          
          table <- self$results$wrs
          table$setRow(rowNo = 1,
                       values = list(
                         var = dep,
                         "type[cc]" = "",
                         "stat[cc]" = "",
                         "rs1[cc]" = "",
                         "u[cc]" = "",
                         "p[cc]" = ""
                       ))
          
        } else {
          
          # write table
          table <- self$results$wrs
          table$setRow(rowNo = 1,
                       values = list(
                         var = dep,
                         "type[cc]" = 'Asymptotic (CC)',
                         "stat[cc]" = qnorm(results$p.value),
                         "rs1[cc]" = RS1,
                         "u[cc]" = u,
                         "p[cc]" = results$p.value
                       ))
          
          footnote2 <- 'The use of the continuity correction is generally not recommended, if an exact test is possible.
                        We recommend using the exact test instead.'
          table$addFootnote(rowNo=1, col="type[cc]", footnote2)
        }
        
      }
      ########## End of asymptotic analysis WITH CC
      
      
      
      #### Throw error if no method is selected
      if(!self$options$exact && !self$options$approximate && !self$options$asymptotic && !self$options$cc) {
        jmvcore::reject("Must select at least one method",
                        code = "min_one_method")
      }
      
      # #### Adjust references based on which tests are selected
      # # exact and/or approximate active, but cc not active
      # if((self$options$exact || self$options$approximate) && !self$options$cc) {
      #   table$setRefs('coin')
      #   # neither exact nor approximate active, but cc active
      # } else if(!self$options$exact && !self$options$approximate && self$options$cc) {
      #   table$setRefs('cf')
      #   # exact and/or approximate active, cc also active
      # } else if((self$options$exact || self$options$approximate) && self$options$cc) {
      #   table$setRefs(c('coin', 'cf'))
      # }
  
      
      
      
      # #### Warnings / remarks
      # ## Empty note-object
      # note1 <- note2 <- c()
      # 
      # ## Write a note, if these conditions are met
      # if(self$options$approximate){
      #   note1 <-  paste('Monte Carlo Approximation with', self$options$nsamples, 'samples was applied. <i>p</i>-value might differ for each execution.')
      # }
      # 
      # if(self$options$cc){
      #   note2 <-  '
      #   The use of the continuity correction is generally not recommended, if an exact test is possible.
      #   We recommend using the exact test instead.
      #   '
      # }
      # 
      # ## Paste the notes together
      # ## ("" ) is so that the string is never empty, which would lead to 'character(0)'
      # if(is.null(note1) & !is.null(note2)) {
      #   # print("note2")
      #   table$setNote('remark', note2)
      # } else if(is.null(note2) & !is.null(note1)) {
      #   # print("note1")
      #   table$setNote('remark', note1)
      # } else if(!is.null(c(note1, note2))) {
      #   # print("beide")
      #   note <- paste('a)', note1, "<br> b)", note2)
      #   table$setNote('remark', note)
      # } else{print('keine')}
      
      
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


###############
# Note: https://github.com/jamovi/jmv/blob/c495c7804acdd772b2eecc14bb7bbc103ff6ac78/R/ttestis.b.R#L324
# hier fügen sie eine Fussnote hinzu mit table$addFootnote(zeile, spalte, "Nachricht")
# Könnte in der Tabelle eine weitere Spalte hinzufügen Namens "CC" und dann in der Fussnote: "CC: Continuity correction",
# bzw. und dann die Warnings und etwas ähnliches mit exact vs. asymptotic vs. approximate.
#
# Und hier haben sie mit setNote etwas gemacht: 
# https://github.com/jamovi/jmv/blob/c495c7804acdd772b2eecc14bb7bbc103ff6ac78/R/ttestis.b.R#L520
###############


