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
      # - Bei den Berechnungen mit try() die allfälligen Fehler abfangen
      # - Bevor Daten eingefüllt werden, wird schon Fehlermeldung angezeigt und Tabelle sieht anders aus
      # - Statistic ist beim exakten Wert z aber beim asymptotischen keine Ahnung was.
      #   -> kann man evtl. immer den stats::wilcox.test() laufen lassen und dort W+ rausziehen?
      #   Wobei ich befürchte dass das auch nicht korrekt berechnet wird.
      # - Quelle hinzufügen für coin wahrscheinlich, evtl. stats, und sicher für den Hinweis,
      #   dass CC nicht empfohlen sei (siehe Zotero)
      #####################################################################



      ########## start of data preparation
      # get formula
      formula <- jmvcore::constructFormula(self$options$dep, self$options$group)
      formula <- as.formula(formula)

      data <- as.data.frame(self$data) # das as.data.frame kann evtl. weg?
      dep <- self$options$dep
      group <- self$options$group

      data[[dep]] <- jmvcore::toNumeric(data[[dep]])
      data[[group]] <- factor(data[[group]], ordered = FALSE)

      data <- na.omit(data)


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
        dplyr::select(Var1) |>                               # select the group-variable
        unlist() |> 
        as.integer()                                         # this gets the name of the group with the least observations


      # get z-statistic
      results <- try(coin::wilcox_test(formula = formula,
                                       data = data,
                                       distribution = "exact",
                                       alternative = self$options$alternative),
                     silent = TRUE)

      zval <- coin::statistic(results)





      ## calculate RS 1 if selected
        RS1 <- data_ranked |>                           # get data
          dplyr::filter(Group == gr1) |>                # filter for first level
          dplyr::select(Ranks) |>                       # select ranks
          sum()                                         # sum of ranks


      ## calculate Mann-Whitney U
        n1 <- count[order(count$Freq), 2][1]
        n2 <- count[order(count$Freq), 2][2]
        u <- RS1 - ((n1+1)*n1/2)



      ## get descriptives
      #### mean ranks per group
      if(self$options$rankmean) {
        rankmean_R1 <- data_ranked |>
          dplyr::filter(Group == gr1) |>
          dplyr::select(Ranks) |>
          colMeans() |>
          as.integer()

        rankmean_R2 <- data_ranked |>
          dplyr::filter(Group != gr1) |>
          dplyr::select(Ranks) |>
          colMeans() |>
          as.integer()
      }



      #### median per group
      if(self$options$median) {
        median_g1 <- data |>
          dplyr::filter(data[[group]] == gr1) |>
          dplyr::select(all_of(dep)) |>
          unlist() |>
          as.integer() |>
          median()

        median_g2 <- data |>
          dplyr::filter(data[[group]] != gr1) |>
          dplyr::select(all_of(dep)) |>
          unlist() |>
          as.integer() |>
          median()
      }



      desk <- self$results$desc
      desk$setRow(rowNo = 1,
                  values = list(
                    kind = 'Mean',
                    "rankmean[g1]" = rankmean_R1,  # g1 = rankmean_R1
                    "rankmean[g2]" = rankmean_R2
                  ))
      desk$setRow(rowNo = 2,
                  values = list(
                    kind = 'Median',
                    "meadian[g1]" = median_g1,
                    "meadian[g2]" = median_g2
                  ))

      ########## end of general statistics and descriptives


      ########## start of exact analysis
      if (self$options$exact) {

        ## wurde auskommentiert, da jetzt der exakte Test schon weiter oben gemacht wird für die z-Statistik

        # results <- try(coin::wilcox_test(formula = formula,
        #                                  data = data,
        #                                  distribution = "exact",
        #                                  alternative = self$options$alternative),
        #                silent = TRUE)
        # 
        # if (jmvcore::isError(results)) {
        # 
        #   table$setRow(rowKey = depName,
        #                list(
        #                  "stat[stud]" = NaN
        #                ))
        #   siehe hier: https://github.com/jamovi/jmv/blob/master/R/ttestis.b.R#L122
        # 
        # } else {

          # create table
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

        # }

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
                         "type[approximate]" = 'Approximate',
                         "stat[approximate]" = zval,
                         "rs1[approximate]" = RS1,
                         "u[approximate]" = u,
                         "p[approximate]" = coin::pvalue(results)
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
                         "type[cc]" = 'Asymptotic (CC)',
                         "stat[cc]" = zval,
                         "rs1[cc]" = RS1,
                         "u[cc]" = u,
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
        note1 <-  paste('Monte Carlo Approximation with', self$options$nsamples, 'samples was applied. <i>p</i>-value might differ for each execution.')
      }

      if(self$options$cc){
        note2 <-  '
        The use of the continuity correction is generally not recommended, if an exact test is possible.
        We recommend using the exact test instead.
        '
      }

      ## Paste the notes together
      ## ("" ) is so that the string is never empty, which would lead to 'character(0)'
      if(is.null(note1) & !is.null(note2)) {
        # print("note2")
        table$setNote('remark', note2)
      } else if(is.null(note2) & !is.null(note1)) {
        # print("note1")
        table$setNote('remark', note1)
      } else if(!is.null(c(note1, note2))) {
        # print("beide")
        note <- paste('a)', note1, "<br> b)", note2)
        table$setNote('remark', note)
      } else{print('keine')}


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





#   alter <- results$alternative
#   table$addFootnote(rowNo = 1,
#                     "p",
#                     alter)
#   
