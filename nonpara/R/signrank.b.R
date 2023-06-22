
# This file is a generated template, your changes will not be overwritten

#' @export

signrankClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "signrankClass",
  inherit = signrankBase,
  private = list(
    .run = function() {
      
      ############################### To Do ###############################
      # - Testen ob alle fehlerhaften Datenytypen abgefangen werden
      # - am Schluss im Code und im r.yaml results$control entfernen
      # - ig ha gad random chönne ID (nominal) bi dependent drizieh... sött eig nid müglech si
      #   GLOUBS da chame nüt mache ussert uf intelligenti Benutzer hoffe
      # - sign-rank test, sign rank test, signed-rank test or signed rank test?
      # - zero.method als auswählbare option einbauen
      #   Das wirkt sich aber wahrsch. auf die Daten aus
      #   d.h. ich müsste die Daten-Manipulation auch anpassen.
      #   Braucht zudem einen Hinweis, welche zeroMethod verwendet wurde,
      #   auch wenn immer nur Wilcoxon verwendet wird.
      # - entscheiden ob Anmerkungen unter der Tabelle als Note oder als Footnote
      #   oder auch einfach als eigenes Element wie die Warnings
      # - Berechnung der Effektstärke und des Konfidenzintervalls anpassen
      #   STATUS: eigentlich fertig, aber funktioniert erst mit jmv v2.4,
      #   was anscheinend bald released werden soll.
      # - In Bortz & Lienert (2008) steht auf S. 193 (PDF s.207),
      #   dass z = (W+ - E(W+)) / Var(W+) ist, bzw. bei Kontinuitätskorrektur
      #   z = (|W+ - E(W+)| - 0.5) / Var(W+)
      #   1. Müsste demfall beim WRS-test auch ein anderer z-Wert resultieren
      #      für den Fall mit CC?
      #   2. Is that even true? Sollte ich das auch so übernehmen?
      #      Actually könnte ich ja wirklich anstatt mit stats::wilcox.test()
      #      einfach selber den asymptotischen Test machen, der ist ja easy.
      # - Hinweis auf nicht-Verwendung der CC hinzufügen
      # - Berechnung von W+: rank(diff) weist einer Differenz von 0
      #   auch einen Rang zu (logischerweise), aber eig werden ja die
      #   Differenzen von 0 einfach entfernt oder?
      # - Berechnung von Erwartungswert und Varianz stimmen noch nicht, 
      #   für den Fall mit Ties ist es eine andere Formel
      # - Hinweis, dass Null-Differenzen ausgeschlossen werden,
      #   sollte nicht erst bei den Deskriptiv-Statistiken stehen,
      #   sondern bereits bei der Haupt-Tabelle als Hinweis stehen.
      #####################################################################
      
      
      
      ########## start of data preparation
      # get ID, dep and samp
      ID <- self$options$id
      dep <- self$options$dep
      samp <- self$options$samp
      ciWidth <- self$options$ciWidth / 100
      zeroMethod <- self$options$zeroMethod
      
      # preparation for possible addition of CI-type selection and number of bootstraps
      ciType <- "perc"
      nboot <- 1000
      
      
      if(is.null(dep) || is.null(samp) || is.null(ID)) {
        return()  # do nothing, as long as not all of samp, dep and id are specified
      } 
      
      # create dataset
      data <- data.frame(ID = self$data[[ID]],
                         dep = self$data[[dep]],
                         samp = self$data[[samp]])
      
      # clean ID
      data$ID <- factor(data$ID, ordered = FALSE) # necessary for ggplot
      if(any(table(data$ID) > 2)) {
        jmvcore::reject("More than 2 instances of an ID were found. This test is only for two samples. Please check your dataset.")
      } else if (all(table(data$ID) == 1)) {
        jmvcore::reject("Only 1 instance found for each ID. Please make sure your data is in long-format.")
      }
      
      # clean dependent variable
      data$dep <- jmvcore::toNumeric(data$dep)
      
      # clean sample / grouping variable
      ## try formating the sample variable as factor and suppress error if not possible
      samErr <- try(factor(data$samp, ordered = FALSE),
                    silent = TRUE)
      if(jmvcore::isError(samErr)) { # throw error message if factoring is unsuccessful
        jmvcore::reject("Unable to determine factors of grouping variable. Please check for missing values.")
      } else if(length(base::levels(samErr)) != 2) {
        jmvcore::reject("Grouping variable must have exactly 2 levels.")
      } else { # if successful, overwrite sample as factor
        data$samp <- factor(data$samp, ordered = FALSE)
        sampLevels <- base::levels(data$samp) #count extract levels
      }
      
      
      # find missings and print warning if any are found
      if(anyNA.data.frame(data)){
        self$results$na_warning$setContent("Missings found in dataset. Corresponding observations were removed.")
        self$results$na_warning$setTitle("Warning")
        # after warning, remove rows with missings and set visibility to TRUE
        data <- jmvcore::naOmit(data)
        self$results$na_warning$setVisible(visible = TRUE)
      }
      
      # counts all IDs and if there are less than 2 of any ID, the ID is stored
      NAid <- data$ID |>                     # take ID
        table() |>                           # creates frequency table
        as.data.frame() |>                   # to dataframe for easier filtering
        dplyr::filter(Freq < 2) |>           # get IDs with frequency < 2
        `colnames<-`(c("ID", "Freq")) |>     # rename variables for easier selection
        dplyr::select(ID) |>                 # select the ID   
        unlist() |>                          # extract values
        as.vector()                          # format as vector for the next step
      
      # remove als IDs with less than two instances
      if(length(NAid) > 0) {                 # only necessary if NAid is not empty
        data <- data |>                      # 
          dplyr::filter(!data$ID %in% NAid)  # filter for all IDs that are not in NAid
      }
      
      
      ############### This now probably has to be done further below,
      ############### at least after the calculation of W+,
      ############### because the full dataset is needed for the ranking in Pratt-Method
      ############### Also, the 0-differences are removed in either case, 
      ############### so the if-else statement is not really necessary probably
      #
      # Preparation of addition of zero.method = "Pratt"
      # if(zeroMethod == "Wilcoxon") {
      
      # find IDs which have identical values for both samples and remove them from the dataset
      ## for each ID get the according dependent values
      # for (id in data$ID) {
      #   vals <- data |> 
      #     dplyr::filter(ID == id) |> 
      #     dplyr::select(dep) |>  
      #     unlist() |>
      #     as.vector() 
      #   # if the dependent vals are equal, remove them from the dataset
      #   if(all(vals == vals[1])) { 
      #     data <- data[data$ID != id,]
      #   }
      # }
      # } else if(zeroMethod == "Pratt") { TBD }
      # 
      # 
      ########### what happens to the calculations relying on g1 and g2 if this is moved down?
      
      
      # sort data for group then for ID
      data <- dplyr::arrange(data, data$samp, data$ID)
      
      # of the dependent variables, take those that have level 1 for samp as samp1
      g1 <- data$dep[data$samp == sampLevels[1]]
      g2 <- data$dep[data$samp == sampLevels[2]]
      
      # prepare output tables
      table <- self$results$srtest
      desk <- self$results$desc
      
      # self$results$control$setContent(data)
      ########## end of data preparation 
      
      
      
      ########## start of general  statistics and descriptives
      # calculate the observed n 'nobs'
      # 
      ############### actually this is now handled just below
      # 
      # if(all.equal(length(g1), length(g2), nrow(data)/2)) {
      #   nobs <- length(g1)
      # } else {
      #   nobs <- NA
      # }
      
      
      # calculate W+
      diffs <- g1 - g2
      abs_diffs <- abs(diffs)
      pos_abs_diffs <- abs_diffs > 0
      if (all(abs_diffs < 1)){
        jmvcore::reject("All pairwise differences equal zero.")
      }
      if (zeroMethod == "Pratt") {
        rank_abs_diffs <- rank(abs_diffs)
        pos <- (rank_abs_diffs * (diffs > 0))[pos_abs_diffs]
        neg <- (rank_abs_diffs * (diffs < 0))[pos_abs_diffs]
        
      } else if (zeroMethod == "Wilcoxon"){
        diffs <- diffs[pos_abs_diffs]
        abs_diffs <- abs_diffs[pos_abs_diffs]
        rank_abs_diffs <- rank(abs_diffs)
        pos <- rank_abs_diffs * (diffs > 0)
        neg <- rank_abs_diffs * (diffs < 0)
      } else {
        jmvcore::reject("No zero-method selected.")
      }
      
      n <- length(pos)
      Wp <- sum(pos)
      tieVec <- table(pos) # not yet sure if this is the right way to calculate the tievec
      tieCorr <- sum(tieVec^3-tieVec)/48 
      ew <- (n*(n+1)) / 4
      vw <- ((n*(n+1)*(2*n+1)) / 24) - tieCorr
      z <- ((Wp - ew)) / sqrt(vw)
      zcc <- (abs(Wp-ew) - 0.5) / sqrt(vw)
      
      
      # set the table's title according to the zero-method selected
      if(zeroMethod == "Pratt") {
        table$setTitle("Wilcoxon-Pratt Signed Rank Test")
      }
      
      
      ########### This was moved down from further up. Need to check that this does not mess up any other calculations.
      ########### The analyses rely on g1 and g2 which is calculated from the unfiltered dataset,
      ########### but it SHOULD not matter as these functions clean up the data themselves.
      ########### only stats::wilcox.test might throw issues, but I think with exact = F it's okay.
      
      # find IDs which have identical values for both samples and remove them from the dataset
      # for each ID get the according dependent values
      for (id in data$ID) {
        vals <- data |>
          dplyr::filter(ID == id) |>
          dplyr::select(dep) |>
          unlist() |>
          as.vector()
        # if the dependent vals are equal, remove them from the dataset
        if(all(vals == vals[1])) {
          data <- data[data$ID != id,]
        }
      }
      
      
      # rstatix is currently not installable within jamovi but will be soon.
      # in the meantime, fake values will be displayed:
      
      effsize <- 0.5
      ciLower <- 0.25
      ciUpper <- 0.75
      
      # # This function bootstraps the CI, which takes a few seconds in R
      # # and might take even longer in jamovi. Maybe find a way to
      # # calculate it without bootstrapping, or reduce bootstrap-count?
      # confi <- rstatix::wilcox_effsize(formula = dep ~ samp,
      #                                  data = data,
      #                                  paired = TRUE,
      #                                  alternative = self$options$alternative,
      #                                  ci = TRUE,
      #                                  conf.level = ciWidth,
      #                                  ci.type = ciType,
      #                                  nboot = nboot)
      # 
      # effsize <- confi$effsize
      # ciLower <- confi$conf.low
      # ciUpper <- confi$conf.high
      # Preparation for possible inclusion of magnitude:
      # ciMag <- confi$magnitude
      
      
      ## get descriptives if selected
      if(self$options$descriptives) {
        try_desk <- try({
          #### median per samp
          median_g1 <- data |>
            dplyr::filter(samp == sampLevels[1]) |>
            dplyr::select(dep) |>
            unlist() |>
            as.vector() |>
            median() |> 
            format(nsmall = 2)
          
          median_g2 <- data |>
            dplyr::filter(samp == sampLevels[2]) |> 
            dplyr::select(dep) |>
            unlist() |>
            as.vector() |>
            median() |> 
            format(nsmall = 2)
          
        }, silent = TRUE)
        
        if(jmvcore::isError(try_desk)) {
          desk$setRow(rowNo = 1,
                      values = list(
                        "dep" = "",
                        "nobs[1]" = "",
                        "nobs[2]" = "",
                        
                        "time[1]" = "",
                        "time[2]" = "",
                        
                        "median[1]" = "",
                        "median[2]" = "",
                        
                        "ev[1]" = "",
                        "ev[2]" = "",
                        
                        "var[1]" = "",
                        "var[2]" = ""
                      ))
        } else {
          desk$setRow(rowNo = 1,
                      values = list(
                        "dep" = dep,
                        "nobs[1]" = n,
                        "nobs[2]" = n,
                        
                        "time[1]" = sampLevels[1],
                        "time[2]" = sampLevels[2],
                        
                        "median[1]" = median_g1,
                        "median[2]" = median_g2,
                        
                        "ev[1]" = ew,
                        "ev[2]" = ew,
                        
                        "var[1]" = vw,
                        "var[2]" = vw
                      ))
          
          note_obs <- "Observations with identical values for both samples are disregarded for this test and do therefore not count as observations."
          desk$setNote(key = 'observs', note = note_obs)
        }
      }
      
      
      if(self$options$plot) {
        self$results$plot$setState(data)
      }
      ########## end of general statistics and descriptives
      
      
      ########## start of analysis
      ####       Exakt
      if(self$options$get("exact")) {
        exakt <- try(coin::wilcoxsign_test(formula = g1 ~ g2,
                                           distribution = "exact",
                                           zero.method = zeroMethod,
                                           alternative = self$options$alternative),
                     silent = TRUE)
        
        if(jmvcore::isError(exakt)) {
          
          table$setRow(rowNo = 1,
                       values = list(
                         var = "",
                         "type[exact]" = "",
                         "stat[exact]" = "",
                         "Wp[exact]" = "",
                         "nobs[exact]" = "",
                         "p[exact]" = "",
                         "es[exact]" = "",
                         "ciles[exact]" = "",
                         "ciues[exact]" = ""
                       ))
        } else {
          table$setRow(rowNo = 1, 
                       values = list(
                         var = self$options$dep,
                         "type[exact]" = "Exact",
                         "stat[exact]" = coin::statistic(exakt),
                         "Wp[exact]" = Wp,
                         "nobs[exact]" = n,
                         "p[exact]" = coin::pvalue(exakt),
                         "es[exact]" = effsize,
                         "ciles[exact]" = ciLower,
                         "ciues[exact]" = ciUpper
                       ))
        }
      }
      
      ####       Monte-Carlo
      if(self$options$get("approximate")) {
        mc <- try(coin::wilcoxsign_test(formula = g1 ~ g2,
                                        distribution = "approximate",
                                        nsamples = self$options$nsamples,
                                        zero.method = zeroMethod,
                                        alternative = self$options$alternative),
                  silent = TRUE)
        
        if(jmvcore::isError(mc)) {
          
          table$setRow(rowNo = 1,
                       values = list(
                         var = "",
                         "type[approximate]" = "",
                         "stat[approximate]" = "",
                         "Wp[approximate]" = "",
                         "nobs[approximate]" = "",
                         "p[approximate]" = "",
                         "es[approximate]" = "",
                         "ciles[approximate]" = "",
                         "ciues[approximate]" = ""
                       ))
        } else {
          table$setRow(rowNo = 1, 
                       values = list(
                         var = self$options$dep,
                         "type[approximate]" = "Monte-Carlo Approximation",
                         "stat[approximate]" = coin::statistic(mc),
                         "Wp[approximate]" = Wp,
                         "nobs[approximate]" = n,
                         "p[approximate]" = coin::pvalue(mc),
                         "es[approximate]" = effsize,
                         "ciles[approximate]" = ciLower,
                         "ciues[approximate]" = ciUpper
                       ))
        }
      }
      
      ####       Asymptotic without CC
      if(self$options$get("asymptotic")) {
        asymp <- try(coin::wilcoxsign_test(formula = g1 ~ g2,
                                           distribution = "asymptotic",
                                           zero.method = zeroMethod,
                                           alternative = self$options$alternative),
                     silent = TRUE)
        
        if(jmvcore::isError(asymp)) {
          
          table$setRow(rowNo = 1,
                       values = list(
                         var = "",
                         "type[asymptotic]" = "",
                         "stat[asymptotic]" = "",
                         "Wp[asymptotic]" = "",
                         "nobs[asymptotic]" = "",
                         "p[asymptotic]" = "",
                         "es[asymptotic]" = "",
                         "ciles[asymptotic]" = "",
                         "ciues[asymptotic]" = ""
                       ))
        } else {
          table$setRow(rowNo = 1, 
                       values = list(
                         var = self$options$dep,
                         "type[asymptotic]" = "Asymptotic",
                         "stat[asymptotic]" = coin::statistic(asymp),
                         "Wp[asymptotic]" = Wp,
                         "nobs[asymptotic]" = n,
                         "p[asymptotic]" = coin::pvalue(asymp),
                         "es[asymptotic]" = effsize,
                         "ciles[asymptotic]" = ciLower,
                         "ciues[asymptotic]" = ciUpper
                       ))
        }
      }
      
      ####       Asymptotic with CC
      
      # allenfalls diesen Teil ersetzen durch pnorm(zcc) bzw. pnorm(-zcc),
      # glaube es ist abhängig von der Richtung der gewählten Hypothese.
      # dann spielt es keine Rolle mehr dass hier keine zero-method gewählt werden kann,
      # weil zcc basierend auf dem Datensatz berechnet wird, 
      # auf den schon die gewählte zeroMethod angewendet wurde.
      if(self$options$get("cc")) {
        asymp_cc <- try(stats::wilcox.test(x = g1,
                                           y = g2,
                                           paired = TRUE,
                                           exact = FALSE,
                                           correct = TRUE,
                                           alternative = self$options$alternative),
                        silent = TRUE)
        
        if(jmvcore::isError(asymp_cc)) {
          
          table$setRow(rowNo = 1,
                       values = list(
                         var = "",
                         "type[cc]" = "",
                         "stat[cc]" = "",
                         "Wp[cc]" = "",
                         "nobs[cc]" = "",
                         "p[cc]" = "",
                         "es[cc]" = "",
                         "ciles[cc]" = "",
                         "ciues[cc]" = ""
                       ))
        } else {
          table$setRow(rowNo = 1, 
                       values = list(
                         var = self$options$dep,
                         "type[cc]" = "Asymptotic (CC)",
                         # "stat[cc]" = z,
                         "stat[cc]" = stats::qnorm(asymp_cc$p.value),
                         "Wp[cc]" = Wp,
                         "nobs[cc]" = n,
                         "p[cc]" = asymp_cc$p.value,
                         "es[cc]" = effsize,
                         "ciles[cc]" = ciLower,
                         "ciues[cc]" = ciUpper
                       ))
        }
      }
      ########## end of analysis
      
      
      
      ########## start of warnings
      # Warnings / remarks
      ## Empty note-object
      note1 <- note2 <- c()
      
      ## Write a note, if these conditions are met
      note1 <- 'Test statistic <i>W</i><sub>+</sub> is calculated as the number of positive differences between values of sample 1 - sample 2.'
      
      if(self$options$approximate){
        note2 <-  paste('Monte Carlo Approximation with', 
                        self$options$nsamples, 
                        'samples was applied. <i>p</i>-value might differ for each execution.')
      }
      
      ## Paste the notes together
      ## ("" ) is so that the string is never empty, which would lead to 'character(0)'
      if(is.null(note2)) {
        table$setNote('remark', note1)
      } else {
        note <- paste('a)', note1, "<br> b)", note2)
        table$setNote('remark', note)
      }
      ########## end of warnings
    },
    
    
    
    
    
    .descplot = function(image, ggtheme, theme...) {
      
      if(is.null(self$options$id) || is.null(self$options$dep) || is.null(self$options$samp)) {
        return()  # do nothing, as long as not all of samp, dep and id are specified
      }
      
      plot <- ggplot(data = image$state,
                     aes(x = samp,
                         y = dep,
                         fill = samp)) + 
        geom_boxplot(outlier.shape = 1,
                     outlier.size = 2) +
        labs(x = self$options$samp, 
             y = self$options$dep) +
        theme_classic() +
        ggtheme +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 16),
              axis.ticks.length = unit(.2, "cm"),
              legend.position = "none",
              plot.margin = margin(5.5, 5.5, 5.5, 5.5))
      
      if(self$options$observed == "line"){
        plot <- plot +
          geom_line(aes(color = ID, group = ID)) +
          geom_point()
      } else if(self$options$observed == "jitter"){
        plot <- plot +
          geom_jitter(color = "black",
                      size = 1,
                      width = 0.1,
                      alpha = 0.9)
      }
      
      print(plot)
      TRUE
      
    },
    
    
    .init=function() {
      
      table <- self$results$get("srtest")
      
      ciTitle <- jmvcore::format('{ciWidth}% Confidence Interval', 
                                 ciWidth = self$options$ciWidth)
      
      table$getColumn('ciles[exact]')$setSuperTitle(ciTitle)
      table$getColumn('ciues[exact]')$setSuperTitle(ciTitle)
      table$getColumn('ciles[approximate]')$setSuperTitle(ciTitle)
      table$getColumn('ciues[approximate]')$setSuperTitle(ciTitle)
      table$getColumn('ciles[asymptotic]')$setSuperTitle(ciTitle)
      table$getColumn('ciues[asymptotic]')$setSuperTitle(ciTitle)
      table$getColumn('ciles[cc]')$setSuperTitle(ciTitle)
      table$getColumn('ciues[cc]')$setSuperTitle(ciTitle)
      
      
    }
  )
)
