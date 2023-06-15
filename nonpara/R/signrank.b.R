
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
      # - continuity-correction für asymptotischen test einbauen (wilcox.test())
      # - zero.method als auswählbare option einbauen?
      #   Das wirkt sich aber wahrsch. auf die Daten aus
      #   d.h. ich müsste die Daten-Manipulation auch anpassen.
      # - Berechnung der Teststatistik anpassen: W+ nicht S. Ändern bei den Results
      #   und auch in den .yaml-files
      # - Berechnung der Effektstärke und des Konfidenzintervalls anpassen
      # - Interpretation für Effektstärke hinzufügen?
      # - Welche Bootstrap-Methode soll man nehmen? Oder user wählen lassen?
      #####################################################################
      
      
      
      ########## start of data preparation
      # get ID, dep and samp
      ID <- self$options$id
      dep <- self$options$dep
      samp <- self$options$samp
      ciWidth <- self$options$ciWidth / 100
      
      # preparation for later addition of zero.method = "Pratt"
      zero <- "Wilcoxon"
      
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
      
      
      
      # find IDs which have identical values for both samples and remove them from the dataset
      ## for each ID get the according dependent values
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
      if(all.equal(length(g1), length(g2), nrow(data)/2)) {
        nobs <- length(g1)
      } else {
        nobs <- NA
      }
      
      
      # calculate W+
      diff <- g1 - g2
      idiff <- sign(diff)
      idiff[idiff < 0] <- 0
      rdiff <- rank(abs(diff))
      Wp <- sum(idiff * rdiff)
      
      
      # calculate expected Wp, variance of Wp and z
      expS <- (nobs * (nobs+1)) / 4
      varS <- (nobs * (nobs+1) * (2*nobs+1)) / 24
      
      # calculate effect size of exact test
      
      # rstatix is currently not installable within jamovi but will be soon.
      # in the meantime, fake values will be displayed:
      
      effsize <- 0.5
      ciLower <- 0.25
      ciUpper <- 0.75
      
      # This function bootstraps the CI, which takes a few seconds in R
      # and might take even longer in jamovi. Maybe find a way to
      # calculate it without bootstrapping, or reduce bootstrap-count?
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
                        "nobs[1]" = nobs,
                        "nobs[2]" = nobs,
                        
                        "time[1]" = sampLevels[1],
                        "time[2]" = sampLevels[2],
                        
                        "median[1]" = median_g1,
                        "median[2]" = median_g2,
                        
                        "ev[1]" = expS,
                        "ev[2]" = expS,
                        
                        "var[1]" = varS,
                        "var[2]" = varS
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
      exakt <- try(coin::wilcoxsign_test(formula = g1 ~ g2,
                                         distribution = "exact",
                                         zero.method = zero,
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
                       "nobs[exact]" = nobs,
                       "p[exact]" = coin::pvalue(exakt),
                       "es[exact]" = effsize,
                       "ciles[exact]" = ciLower,
                       "ciues[exact]" = ciUpper
                     ))
      }
      
      
      ####       Monte-Carlo
      mc <- try(coin::wilcoxsign_test(formula = g1 ~ g2,
                                      distribution = "approximate",
                                      nsamples = self$options$nsamples,
                                      zero.method = zero,
                                      alternative = self$options$alternative),
                silent = TRUE)
      
      if(jmvcore::isError(exakt)) {
        
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
                       "nobs[approximate]" = nobs,
                       "p[approximate]" = coin::pvalue(mc),
                       "es[approximate]" = effsize,
                       "ciles[approximate]" = ciLower,
                       "ciues[approximate]" = ciUpper
                     ))
      }
      
      
      ####       Asymptotisch
      asymp <- try(coin::wilcoxsign_test(formula = g1 ~ g2,
                                         distribution = "asymptotic",
                                         zero.method = zero,
                                         alternative = self$options$alternative),
                   silent = TRUE)
      
      if(jmvcore::isError(exakt)) {
        
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
                       "nobs[asymptotic]" = nobs,
                       "p[asymptotic]" = coin::pvalue(asymp),
                       "es[asymptotic]" = effsize,
                       "ciles[asymptotic]" = ciLower,
                       "ciues[asymptotic]" = ciUpper
                     ))
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
      
      
    }
  )
)
