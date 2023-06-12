
# This file is a generated template, your changes will not be overwritten

#' @export

signtestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "signtestClass",
  inherit = signtestBase,
  private = list(
    .run = function() {
      
      ############################### To Do ###############################
      # - Testen ob alle fehlerhaften Datenytypen abgefangen werden
      # - überprüfen ob df wirklich nur Integer sein kann
      # - am Schluss im Code und im r.yaml results$control entfernen
      #####################################################################
      
      
      
      ########## start of data preparation
      # get ID, dep and samp
      ID <- self$options$id
      dep <- self$options$dep
      samp <- self$options$samp
      
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
        jmvcore::reject("Grouping variable must have exactly 2 levels")
      } else { # if successful, overwrite sample as factor
        data$samp <- factor(data$samp, ordered = FALSE)
        sampLevels <- base::levels(data$samp) #count extract levels
      }
      
      
      # find missings and print warning if any are found
      if(anyNA.data.frame(data)){
        self$results$na_warning$setContent("Missings found in dataset. Corresponding observations were removed.")
        self$results$na_warning$setTitle("Warning")
        # after warning, remove rows with missings
        data <- jmvcore::naOmit(data)
        
      } else {
        self$results$na_warning$setVisible(visible = FALSE)
      }
      
      # counts all IDs and if there are less than 2 of any ID, the ID is stored
      NAid <- data$ID |>                     # take ID
        table() |>                           # creates frequency table
        as.data.frame() |>                   # to df for easier filtering
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
      table <- self$results$vzr
      desk <- self$results$desc
      
      # self$results$control$setContent(data)
      ########## end of data preparation 
      
      
      
      ########## start of general statistics and descriptives
      # calculate df
      if(all.equal(length(g1), length(g2), nrow(data)/2)) {
        df <- length(g1)
      } else {
        df <- NA
      }
      
      
      # calculate s
      s <- 0
      
      # if a value is greater in sample 1 than in sample 2, increase s by 1
      for(i in 1:length(g1)) { 
        if(g1[i]  > g2[i]) { s = s + 1 }
      }
      
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
                        "median[2]" = ""
                      ))
        } else {
          desk$setRow(rowNo = 1,
                      values = list(
                        "dep" = dep,
                        "nobs[1]" = df,
                        "nobs[2]" = df,
                        
                        "time[1]" = sampLevels[1],
                        "time[2]" = sampLevels[2],
                        
                        "median[1]" = median_g1,
                        "median[2]" = median_g2
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
      exakt <- try(coin::sign_test(formula = g1 ~ g2,
                                   data = data,
                                   distribution = "exact",
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
                       var = self$options$dep,
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
                       var = self$options$dep,
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
                       var = self$options$dep,
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
      
    }
  )
)