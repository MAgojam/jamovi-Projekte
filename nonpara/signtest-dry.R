# Options init
self <- list()

self$data <- data.frame(
  ID = as.factor(rep(1:6, times = 2)),
  Blutdruck = c(130.2, 180.2, 149.6, 153.2, 162.6, 160.1,
                136.9, 201.4, 166.8, 150.0, 173.2, 169.3),
  Horrorfilm = factor(rep(c("vorher","nachher"), each = 6),
                      levels = c("vorher", "nachher")))

# self$data <- data.frame(
#   ID = as.factor(rep(1:7, times = 2)),
#   Blutdruck = c(6,4,1,4,10,3,8,
#                 3,9,2,4, 3,1,4),
#   Horrorfilm = factor(rep(c("vorher","nachher"), each = 7),
#                       levels = c("vorher", "nachher")))
# 
# self$data$Horrorfilm[13:14] <- NA


# self$data <- data.frame( 
#   ID = as.factor(rep(1:6, times = 2)),
#   Blutdruck = c(1,2,3,4,5,6,
#                 1,2,4,4,5,6),
#   Horrorfilm = factor(rep(c("vorher","nachher"), each = 6),
#                       levels = c("vorher", "nachher")))




# self$data <- data.frame( 
#   ID = as.factor(rep(1:7, times = 2)),
#   Blutdruck = c(6,4,1,4,10,3,8,
#                 3,9,2,4, 3,1,4),
#   Horrorfilm = factor(rep(c("vorher","nachher"), each = 7),
#                       levels = c("vorher", "nachher")))

self$options$exact <- TRUE
self$options$approximate <- TRUE
self$options$nsamples <- 10000
self$options$asymptotic <- TRUE
self$options$dep <- "Blutdruck"
self$options$samp <- "Horrorfilm"
self$options$id <- "ID"
self$options$alternative <- "less"

self$options$descriptives <- FALSE
self$options$plot <- TRUE
self$options$observed <- "line"

self$options$nobs <- TRUE
self$options$effectSize <- TRUE
self$options$ciES <- TRUE
self$options$ciWidth <- 95


# self$data[13, 1] <- NA
# self$data[14, 1] <- NA


# df <- data.frame(ID = rep(1:200,
#                           times = 2),
#                  values = c(runif(200,150, 200),
#                                 runif(200, 145, 195)),
#                  group = factor(rep(c("vorher","nachher"),
#                                     each = 200),
#                                 levels = c("vorher", "nachher")))

# self$data <- df
# self$options$id <- "ID"
# self$options$dep <-  "values"
# self$options$group <- "group"










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
  # self$results$na_warning$setContent("Missings found in dataset. Corresponding observations were removed.")
  # self$results$na_warning$setTitle("Warning")
  # after warning, remove rows with missings
  data <- jmvcore::naOmit(data)
  
} else {
  # self$results$na_warning$setVisible(visible = FALSE)
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
table <- self$results$stest
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


# calculate s
s <- 0

# if a value is greater in sample 1 than in sample 2, increase s by 1
for(i in 1:length(g1)) { 
  if(g1[i]  > g2[i]) { s = s + 1 }
}


# calculate expected s, variance of s and z
eS <- nobs * 0.5
varS <- nobs * 0.25
z <- (nobs - eS) / sqrt(varS)

# calculate effect size of exact test if selected
if(self$options$effectSize) {
  pi0 <- 0.5
  pi <- s/nobs
  effsize <- pi - pi0
  
  if(self$options$ciES){
    ci <- c(((2*nobs*pi + z^2 - (z*sqrt(z^2+4*nobs*pi*(1-pi)))) /
               (2*(nobs+z^2)) - 0.5),
            ((2*nobs*pi + z^2 + (z*sqrt(z^2+4*nobs*pi*(1-pi)))) /
               (2*(nobs+z^2)) - 0.5))
    ciLower <- min(ci)
    ciUpper <- max(ci)
  }
  
  # self$results$control$setContent(effsize)
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
    # desk$setRow(rowNo = 1,
    #             values = list(
    #               "dep" = "",
    #               "nobs[1]" = "",
    #               "nobs[2]" = "",
    #               
    #               "time[1]" = "",
    #               "time[2]" = "",
    #               
    #               "median[1]" = "",
    #               "median[2]" = ""
    #             ))
  } else {
    # desk$setRow(rowNo = 1,
    #             values = list(
    #               "dep" = dep,
    #               "nobs[1]" = nobs,
    #               "nobs[2]" = nobs,
    #               
    #               "time[1]" = sampLevels[1],
    #               "time[2]" = sampLevels[2],
    #               
    #               "median[1]" = median_g1,
    #               "median[2]" = median_g2
    #             ))
    
    note_obs <- "Observations with identical values for both samples are disregarded for this test and do therefore not count as observations."
    # desk$setNote(key = 'observs', note = note_obs)
  }
}


if(self$options$plot) {
  # self$results$plot$setState(data)
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
  
  # table$setRow(rowNo = 1,
  #              values = list(
  #                var = "",
  #                "type[exact]" = "",
  #                "stat[exact]" = "",
  #                "s[exact]" = "",
  #                "nobs[exact]" = "",
  #                "p[exact]" = "",
  #                "es[exact]" = "",
  #                "ciles[exact]" = "",
  #                "ciues[exact]" = ""
  #              ))
} else {
  # table$setRow(rowNo = 1, 
  #              values = list(
  #                var = self$options$dep,
  #                "type[exact]" = "Exact",
  #                "stat[exact]" = z,
  #                "s[exact]" = s,
  #                "nobs[exact]" = nobs,
  #                "p[exact]" = coin::pvalue(exakt),
  #                "es[exact]" = effsize,
  #                "ciles[exact]" = ciLower,
  #                "ciues[exact]" = ciUpper
  #              ))
}


####       Monte-Carlo
mc <- try(coin::sign_test(formula = g1 ~ g2,
                          data = data,
                          distribution = "approximate",
                          nsamples = self$options$nsamples,
                          alternative = self$options$alternative),
          silent = TRUE)

if(jmvcore::isError(exakt)) {
  
  # table$setRow(rowNo = 1,
  #              values = list(
  #                var = "",
  #                "type[approximate]" = "",
  #                "stat[approximate]" = "",
  #                "s[approximate]" = "",
  #                "nobs[approximate]" = "",
  #                "p[approximate]" = "",
  #                "es[approximate]" = "",
  #                "ciles[approximate]" = "",
  #                "ciues[approximate]" = ""
  #              ))
} else {
  # table$setRow(rowNo = 1, 
  #              values = list(
  #                var = self$options$dep,
  #                "type[approximate]" = "Monte-Carlo Approximation",
  #                "stat[approximate]" = z,
  #                "s[approximate]" = s,
  #                "nobs[approximate]" = nobs,
  #                "p[approximate]" = coin::pvalue(mc),
  #                "es[approximate]" = effsize,
  #                "ciles[approximate]" = ciLower,
  #                "ciues[approximate]" = ciUpper
  #              ))
}


####       Asymptotisch
asymp <- try(coin::sign_test(formula = g1 ~ g2,
                             data = data,
                             distribution = "asymptotic",
                             alternative = self$options$alternative),
             silent = TRUE)

if(jmvcore::isError(exakt)) {
  
  # table$setRow(rowNo = 1,
  #              values = list(
  #                var = "",
  #                "type[asymptotic]" = "",
  #                "stat[asymptotic]" = "",
  #                "s[asymptotic]" = "",
  #                "nobs[asymptotic]" = "",
  #                "p[asymptotic]" = "",
  #                "es[asymptotic]" = "",
  #                "ciles[asymptotic]" = "",
  #                "ciues[asymptotic]" = ""
  #              ))
} else {
  # table$setRow(rowNo = 1, 
  #              values = list(
  #                var = self$options$dep,
  #                "type[asymptotic]" = "Asymptotic",
  #                "stat[asymptotic]" = z,
  #                "s[asymptotic]" = s,
  #                "nobs[asymptotic]" = nobs,
  #                "p[asymptotic]" = coin::pvalue(asymp),
  #                "es[asymptotic]" = effsize,
  #                "ciles[asymptotic]" = ciLower,
  #                "ciues[asymptotic]" = ciUpper
  #              ))
}
########## end of analysis



########## start of warnings
# Warnings / remarks
## Empty note-object
note1 <- note2 <- c()

## Write a note, if these conditions are met
note1 <- 'Test statistic <i>S</i> is calculated as the number of positive differences between values of sample 1 - sample 2.'

if(self$options$approximate){
  note2 <-  paste('Monte Carlo Approximation with', 
                  self$options$nsamples, 
                  'samples was applied. <i>p</i>-value might differ for each execution.')
}

## Paste the notes together
## ("" ) is so that the string is never empty, which would lead to 'character(0)'
if(is.null(note2)) {
  # table$setNote('remark', note1)
} else {
  note <- paste('a)', note1, "<br> b)", note2)
  # table$setNote('remark', note)
}
########## end of warnings


