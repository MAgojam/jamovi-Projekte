# Options init
self <- list()

# self$data <- data.frame( 
#   ID = as.factor(rep(1:6, times = 2)),
#   Blutdruck = c(130.2, 180.2, 149.6, 153.2, 162.6, 160.1,
#                 136.9, 201.4, 166.8, 150.0, 173.2, 169.3), 
#   Horrorfilm = factor(rep(c("vorher","nachher"), each = 6),
#                       levels = c("vorher", "nachher")))

self$data <- data.frame(
  ID = as.factor(rep(1:7, times = 2)),
  Blutdruck = c(6,4,1,4,10,3,8,
                3,9,2,4, 3,1,4),
  Horrorfilm = factor(rep(c("vorher","nachher"), each = 7),
                      levels = c("vorher", "nachher")))

self$data$Horrorfilm[13:14] <- NA


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
# get dep and samp
dep <- self$options$dep
samp <- self$options$samp
id <- self$options$id

if(is.null(dep) || is.null(samp) || is.null(id)) {
  return()  # do nothing, as long as not all of samp, dep and id are specified
} 

# create and control data
data <- as.data.frame(self$data)
data <- data.frame(ID = data[[id]],
                   dep = data[[dep]],
                   samp = data[[samp]])

# clean ID
data$ID <- factor(data$ID, ordered = FALSE)
if(any(table(data$ID) > 2)) {
  jmvcore::reject("More than 2 instances of an ID were found. This test is only for two samples. Please check your dataset.")
}

# clean dependent variable
data$dep <- jmvcore::toNumeric(data$dep)

# clean sample / grouping variable
sampLevels <- try(factor(data$samp, ordered = FALSE),
                  silent = TRUE)
if(jmvcore::isError(sampLevels)) {
  jmvcore::reject("Unable to determine factors of grouping variable. Please check for missing values.",
                  code = "levels_undeterminable")
} else if(length(base::levels(sampLevels)) != 2) {
  jmvcore::reject("Grouping variable must have exactly 2 levels",
                  code = "grouping_var_must_have_2_levels")
} else {
  data$samp <- factor(data$samp, ordered = FALSE)
  sampLevels <- base::levels(data$samp)
}



# find missings and print warnings
if(anyNA.data.frame(data)){
  # self$results$na_warning$setContent("Missings found in dataset. Corresponding observations were removed.")
  # self$results$na_warning$setTitle("Warning")
  # remove rows with missings
  data <- jmvcore::naOmit(data)
  
} 
# else {self$results$na_warning$setVisible(visible = FALSE)}

# counts all IDs and if there are less than 2 of any ID, the ID is stored
NAid <- data$ID |> 
  table() |> 
  as.data.frame() |> 
  dplyr::filter(Freq < 2) |> 
  `colnames<-`(c("ID", "Freq")) |>
  dplyr::select(ID) |>
  unlist() |>
  as.vector()

# remove als IDs with less than two instances
if(length(NAid) > 0) {
  data <- data |> 
    dplyr::filter(!data$ID %in% NAid)
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


# Daten sortieren zuerst nach Gruppe dann nach ID
data <- dplyr::arrange(data, data$samp, data$id)

# of the dependent variables, take those that have level 1 for samp as samp 1
g1 <- data$dep[data$samp == sampLevels[1]]
g2 <- data$dep[data$samp == sampLevels[2]]

# table <- self$results$vzr
# self$results$control$setContent(data)

########## end of data preparation 



########## start of general statistics and descriptives
# calculate df and s
df = 0
s = 0

for(i in 1:length(g1)) {
  # if a value does not change between t1 and t2, 
  # it is excluded from the analysis, so df is reduced by 1
  # coin does this automatically, so no need to change the dataset
  if(g1[i] != g2[i]) { df = df + 1 }  
  if(g1[i]  > g2[i]) { s = s + 1 }
}

## get descriptives if selected
if(self$options$descriptives) {
  try_desk <- try({
    ### n per samp
    nobs <- length(data$samp)/2 # or rather df, I figure?
    
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
    #               "nobs[1]" = nobs,  # or df? see above
    #               "nobs[2]" = nobs,
    #               
    #               "time[1]" = sampLevels[1],
    #               "time[2]" = sampLevels[2],
    #               
    #               "median[1]" = median_g1,
    #               "median[2]" = median_g2
    #             ))
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
  #                "df[exact]" = "",
  #                "p[exact]" = ""
  #              ))
} else {
  # table$setRow(rowNo = 1, 
  #              values = list(
  #                var = self$options$dep,
  #                "type[exact]" = "Exact",
  #                "stat[exact]" = coin::statistic(exakt),
  #                "s[exact]" = s,
  #                "df[exact]" = df,
  #                "p[exact]" = coin::pvalue(exakt)
  #              ))
}

####       Monte carlo
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
  #                "df[approximate]" = "",
  #                "p[approximate]" = ""
  #              ))
} else {
  # table$setRow(rowNo = 1, 
  #              values = list(
  #                var = self$options$dep,
  #                "type[approximate]" = "Monte-Carlo Approximation",
  #                "stat[approximate]" = coin::statistic(mc),
  #                "s[approximate]" = s,
  #                "df[approximate]" = df,
  #                "p[approximate]" = coin::pvalue(mc)
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
  #                "df[asymptotic]" = "",
  #                "p[asymptotic]" = ""
  #              ))
} else {
  # table$setRow(rowNo = 1, 
  #              values = list(
  #                var = self$options$dep,
  #                "type[asymptotic]" = "Asymptotic",
  #                "stat[asymptotic]" = coin::statistic(asymp),
  #                "s[asymptotic]" = s,
  #                "df[asymptotic]" = df,
  #                "p[asymptotic]" = coin::pvalue(asymp)
  #              ))
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
  # table$setNote('remark', note)
}
########## end of warnings



library(ggplot2)

if(is.null(self$options$id) || is.null(self$options$dep) || is.null(self$options$samp)) {
  return()  # do nothing, as long as not all of samp, dep and id are specified
}

plot <- ggplot(data = data,
               aes(x = samp,
                   y = dep,
                   fill = samp)) + 
  geom_boxplot(outlier.shape = 1,
               outlier.size = 2) +
  labs(x = self$options$samp, 
       y = self$options$dep) +
  theme_classic() +
  # ggtheme +
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



