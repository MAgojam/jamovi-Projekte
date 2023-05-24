self <- list()
self$data <- data.frame(ID = as.factor(1:7),
                        Werte = c(4,6,8, 2,4,4,6), 
                        Gruppe = factor(c(1,1,1, 2,2,2,2)))
self$data <- data.frame(ID = as.factor(1:7),
                        Werte = c(1,4,9, 3,6,7,8), 
                        Gruppe = factor(c(1,1,1, 2,2,2,2)))
self$options$exact <- TRUE
self$options$app <- TRUE
self$options$asy <- TRUE
self$options$cc <- TRUE
self$options$exact <- TRUE
self$options$dep <- "Werte"
self$options$group <- "Gruppe"
self$options$alternative <- "greater"
self$options$correct <- TRUE
self$options$rs1 <- TRUE
self$options$u <- TRUE
self$options$rankmean <- TRUE
self$options$median <- TRUE







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
# 
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
if(self$options$rs1) {
  RS1 <- data_ranked |>                           # get data
    dplyr::filter(Group == gr1) |>                # filter for first level
    dplyr::select(Ranks) |>                       # select ranks
    sum()                                         # sum of ranks
}


## calculate Mann-Whitney U if selected
if(self$options$u) {
  ## Get n1 and n2 by ordering the count-table by Freq, selecting the second column,
  ## and then assigning the 1st and 2nd value respectively
  n1 <- count[order(count$Freq), 2][1]
  n2 <- count[order(count$Freq), 2][2]
  u <- RS1 - ((n1+1)*n1/2)
}



## get descriptives
#### mean ranks per group
if(self$options$rankmean) {
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
}



#### median per group
if(self$options$median) {
  median_g1 <- data |>
    dplyr::filter(data[[group]] == gr1) |>
    dplyr::select(all_of(dep)) |>
    unlist() |>  
    as.vector() |>
    median()
  
  median_g2 <- data |>
    dplyr::filter(data[[group]] != gr1) |>
    dplyr::select(all_of(dep)) |>
    unlist() |> 
    as.vector() |>
    median()
}

#### get descriptives for plot
sd1 <- data |> 
  dplyr::filter(data[[group]] == gr1) |> 
  dplyr::select(all_of(dep)) |> 
  unlist() |> 
  sd()

sd2 <- data |> 
  dplyr::filter(data[[group]] != gr1) |> 
  dplyr::select(all_of(dep)) |> 
  unlist() |> 
  sd()

se1 <- sd1/sqrt(n1)
se2 <- sd2/sqrt(n2)


plotData <- data.frame(Group = rep(c("Group 1", "Group 2"), 2),
                       Values = c(rankmean_R1, rankmean_R2, median_g1, median_g2),
                       se = c(se1, se2, NA, NA),
                       type = c("rankmean", "rankmean", "median", "median"))


data |> 
  ggplot(aes(x = Gruppe, 
             y = Werte, 
             fill = Gruppe)) + 
  geom_boxplot() +
  scale_fill_viridis_d(alpha = 0.6) +
  geom_jitter(color = "black", 
              size = 1,
              width = 0.1,
              alpha = 0.9) +
  theme(legend.position = "none")



pd = position_dodge(0.2)

ggplot(data = plotData, 
       aes(x = Group, 
           y = means, 
           shape = type)) + 
  geom_errorbar(data = plotData,
                aes(x = Group, 
                    ymin = means - se, 
                    ymax = means + se, 
                    width = .2), 
                linewidth = .8, 
                position = pd) + 
  geom_point(data = plotData,
             aes(x = Group, 
                 y = means), 
             size = 3,
             position = pd) +
  labs(x = group, y = dep) +
  scale_shape_manual(name = '',
                     values = c(rankmean = 21,
                                median = 22),
                     labels = c(median = "Median",
                                rankmean = "Rankmean")) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(.2, "cm"),
        legend.text = element_text(size = 16),
        plot.margin = margin(5.5, 5.5, 5.5, 5.5))

########## end of general statistics and descriptives


########## start of exact analysis
if (self$options$exact) {
  
  ## wurde auskommentiert, da jetzt der exakte Test schon weiter oben gemacht wird für die z-Statistik
  
  # results <- try(coin::wilcox_test(formula = formula,
  #                                  data = data,
  #                                  distribution = "exact",
  #                                  alternative = self$options$alternative),
  #                silent = TRUE)
  
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
                   "type[ex]" = "Exact",
                   "stat[ex]" = zval,
                   "rs1[ex]" = RS1,
                   "u[ex]" = u,
                   "p[ex]" = coin::pvalue(results)
                 ))
    
  }
  
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
                   "type[app]" = 'Approximate',
                   "stat[app]" = zval,
                   "rs1[app]" = RS1,
                   "u[app]" = u,
                   "p[app]" = coin::pvalue(results)
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
                   "type[asy]" = 'Asymptotic',
                   "stat[asy]" = zval,
                   "rs1[asy]" = RS1,
                   "u[asy]" = u,
                   "p[asy]" = results$p.value
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
  note1 <-  paste('Monte Carlo Approximation with', self$options$nsample, 'samples was applied. <i>p</i>-value might differ for each execution.')
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











