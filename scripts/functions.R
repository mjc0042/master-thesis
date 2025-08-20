library(ordinal)
library(RColorBrewer)
library(lme4)
library(likert)
library(MASS)
library(AER)

# Change graph fonts
#font_import()
#loadfonts(device = "win")


palette_blue <- colorRampPalette(colors = c(
  "#CEDFF0", "#9DC0E2",
  "#6DA0D3", "#3C80C5",
  "#316BA4", "#2A5C8D",
  "#20456A", "#233445",
  "#8b3a01", "#592601",
  "#34b3b9", "#27dee8"))(12)
palette_theme <- colorRampPalette(colors=c("#316ba4","#34b3b9","#233445", "#f25850", "#592601", "#00478d","#027df7","#233445","#27dee8"))(9)
scales::show_col(palette_blue)
 #"#f25850", "#27dee8","#34b3b9","#233445","#00478d","#027df7","#316ba4"

# Run Ordinal regression
#
#
runOrdinalRegression <- function(df, independent, dependent) {
  tempData <- df[which(complete.cases(df[,c(independent, dependent)])),]
  #nullModel <- clm(as.factor(tempData[, dependent]) ~ 1, data=tempData, link="logit")
  #predictor <- as.factor(tempData[,independent])
  varD <- as.symbol(dependent)
  varI <- as.symbol(independent)
  model <- eval(bquote(
    clm(.(varD) ~ .(varI), data=df, link="logit")))
  
  #variance <- anova(nullModel, model)
  
  
  #cat(sprintf("The final (AIC=%f) model compared to the 
  #            null model(AIC=%f; X2 = %f, df=%f, p=%f \n",
  #        variance[2,2],
  #        variance[1,2],
  #        variance[2,4],
  #        variance[2,5],
  #        variance[2,6]))
  # The final (AIC=3310.7) model was not a significantly better fit than the null model(AIC=250.54; X2 = 19.42, df=10, p>0.05)
  
  print(summary(model))
  
  print(confint(model))
  # There was a significant positive association between ____ and ____ B=1.38, SE=0.55, p=0.12, 95%CI=0.34 to 2.51
}

# Run Ordinal regression
#
#
runologit <- function(df, independent, dependent) {
  tempData <- df[which(complete.cases(df[,c(independent, dependent, "INCOME_3", "ASSETS_3", "TIME_IN_RES")])),]
  varD <- as.symbol(dependent)
  varI <- as.symbol(independent)
  model <- eval(bquote(
    glm(.(varD) ~ factor(.(varI), ordered = F) + factor(INCOME_3) + Y_SCHOOLING + factor(ASSETS_3) + TIME_IN_RES, data=df, family=binomial())))
  
  #print(summary(model))
  #print(confint(model))
  return(model)
}

# Run Ordinal regression
#
#
runOrdinalRegression2 <- function(df, independent, dependent) {
  tempData <- df[which(complete.cases(df[,c(independent, dependent, "INCOME_3", "ASSETS_3", "TIME_IN_RES")])),]
  varD <- as.symbol(dependent)
  varI <- as.symbol(independent)
  model <- eval(bquote(
    clm(.(varD) ~ factor(.(varI), ordered = F) + INCOME_3 + Y_SCHOOLING + ASSETS_3 + TIME_IN_RES, data=df)))

  #print(summary(model))
  #print(confint(model))
  return(model)
}

# Run Linear Regression
#
#
runLinearRegression <- function(data, independent, dependent) {
  tempData <- data[which(complete.cases(data[,c(independent, dependent,"INCOME_RECODE","ASSETS_RECODE","Y_SCHOOLING")])),]
  cat(sprintf("Running model. n=%f", nrow(tempData)))
  
  nullModel <- glm(formula = tempData[, dependent] ~ 1, 
                   data=tempData,
                   family = quasipoisson())
  predictor <- tempData[,independent]
  
  # Models
  reducedmodel <- glm(formula = tempData[,dependent] ~ predictor,
                      data=tempData,
                      family =quasipoisson())
  
  fullmodel <- glm(formula = tempData[,dependent] ~ predictor + INCOME_RECODE + ASSETS_RECODE + Y_SCHOOLING + MARITLST,
               data=tempData,
               family = quasipoisson())
  
  variance <- anova(nullModel, fullmodel)
  
  
  cat(sprintf("The final (AIC=%f) model compared to the 
              null model(AIC=%f; X2 = %f, df=%f, p=%f \n",
              variance[2,2],
              variance[1,2],
              variance[2,4],
              variance[2,5],
              variance[2,6]))
  # The final (AIC=3310.7) model was not a significantly better fit than the null model(AIC=250.54; X2 = 19.42, df=10, p>0.05)
  
  print(summary(fullmodel), show.residuals=TRUE)
  
  #print(confint(model))
  # There was a significant positive association between _____ and _______ B=1.38, SE=0.55, p=0.12, 95%CI=0.34 to 2.51
  return(fullmodel)
}

# Run Linear Regression (simplified)
#
#
runLinearRegression2 <- function(data, independent, dependent) {
  cat("---------------------------------------------------------------------")
  cat(sprintf("Running model comparing %s to %s. n=%f", independent, dependent, nrow(data)))
  
  # Models
  reducedmodel <- lm(formula = data[[dependent]] ~ data[[independent]],
                      data=data)#,
                      #family =quasipoisson())
  
  #print(summary(reducedmodel), show.residuals=TRUE)
  
  fullmodel <- lm(formula = data[[dependent]] ~ data[[independent]] + INCOME_RECODE + ASSETS_RECODE + Y_SCHOOLING + MARITLST_RECODE + TIME_IN_RES,
                   data=data)#,
                   #family = quasipoisson())
  
  print(summary(fullmodel), show.residuals=TRUE)
  cat("---------------------------------------------------------------------")
  
  # There was a significant positive association between _____ and _______ B=1.38, SE=0.55, p=0.12, 95%CI=0.34 to 2.51
  return(fullmodel)
}

# Run step-wise regression
#
#
#
runStepWiseRegression <- function(yoData, ooData, tData, variable) {
  subset1 <- youngOldData[,c(variable,"INDEX_SC_P_NORM","INDEX_SC_N_NORM","INDEX_SC_F2_NORM","INCOME_T","ASSETS_T","Y_SCHOOLING")]
  subset2 <- oldOldData[,c(variable,"INDEX_SC_P_NORM","INDEX_SC_N_NORM","INDEX_SC_F2_NORM","INCOME_T","ASSETS_T","Y_SCHOOLING")]
  subset3 <- filteredData[,c(variable,"INDEX_SC_P_NORM","INDEX_SC_N_NORM","INDEX_SC_F2_NORM","INCOME_T","ASSETS_T","Y_SCHOOLING")]
  # For direction: both & backward
  #subset1 <- subset1[which(complete.cases(subset1[,c(variable,"INCOME_T","ASSETS_T","Y_SCHOOLING")])),]
  #subset2 <- subset2[which(complete.cases(subset2[,c(variable,"INCOME_T","ASSETS_T","Y_SCHOOLING")])),]
  #subset3 <- subset3[which(complete.cases(subset3[,c(variable,"INCOME_T","ASSETS_T","Y_SCHOOLING")])),]
  var <- as.symbol(variable)
  
  models <- eval(bquote(list(
    stepAIC(polr( 
      .(var) ~ .,
      data = subset1, Hess = T),
      direction = "forward", trace = T),
    stepAIC(polr( 
      .(var) ~ .,
      data = subset2, Hess = T),
      direction = "forward", trace = T),
    stepAIC(polr( 
      .(var) ~ .,
      data = subset3, Hess = T),
      direction = "forward", trace = T)
  )))
  return(models)
}


plotFrequencyFill <- function(df, y, legendTitle, z) {
  
  #s <- coef(lm(as.numeric(df[,y]) ~ AGE, data=df))[2]
  #i <- mean(as.numeric(filter(df, AGE == min(AGE))[,y]) %>%  replace_na(0))
  
  ggplot(data=df, 
         aes(x=AGE, fill=df[,y], na.exclude = FALSE)) +
    labs(x="Age", y="Frequency") +
    geom_bar(stat="count", position="fill", width=.9) +
    #geom_abline(aes(yintercept = mean(as.numeric(filter(df, AGE == min(AGE))[,y]))),# %>% replace_na(0))), 
    #            coef(lm(as.numeric(y) ~ AGE, data=df))[2]) +
    facet_grid(~df[[z]], scales='free') +
    scale_y_continuous(labels=scales::percent) +
    theme_classic() +
    scale_fill_manual(name=legendTitle, values=palette_blue, na.value = "white")#c("#f25850", "#27dee8","#34b3b9","#233445","#00478d","#027df7","#316ba4"), na.value="white")
}

plotFrequencyLines <- function(df, y, legendTitle, z) {

  cat <- z
  x_name <- "AGE"
  y_name <- y
  
  d <- df %>%  group_by(!!cat := df[[z]], !!x_name := df$AGE, !!y_name := df[[y]]) %>% summarise(n=n()) %>% mutate(perc = n/sum(n)) %>% ungroup()
  d <- subset(d, !is.na(d[[y_name]]))
    
  ggplot(data=d,
         aes(x=AGE, y=perc, color=.data[[y_name]], group=.data[[y_name]])) +
    labs(x="Age", y="Frequency") +
    #geom_line(size=1) +
    geom_smooth(method=lm, se=F)+
    facet_grid(~d[[cat]], scales='free') +
    scale_y_continuous(labels=scales::percent) +
    theme_classic() +
    scale_colour_manual(name=legendTitle, values=palette_theme, na.value = "gray") +
    scale_y_log10()
}

getSlopeCI <- function(yoDf, ooDf, dependent) {
  # Print young-old slope and CI
  model1 <- clm(yoDf[[dependent]] ~ AGE, data=yoDf)
  s1 <- tail(coef(model1), n=1)
  ci1 <- confint(model1)
  p1 <- tail(summary(model1)$coefficients[,4], n=1)
  
  # Print old-old slope and CI
  model2 <- clm(ooDf[[dependent]] ~ AGE, data=ooDf)
  s2 <- tail(coef(model2), n=1)
  ci2 <- confint(model2)
  p2 <- tail(summary(model2)$coefficients[,4], n=1)
  #rbind(c(s1,ci1,p1),c(s2,ci2,p2))
  rbind(c(s1,p1), c(s2,p2))
}

reverseFactorLevels <- function(df, new_name, variable) {
  factor_levels <- c("(1) strongly disagree","(2) disagree","(3) neither agree nor disagree", "(4) agree", "(5) strongly agree")
  inverseDF <- df %>% mutate(!!new_name := factor(as.integer(reverse.levels(df[[variable]])), labels=factor_levels, ordered=T))
  return(inverseDF)
}

reverseMaritalStatus <- function(df, new_name, variable) {
  factor_levels <- c("(1) never married", "(2) widowed", "(3) divorced", "(4) separated","(5) living with a partner","(6) married")
  inverseDF <- df %>% mutate(!!new_name := factor(as.integer(reverse.levels(df[[variable]])), labels=factor_levels))
  return(inverseDF)
}

reduceFactorLevels <- function(df, new_name, variable) {
  factor_levels <- c("low", "medium", "high")
  newDF <- df %>% mutate(!!new_name := factor(case_match(as.integer(df[[variable]]), 1 ~ 1, 2 ~ 1, 3 ~ 2, 4 ~ 3, 5 ~ 3, .default = as.integer(df[[variable]])), labels=factor_levels, ordered = T))
  return(newDF)
}

#
#
#
calculateWalkability <- function (s) {
  #s <- row[["STRUCTQ"]]
  v <- 0
  
  if (is.na(s) || s == 1 || s == 2) {
    v <- 1
  } else if (s == 3 || s == 4 || s == 5 || s == 10 || s == 11) {
    v <- 2
  } else if (s == 6) {
    v <-3
  } else if (s == 7 || s == 8) {
    v <- 4
  } else if (s == 9) { 
    v <- 5
  } else {
    v <- 1
  }
  return(v)
}