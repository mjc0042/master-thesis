library(haven)
library(readr)
library(tidyverse)
library(stargazer)
library(ParamHelpers)
library(mlr)
library(table1)
library(modelsummary)
library(ordinal)

# Load data
getwd()
setwd("~/School/4 Thesis/data/NSHAP/workspace")
load(file="core_data.rda")

# Rename
filteredData <- da36873.0001 %>% filter(AGE > 64)
rm(da36873.0001)
filteredData <- filteredData %>% mutate(AGE_CAT = factor(ifelse(AGE < 75, 1, 2), levels=c(1,2), labels=c("young-old", "old-old")))

# Drop rows with multiple NAs
# filteredData <- filteredData[rowSums(is.na(x[,5:9]))!=5,]
filteredData <- filteredData[rowSums(is.na(filteredData[,c("VOLUNTEER","ATTEND","SOCIAL","ATNDSERV2","LEFTOUT2","ISOLATED2")]))!=6,]

# Recode
filteredData <- reverseFactorLevels(filteredData, "GETALONG_RECODE", "GETALONG")
filteredData <- reverseFactorLevels(filteredData, "VALUES_RECODE", "VALUES")
#filteredData <- reduceFactorLevels(filteredData, "LITTER_RECODE", "IWDESCRIBE1")
#filteredData <- reduceFactorLevels(filteredData, "NOISE_RECODE", "IWDESCRIBE2")
#filteredData <- reduceFactorLevels(filteredData, "TRAFFIC_RECODE", "IWDESCRIBE3")
#filteredData <- reduceFactorLevels(filteredData, "BLDG_SPACE_RECODE", "IWDESCRIBE4")
#filteredData <- reduceFactorLevels(filteredData, "POLLUTION_RECODE", "IWDESCRIBE5")

# Create background table
backgroundData <- filteredData[c("ID","AGE","GENDER","HEARN_RECODE","HSASSETS_RECODE","EDUC","MARITLST","RESIDEY2", "AGE_CAT")]
backgroundData <- backgroundData %>% mutate(across(.cols=3:8, as.ordered))

yoBgData <- backgroundData %>% filter(AGE <= 74)
ooBgData <- backgroundData %>% filter(AGE >= 75)
#bgData <- backgroundData %>% group_split(AGE <= 74, AGE >= 75)

# Create social capital table
scData <- filteredData[c("ID","VOLUNTEER","ATTEND","SOCIAL","ATNDSERV2","FAMRELY2","FRRELY2","FRHELP","FRAMT","VISIT","DOFAVORS","PERSADVICE","CLOSEKNIT","HELPNEIGH","GETALONG_RECODE","VALUES_RECODE","CANTRUST","AFRAIDNITE","LEFTOUT2","ISOLATED2")]
scData <- scData %>% mutate(across(.cols=2:20, as.ordered))

# Create built environment table
beData <- filteredData[c("ID","STRUCTQ","BUILD","OTBUILD","COMBUILD","IWDESCRIBE1","IWDESCRIBE2","IWDESCRIBE3","IWDESCRIBE4","IWDESCRIBE5","AREACOMFORT","AREASAFE","AREAAMENITIES")]
beData <- beData %>% mutate(across(.cols=2:13, as.ordered))

# Create well-being
wbData <- filteredData[c("ID","MNTLHLTH3","HAPPY","FLTDEP","FLTEFF","NOSLEEP","WASHAPY","WASLONLY","UNFRIEND","ENJLIFE","FLTSAD","DISLIKD")]
wbData <- wbData %>% mutate(across(.cols=2:12, as.ordered))

# Create young-old table
youngOldData <- right_join(wbData, 
                   right_join(beData,
                             left_join(yoBgData, scData, by="ID"), by="ID"), by="ID") 

oldOldData <- right_join(wbData, 
                         right_join(beData,
                                    left_join(ooBgData, scData, by="ID"), by="ID"), by="ID") 

youngOldData_omitted <- na.omit(youngOldData)
oldOldData_omitted <- na.omit(oldOldData)

# Descriptive Statistics
summaries <- t(sapply(youngOldData, function(x) list(Mean = mean(x), SD=sd(x))))
summaries <- youngOldData %>% summarise(Count=n()),Mean=mean(x),Median=median(x),SD=sd(x),Min=min(x),Max=max(x))
summaries

X <- mlr::createDummyFeatures(obj = youngOldData[,-1])
X.df <- data.frame(X)
stargazer(youngOldData[,-1], type = "text")

datasummary_skim(backgroundData[,-1])


# Age frequency distribution
#qplot(data=backgroundData,
#      x=AGE, 
#      geom="histogram", 
#      xlab = "Age", ylab = "Frequency", main = "Age Distribution",
#      col = I("white"), fill = I("gray"), 
#      bins = max(backgroundData$AGE) - min(backgroundData$AGE),
#      binwidth = 1)
ggplot(data=backgroundData, aes(x=AGE, fill=ifelse(AGE > 74, "Old-Old", "Young-Old"))) +
  labs(x="Age", y="", title="Age Distribution") +
  geom_bar() +
  scale_fill_manual(values=c("#316ba4", "#233445"), name="")

#Social capital
# Gatherings
#qplot(data=filteredData,
#      x=AGE,
#      geom="histogram",
#      fill=VOLUNTEER,
#      xlab = "Age", ylab = "Volunteer Frequency",
#      main = "Volunteer Distribution")

library(RColorBrewer)
myColors <- brewer.pal()
names(myColors) <- levels(filteredData$VOLUNTEER)
colScale <- scale_color_manual(name = "grp",values = myColors)
#colScale <- scale_fill_manual(values=c("#f25850", "#233445","#27dee8","#00478d", "#34b3b9", "#027df7", "#316ba4"))

ggplot(data=youngOldData, 
       aes(x=AGE, fill=VOLUNTEER)) +
  labs(x="Age", y="Frequency", colour="Volunteering") +
  geom_bar(stat="count", position="fill") +
  scale_y_continuous(labels=scales::percent) +
  theme_classic() + colScale
  scale_fill_manual(name="Volunteering", values=c("#f25850", "#27dee8","#34b3b9","#233445","#00478d","#027df7","#316ba4"), na.value="white")
  #scale_color_manual(values = c("foo" = "#999999", "bar" = "#E69F00"))
  #scale_fill_grey(na.value="white")

ggplot(data=subset(filteredData,!is.na(VOLUNTEER)), 
       aes(x=AGE, fill=forcats::fct_rev(VOLUNTEER))) +
  labs(x="Age", y="Frequency", main="Volunteer Dist.") +
  geom_histogram(stat="count") +
  theme_classic() +
  scale_fill_grey(na.value="white")

# Friends
#ggplot(data=filteredData, 
#      aes(x=AGE, fill=FRAMT)) +
#  labs(x="Age", y="Number") +
#  geom_histogram(stat="count") +
#  theme_classic() +
#  scale_fill_grey(name="Number of friends", na.value="white")

# environment
ggplot(data=filteredData, 
       aes(x=AGE, fill=STRUCTQ)) +
  labs(x="Age", y="Frequency") +
  geom_bar(stat="count", position="fill") +
  scale_y_continuous(labels=scales::percent) +
  theme_classic() +
  scale_fill_grey(name="Building Type", na.value="white") #+
  facet_wrap(~AGE)

# well-being
ggplot(data=filteredData, 
       aes(x=AGE, fill=HAPPY)) +
  labs(x="Age", y="Frequency") +
  geom_bar(stat="count", position="fill") +
  scale_y_continuous(labels=scales::percent) +
  theme_classic() +
  scale_fill_grey(name="Happiness", na.value="white") +
  geom_smooth(stat="count", aes(x=AGE, y=HAPPY), method="lm", position="fill")
ggplot(data=filteredData, aes(x=AGE, color=HAPPY)) +
  labs(x="Age", y="Frequency") +
  geom_line(stat="count", position="fill") +
  scale_y_continuous(labels=scales::percent)
  #theme_(name="Happiness", na.value="white")

# Correlations
levels(scData$VOLUNTEER)
str(scData$VOLUNTEER)
is.factor(scData$VOLUNTEER)
is.numeric(scData$VOLUNTEER)
as.numeric(scData$VOLUNTEER)
is.character(scData$VOLUNTEER)
head(scData[2])
cor(as.numeric(scData[,-1]), as.numeric(scData[,-1]),use="pairwise.complete.obs")
scData[,-1] %>% mutate_all(as.numeric) %>% cor(use="pairwise.complete.obs")

library(polycor)
library(modelsummary)
hetcor(scData[-1])$correlations %>% ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=1)
scData[,-1] %>% mutate_all(as.numeric) %>% datasummary_correlation()

sc_be_corr <- cor(mutate_all(scData[,-1], as.numeric), mutate_all(beData[,-1],as.numeric),use="pairwise.complete.obs")
sc_be_corr %>% kbl() %>% kable_classic("hover", full_width=FALSE)
ggcorrplot(sc_be_corr, lab=TRUE, lab_size=1)


#set.seed(1)
#DF <- data.frame(x=sample(c("Y","N"),100,T),y=sample(c("Y","N"),100,T))

# how to get correlation
#DF[] <- lapply(DF,as.numeric)
#cor(DF)
#yoCorr <- cor.test(as.numeric(youngOldData$WASHAPY), as.numeric(youngOldData$STRUCTQ))
#yoCorr <- cor(x=as.numeric(youngOldData[,-1]),y=as.numeric(youngOldData[,-1]))
#datasummary_correlation(youngOldData[,-1])

#####
#library("corrplot")
#library(tinytable)

#df <- data.frame(age=c(19,25,23,30), region=c("North","South","Center","South"), graduate=c("no","yes","yes","no"), salary=c(21000,24000,23000,25000))
#corrplot(cor(df), method='number')

#lm(salary~age+region+graduate, df)
####



# Generalized Linear Regression
# Test SOCIAL as predicted by STRUCTURE
# Remove NA cases
tempData <- oldOldData[which(complete.cases(oldOldData[,c('SOCIAL', 'STRUCTQ', 'IWDESCRIBE4', 'AREAAMENITIES')])),]
model_social_null <- clm(as.factor(SOCIAL) ~ 1, 
                         data=tempData, 
                         link="logit")
model_social_struct <- clm(as.factor(SOCIAL) ~ as.factor(IWDESCRIBE4),
                           data=tempData,
                           link="logit")
v <- anova(model_social_null, model_social_struct)



# The final (AIC=3310.7) model was not a significantly better fit than the null model(AIC=250.54; X2 = 19.42, df=10, p>0.05)
s <- summary(model_social_struct)
s
confint(model_social_struct)
# There was a significant positive association between stupidity and liking coldplay B=1.38, SE=0.55, p=0.12, 95%CI=0.34 to 2.51

