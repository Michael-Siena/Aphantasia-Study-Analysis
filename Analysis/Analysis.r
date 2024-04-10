# RESET ENVIRONMENT ----
cat("\014") # clears console (code to send CTRL + L)
rm(list = ls()) # clears workspace
options(scipen = 999) # suppresses scientific notation
options(contrasts = c("contr.sum","contr.poly")) # makes summary() produce ANOVA-like contrasts

# CUSTOM FUNCTION / OPERATOR DEFS ----
# negate %in%
'%!in%' <- function(x, y) { !('%in%'(x, y)) }
# convert Kendall's tau to Pearson's r
TauToR <- function(tau) { sin(0.5 * pi * tau) } # https://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=1719&context=jmasm

# LOAD PACKAGES ----
pkgNames <- c("moments", 
              "corrplot",
              "tidyverse", 
              "ggpubr", 
              "rstatix", 
              "reshape2", 
              "psycho", 
              "ez",
              "afex",
              "emmeans",
              "multcomp",
              "MASS",
              "dplyr",
              "ggplot2",
              "geoR",
              "lme4",
              "brms",
              "car",
              "moments",
              "statmod",
              "DHARMa",
              "AICcmodavg",
              "lmerTest", 
              "glmmTMB",
              "MuMIn", 
              "rstanarm",
              "performance",
              "AID",
              "BayesFactor", 
              "nparLD",
              "dfoptim",
              "optimx", 
              "data.table",
              "Barnard",
              "MuMIn",
              "sjstats",
              "pwr",
              "ppcor",
              "nlme",
              "influence.ME",
              "buildmer",
              "remotes",
              "rmcorr",
              "brms",
              "DescTools")

for (pkgName in pkgNames)
{
   if(!require(pkgName, character.only = TRUE))
   {
      install.packages(pkgName)
   }
   require(pkgName, character.only = TRUE)
}

# READ DATA ----
# demographic data
demographicData <- read.csv2("DemographicData.csv", sep = ",")
# questionnaire data
questionnaireData <- read.csv2("QuestionnaireData.csv", sep = ",")
questionnaireData[, 4:8] <- lapply(questionnaireData[, 4:8], as.double)  # converts numeric strings to doubles

wide_subjectData <- read.csv2(
  "SubjectData.csv",
   header = TRUE, 
   sep = ",",
   dec = ".",
   fileEncoding = "UTF-8-BOM"
  )

subjectData <- wide_subjectData |> 
  reshape2::melt(id.vars = c(
    "sid", 
    "group", 
    "VVIQ", 
    "SAM", 
    "SAM_Epis", 
    "SAM_Fact", 
    "SAM_Spat", 
    "SAM_Future",
    "age",
    "sex", 
    "edu_lvl"
    )
  ) |>
  separate(
   col = variable, 
   into = c("testPerspective", "switchStatus", "Measure"),
   sep = "_"
  ) |>
  pivot_wider(
   names_from = Measure, 
   values_from = value
  ) |>
  convert_as_factor(
   sid, 
   group, 
   testPerspective, 
   switchStatus,
   sex,
   edu_lvl
  ) |>
  as.data.frame()

newGrpOrder <- c("Control", "Aphantasic")
subjectData$group <- factor(
  subjectData$group, 
  levels = newGrpOrder
  )

newPerspectiveNames <- c("Third person", "First person")
levels(subjectData$testPerspective) <- newPerspectiveNames
newPerspectiveOrder <- rev(newPerspectiveNames)
subjectData$testPerspective <- factor(
  subjectData$testPerspective, 
  levels = newPerspectiveOrder
  )

wide_subjectData_objMemOnly <- read.csv2(
  "SubjectData_ObjectMemOnly.csv",
  header = TRUE, 
  sep = ",",
  dec = ".",
  fileEncoding = "UTF-8-BOM"
  )

subjectData_objMemOnly <- wide_subjectData_objMemOnly |>
   pivot_longer(
     cols = 3:10,
     names_to = c("perspective", ".value"),
     names_sep = "_"
   ) |>
   convert_as_factor(
     Subject,
     Group,
     perspective,
     sex,
     edu_lvl
   ) |>
   as.data.frame()

newGrpOrder <- c("Control", "Aphantasic")
subjectData_objMemOnly$Group <- factor(
  subjectData_objMemOnly$Group, 
  levels = newGrpOrder
  )

newPerspectiveNames <- c("Third person", "First person")
levels(subjectData_objMemOnly$perspective) <- newPerspectiveNames
newPerspectiveOrder <- rev(newPerspectiveNames)
subjectData_objMemOnly$perspective <- factor(
  subjectData_objMemOnly$perspective, 
  levels = newPerspectiveOrder
  )

wide_mixtureModelData_pT <- read.csv2(
  "MixtureModelData_pT.csv",
  header = TRUE, 
  sep = ",",
  dec = ".",
  fileEncoding = "UTF-8-BOM"
  )

wide_mixtureModelData_pK <- read.csv2(
  "MixtureModelData_pK.csv",
  header = TRUE, 
  sep = ",",
  dec = ".",
  fileEncoding = "UTF-8-BOM"
  )

mixtureModelData_pT <- wide_mixtureModelData_pT |> 
  pivot_longer(cols = 1:2,
               names_to = c("perspective", ".value"),
               names_sep = "_") |>
  convert_as_factor(SID,
                    Group,
                    perspective) |>
  as.data.frame()

mixtureModelData_pK <- wide_mixtureModelData_pK |> 
  pivot_longer(cols = 1:2,
               names_to = c("perspective", ".value"),
               names_sep = "_") |>
  convert_as_factor(SID,
                    Group,
                    perspective) |>
  as.data.frame()

newGroupNames <- c("Aphantasic", "Control")
levels(mixtureModelData_pT$Group) <- newGroupNames
levels(mixtureModelData_pK$Group) <- newGroupNames

newGroupOrder <- rev(newGroupNames)
mixtureModelData_pT$Group <- factor(
  mixtureModelData_pT$Group, 
  levels = newGroupOrder
  )
mixtureModelData_pK$Group <- factor(
  mixtureModelData_pK$Group, 
  levels = newGroupOrder
  )

subjectData_objMemOnly |>
   group_by(Group, perspective) |> 
   summarise(
     n = n(), 
     mean = mean(ObjViv, na.rm = TRUE), 
     sd = sd(ObjViv, na.rm = TRUE), 
     se = sd / sqrt(n)
     ) |>
   as.data.frame()

subjectData_objMemOnly |>
   group_by(Group, perspective) |> 
   summarise(
     n = n(), 
     mean = mean(ObjRT, na.rm = TRUE), 
     sd = sd(ObjRT, na.rm = TRUE), 
     se = sd / sqrt(n)
     ) |>
   as.data.frame()

mixtureModelData_pT |>
   group_by(Group, perspective) |>
   summarise(
     n = n(),
     mean = mean(pT, na.rm = TRUE),
     sd = sd(pT, na.rm = TRUE),
     se = sd / sqrt(n)
     ) |>
   as.data.frame()

mixtureModelData_pK |>
   group_by(Group, perspective) |>
   summarise(
     n = n(),
     mean = mean(K, na.rm = TRUE),
     sd = sd(K, na.rm = TRUE),
     se = sd / sqrt(n)
     ) |>
   as.data.frame()

# OUTLIER IDENTIFICATION ----
# uses distribution agnostic interquartile range method
otlrs_sub_spat_Viv <- subjectData |> 
   group_by(group, testPerspective, switchStatus) |> 
   identify_outliers(SpatViv)
length(unique(otlrs_sub_spat_Viv$sid))

otlrs_sub_spat_MS <- subjectData |> 
   group_by(group, testPerspective, switchStatus) |> 
   identify_outliers(SpatMS)
length(unique(otlrs_sub_spat_MS$sid))

otlrs_sub_spat_RT <- subjectData |> 
   group_by(group, testPerspective, switchStatus) |> 
   identify_outliers(SpatRT)
length(unique(otlrs_sub_spat_RT$sid))

otlrs_sub_obj_viv <- subjectData_objMemOnly |> 
   group_by(Group, perspective) |> 
   identify_outliers(ObjViv)
length(unique(otlrs_sub_obj_viv$Subject))

otlrs_sub_obj_err <- subjectData_objMemOnly |> 
   group_by(Group, perspective) |> 
   identify_outliers(ObjErr)
length(unique(otlrs_sub_obj_err$Subject))

otlrs_sub_obj_pT <- mixtureModelData_pT |> 
   group_by(Group, perspective) |> 
   identify_outliers(pT)
length(unique(otlrs_sub_obj_pT$SID))  

otlrs_sub_obj_pK <- mixtureModelData_pK |> 
   group_by(Group, perspective) |> 
   identify_outliers(K)
length(unique(otlrs_sub_obj_pK$SID)) 

otlrs_sub_obj_RT <- subjectData_objMemOnly |> 
   group_by(Group, perspective) |> 
   identify_outliers(ObjRT)
length(unique(otlrs_sub_obj_RT$Subject))

# DATA FILTERS ----
# These are for subsetting data depending on different criteria for data exploration and
# sensitivity analysis
failingBainbridgeCriteria <- c(1, 13, 28, 31, 42, 56)
floorVVIQ <- c(1, 47, 61, 79, 84, 89, 100)
floorViv <- c(61, 62, 67, 77, 80, 91, 97, 100, 106)
ceilingViv <- c(1, 47, 58, 71, 74, 75, 79, 81, 84, 86, 89)
aphantsAboveFloorVVIQ <- c(1, 47, 58, 71, 74, 75, 79, 81, 84, 86, 89)

# DEMOGRAPHIC ANALYSIS ----
# Must ensure participant groups are matched on sex, age, and education otherwise
# results may be confounded

# Fisher's Exact Test on sex (count), well suited for smaller sample sizes...
sexCounts <- demographicData |> 
   group_by(group) |> 
   mutate(
     male = sum(sex == "M", na.rm = TRUE), 
     fem = sum(sex == "F", na.rm = TRUE),
     .keep = "none"
     ) |>
   distinct() |>
   as.data.frame()

ft_sex <- fisher.test(
  sexCounts[, -1],
  alternative = "two.sided"
  )

# Independent samples t-test on age
tt_age <- t.test(
  x = demographicData |> 
    subset(group == "Control", age),
  y = demographicData |> 
    subset(group == "Aphantasic", age),
  paired = FALSE,
  alternative = "two.sided",
  var.equal = FALSE # unreasonable assumption given disparity in group sizes
  )

# Wilcoxon's signed-rank test on eduction level
wt_Education <- wilcox.test(
  x = demographicData |> 
     subset(group == "Control", edu_lvl) |> 
     as.matrix(),
  y = demographicData |> 
     subset(group == "Aphantasic", edu_lvl) |> 
     as.matrix(),
  paired = FALSE,
  alternative = "two.sided",
  exact = FALSE, # cannot compute exact p-value with ties
  conf.int = TRUE,
  na.rm = TRUE
  )

# QUESTIONNAIRE ANALYSIS ----
# VVIQ measures trait mental imagery whereas SAM is a memory questionnaire
# with episodic, prospective, semantic, and spatial memory subcomponents.
# t-test on VVIQ
tt_VVIQ <- t.test(
  x = questionnaireData |> 
     subset(Group == "Control", VVIQ),
  y = questionnaireData |> 
     subset(Group == "Aphantasic", VVIQ),
  paired = FALSE,
  alternative = "two.sided",
  var.equal = FALSE
  )

# t-test on semantic memory (SAM)
tt_SAM_tot <- t.test(
  x = questionnaireData |> 
    subset(Group == "Control", SAM_tot),
  y = questionnaireData |> 
    subset(Group == "Aphantasic", SAM_tot),
  paired = FALSE,
  alternative = "two.sided",
  var.equal = FALSE
  )

# t-test on episodic memory (SAM)
tt_SAM_epis <- t.test(
  x = questionnaireData |> 
    subset(Group == "Control", SAM_epis),
  y = questionnaireData |> 
    subset(Group == "Aphantasic", SAM_epis),
  paired = FALSE,
  alternative = "two.sided",
  var.equal = FALSE
  )

# t-test on prospective (future) memory (SAM)
tt_SAM_fut <- t.test(
  x = questionnaireData |> 
     subset(Group == "Control", SAM_fut),
  y = questionnaireData |> 
     subset(Group == "Aphantasic", SAM_fut),
  paired = FALSE,
  alternative = "two.sided",
  var.equal = FALSE
  )

# t-test on semantic (fact) memory (SAM)
tt_SAM_fact <- t.test(
  x = questionnaireData |> 
    subset(Group == "Control", SAM_fact),
  y = questionnaireData |> 
    subset(Group == "Aphantasic", SAM_fact),
  paired = FALSE,
  alternative = "two.sided",
  var.equal = FALSE
  )

# t-test on spatial memory (SAM)
tt_SAM_spat <- t.test(
  x = questionnaireData |> 
    subset(Group == "Control", SAM_spat),
  y = questionnaireData |> 
    subset(Group == "Aphantasic", SAM_spat),
  paired = FALSE,
  alternative = "two.sided",
  var.equal = FALSE
  )

# MAIN ANALYSIS ----
# Linear mixed effects models (accounting for repeated measures from same participants)
# on data collected from my memory task (see https://github.com/Michael-Siena/Object-and-Spatial-Memory-Task).
# These models handle missing data, unbalanced group sizes, non-normal response distributions, etc.
# better than standard RM ANOVAs. Mixed effects models can model both trial-level and 
# aggregate (e.g., mean) data. We use aggregate data as it is cleaner/meets model assumptions better.
#
# The memory task measured object (i.e., colour) and spatial (i.e., location) memory features 
# for objects studied within a 3D virtual environment.
#
# Key variables:
# Object/spatial memory subjective vividness rating (trial-specific mental imagery)
#
# Object memory accuracy (derived from probabilistic mixture modelling)
# Object memory precision (derived from probabilistic mixture modelling)
# Object memory raw error (model free measure of angular deviation between true and response colour on colour wheel)
# Object memory response time
#
# Spatial memory score (takes form of accuracy, corrected for varying location difficulty)
# Spatial memory raw error (Euclidean/straight line distance between true and response location)
# Spatial memory response time

## Object Memory Measures
# Object memory vividness rating model specification 
subMod_gauss_objOnly_viv <- lmer(
  formula = value ~ Group * perspective + (1 | Subject),
  data = subjectData_objMemOnly |>
    #subset(Subject %!in% aphantsAboveFloorVVIQ) |> # for sensitivity and exploratory analyses
    rename(value = ObjViv)
  ) 

# Diagnostic plots to assess suitability of model
subMod_gauss_objOnly_viv |>
   plot(type = c("p", "smooth"), col.line = 2)
# scale-location
subMod_gauss_objOnly_viv |>
   plot(
     sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"),
     col.line = 2
   )
par(mfrow = c(2, 2))
# resid density
subMod_gauss_objOnly_viv |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
subMod_gauss_objOnly_viv |> 
   residuals() |>
   qqPlot()

# Model results
summary(subMod_gauss_objOnly_viv)

# Object memory accuracy model specification 
subMod_gauss_obj_pT <- lmer(
  formula = value ~ Group * perspective + (1 | SID),
  data = mixtureModelData_pT |>
     #subset(SID %!in% aphantsAboveFloorVVIQ) |>
     rename(value = pT)
  ) 

# Diagnostic plots to assess suitability of model
subMod_gauss_obj_pT |>
   plot(type = c("p", "smooth"), col.line = 2)
# scale-location
subMod_gauss_obj_pT |>
   plot(
     sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"),
     col.line = 2
     )
par(mfrow = c(1,2))
# resid density
subMod_gauss_obj_pT |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
subMod_gauss_obj_pT |> 
   residuals() |>
   qqPlot()

# model results
summary(subMod_gauss_obj_pT)

# Object memory precision model specification 
subMod_gauss_obj_pK <- lmer(
  formula = value ~ Group * perspective + (1 | SID),
  data = mixtureModelData_pK |>
    #subset(SID %!in% aphantsAboveFloorVVIQ) |>
    rename(value = K)
  ) 

# Diagnostic plots to assess suitability of model
subMod_gauss_obj_pK |>
   plot(type = c("p", "smooth"), col.line = 2)
# scale-location
subMod_gauss_obj_pK |>
   plot(
     sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"),
     col.line = 2
     )
par(mfrow = c(1,2))
# resid density
subMod_gauss_obj_pK |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
subMod_gauss_obj_pK |> 
   residuals() |>
   qqPlot()

# model results
summary(subMod_gauss_obj_pK)

# Object memory raw error model specification 
subMod_gauss_objOnly_Err <- lmer(
  formula = value ~ Group * perspective + (1 | Subject),
  data = subjectData_objMemOnly |>
     #subset(Subject %!in% aphantsAboveFloorVVIQ) |>
     rename(value = ObjErr)
  ) 

# Diagnostic plots to assess suitability of model
subMod_gauss_objOnly_Err |>
   plot(type = c("p", "smooth"), col.line = 2)
# scale-location
subMod_gauss_objOnly_Err |>
   plot(
     sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"),
     col.line = 2
     )
par(mfrow = c(2, 2))
# resid density
subMod_gauss_objOnly_Err |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
subMod_gauss_objOnly_Err |> 
   residuals() |>
   qqPlot()

# model results
summary(subMod_gauss_objOnly_Err)

# object memory response time model specification 
subMod_gauss_objOnly_RT <- lmer(
  formula = value ~ Group * perspective + (1 | Subject),
  data = subjectData_objMemOnly |>
    #subset(Subject %!in% aphantsAboveFloorVVIQ) |> 
    rename(value = ObjRT)
  ) 

# Diagnostic plots to assess suitability of model
subMod_gauss_objOnly_RT |>
   plot(type = c("p", "smooth"), col.line = 2)
# scale-location
subMod_gauss_objOnly_RT |>
   plot(
     sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"),
     col.line = 2
     )
par(mfrow = c(2, 2))
# resid density
subMod_gauss_objOnly_RT |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
subMod_gauss_objOnly_RT |> 
   residuals() |>
   qqPlot()

# Model results
summary(subMod_gauss_objOnly_RT)

## Spatial Memory Measures
# Model formula - includes a perspective switch condition that was collapsed in object memory analyses
# (needed for the probabilistic mixture modelling).
subMod_Formula = value ~ group * testPerspective * switchStatus + (1 | sid)

# Spatial memory vividness model specification
subMod_gauss_spat_viv <- lmer(
  formula = formula(subMod_Formula),
  data = subjectData |>
     #subset(sid %!in% aphantsAboveFloorVVIQ) |>
     rename(value = SpatViv)
  )

# Diagnostic plots to assess suitability of model
subMod_gauss_spat_viv  |>
   plot(type = c("p", "smooth"), col.line = 2)
# scale-location
subMod_gauss_spat_viv  |>
   plot(
     sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"),
     col.line = 2
     )
par(mfrow = c(1,2))
# resid density
subMod_gauss_spat_viv  |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
subMod_gauss_spat_viv  |> 
   residuals() |>
   qqPlot()

# Model results
summary(subMod_gauss_spat_viv)

# Estimated marginal means to explore interaction effects
subEmm_gauss_spat_viv <- emmeans(
  object = subMod_gauss_spat_viv, 
  specs = pairwise ~ switchStatus | group | testPerspective,
  lmer.df = "satterthwaite"
  )
subEmm_gauss_spat_viv$emmeans |> pairs(adjust = "Tukey") 
subEmm_gauss_spat_viv |> plot(comparisons = TRUE)

# Spatial memory score
subMod_gauss_spat_MS <- lmer(
  formula = formula(subMod_Formula),
  data = subjectData |>
    #subset(sid %!in% aphantsAboveFloorVVIQ) |>
    rename(value = SpatMS)
  )

# Diagnostic plots to assess suitability of model
subMod_gauss_spat_MS |>
   plot(type = c("p", "smooth"), col.line = 2)
# scale-location
subMod_gauss_spat_MS |>
   plot(
     sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"),
     col.line = 2
     )
par(mfrow = c(1,2))
# resid density
subMod_gauss_spat_MS |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
subMod_gauss_spat_MS |> 
   residuals() |>
   qqPlot()

# Model results
summary(subMod_gauss_spat_MS)

# Estimated marginal means to explore interaction effects
subEmm_gauss_spat_MS <- emmeans(
  object = subMod_gauss_spat_MS, 
  specs = pairwise ~ testPerspective | group | switchStatus,
  lmer.df = "satterthwaite"
  )
subEmm_gauss_spat_MS$emmeans |> pairs(adjust = "Tukey") 
subEmm_gauss_spat_MS |> plot(comparisons = TRUE)

# Spatial memory response time
subMod_gauss_spat_RT <- lmer(
  formula = formula(subMod_Formula),
  data = subjectData |>
    #subset(sid %!in% aphantsAboveFloorVVIQ) |>
    rename(value = SpatRT)
  )

# Diagnostic plots to assess suitability of model
subMod_gauss_spat_RT |>
   plot(type = c("p", "smooth"), col.line = 2)
# scale-location
subMod_gauss_spat_RT |>
   plot(sqrt(abs(resid(.))) ~ fitted(.),
        type = c("p", "smooth"),
        col.line = 2)
par(mfrow = c(1,2))
# resid density
subMod_gauss_spat_RT |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
subMod_gauss_spat_RT |> 
   residuals() |>
   qqPlot()

# Model results
summary(subMod_gauss_spat_RT)

# Estimated marginal means to explore interaction effects
subEmm_gauss_spat_RT <- emmeans(
  object = subMod_gauss_spat_RT, 
  specs = pairwise ~ testPerspective | group,
  lmer.df = "satterthwaite"
  )
subEmm_gauss_spat_RT$emmeans |> pairs(adjust = "Tukey") 
subEmm_gauss_spat_RT |> plot(comparisons = TRUE)

# CORRELATION ANALYSIS ----
# Prepare data for correlation analysis. We will use Kendall correlations to account 
# for the high proportion of floor VVIQ scores/task vividness ratings in the aphantasic group.
# Both groups subjected to Kendall correlations for fairness of subsequent comparisons.
avgObjViv <- wide_subjectData_objMemOnly |> 
  #subset(Subject %!in% excludedSubs_Bainbridge) |>
  subset(select = c("Ego_ObjViv", "Allo_ObjViv")) |> 
  rowMeans()

avgObjErr <- wide_subjectData_objMemOnly |>
  #subset(sid %!in% excludedSubs_Bainbridge) |>
  subset(select = c("Ego_ObjErr", "Allo_ObjErr")) |> 
  rowMeans()

avgSpatViv <- wide_subjectData |> 
  #subset(sid %!in% excludedSubs_Bainbridge) |>
  subset(
    select = c(
      "Ego_Stay_SpatViv", 
      "Ego_Switch_SpatViv",
      "Allo_Stay_SpatViv",
      "Allo_Switch_SpatViv")
  ) |> 
  rowMeans()

avgSpatMS <- wide_subjectData |> 
  #subset(sid %!in% excludedSubs_Bainbridge) |>
  subset(
    select = c(
      "Ego_Stay_SpatMS", 
      "Ego_Switch_SpatMS",
      "Allo_Stay_SpatMS",
      "Allo_Switch_SpatMS"
    )
  ) |> 
  rowMeans()

# Correlations in controls
cor.test(
  x = wide_subjectData$VVIQ[wide_subjectData$group == "Control"], 
  y = avgObjViv[wide_subjectData$group == "Control"],
  method = "kendall"
  )

cor.test(
  x = avgObjViv[wide_subjectData$group == "Control"], 
  y = avgObjErr[wide_subjectData$group == "Control"],
  method = "kendall"
  )

# Correlations in Aphantasic
cor.test(
  x = wide_subjectData$VVIQ[wide_subjectData$group == "Aphantasic"], 
  y = avgSpatViv[wide_subjectData$group == "Aphantasic"],
  method = "kendall"
  )

cor.test(
  x = avgSpatViv[wide_subjectData$group == "Aphantasic"], 
  y = avgSpatMS[wide_subjectData$group == "Aphantasic"],
  method = "kendall"
  )

# Compare Correlations
# test the difference between correlations to see whether one is significantly 
# stronger than the other.
# Group sample sizes
n_contr = 27
n_aphant = 20

# object memory vividness vs vviq score
psych::r.test(n = n_contr, r12 = TauToR(0.22322217), 
              n2 = n_aphant, r34 = TauToR(0.14709071))
# spatial memory vividness vs vviq scpre
psych::r.test(n = n_contr, r12 = TauToR(0.40295949), 
              n2 = n_aphant, r34 = TauToR(0.25778496))

# object memory vividness vs object memory error
psych::r.test(n = n_contr, r12 = TauToR(-0.54415954), 
              n2 = n_aphant, r34 = TauToR(-0.090188532))
# spatial memory vividness vs spatial memory MS
psych::r.test(n = n_contr, r12 = TauToR(0.28205128), 
              n2 = n_aphant, r34 = TauToR(0.2585761))

# DATA VISUALISATION ----
## Histogram showing spread/frequency of VVIQ scores in both groups
vviq_hist_data <- wide_subjectData[c("group", "VVIQ")]
vviq_hist_data |> ggplot(
  aes(x = VVIQ, fill = group)
  ) +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    strip.text.y = element_blank()
    ) +
  geom_histogram(
    color = "black",
    alpha = 0.6, 
    position = 'identity',
    bins = nrow(wide_subjectData)
    ) +
  scale_fill_manual(values = c("red", "steelblue2")) +   
  scale_x_continuous(
    breaks = seq(16, 80, 8), 
    limits = c(16, 80)
    ) +
  geom_vline(
    xintercept = 32, 
    linetype = "dashed", 
    size = 1) +
  labs(fill="") +
  xlab("VVIQ score") +
  ylab("Frequency")

## Scatter plots to visualise correlations in control and aphantasic groups
# Object memory vividness vs error in controls
control_objX = avgObjViv[wide_subjectData$group == "Control"]
control_objY = avgObjErr[wide_subjectData$group == "Control"]
ggplot(mapping = aes(x = control_objX, y = control_objY)) + 
  geom_point(
    stat = "identity",
    size = 4,
    position = position_dodge(width = 0.9)
    ) +
  geom_smooth(method = lm, se = FALSE) +
  theme_classic() + 
  ggtitle("Object Memory") +
  xlab("Vividness") + 
  ylab("Error") +
  theme(
    plot.title = element_text(size = 32, hjust = 0.5),
    axis.text = element_text(size = 26),
    axis.title = element_text(size = 26),
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 26),
    strip.background = element_blank(),
    strip.text = element_text(size = 32),
    strip.text.y = element_blank()
    )

# Spatial memory vividness vs MS in controls
control_spatX = avgSpatViv[wide_subjectData$group == "Control"]
control_spatY = avgSpatMS[wide_subjectData$group == "Control"]
ggplot(mapping = aes(x = control_spatX, y = control_spatY)) + 
  geom_point(
    stat = "identity",
    size = 4,
    position = position_dodge(width = 0.9)
    ) +
  geom_smooth(method = lm, se = FALSE) +
  theme_classic() + 
  ggtitle("Spatial Memory") +
  xlab("Vividness") + 
  ylab("MS") +
  theme(
    plot.title = element_text(size = 32, hjust = 0.5),
    axis.text = element_text(size = 26),
    axis.title = element_text(size = 26),
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 26),
    strip.background = element_blank(),
    strip.text = element_text(size = 32),
    strip.text.y = element_blank()
    )

# Object memory vividness vs error in aphantasics
aphant_objX = avgObjViv[wide_subjectData$group == "Aphantasic"]
aphant_objY = avgObjErr[wide_subjectData$group == "Aphantasic"]
ggplot(mapping = aes(x = aphant_objX, y = aphant_objY)) + 
  geom_point(
    stat = "identity",
    size = 4,
    position = position_dodge(width = 0.9)
    ) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point(mapping = aes(
      x = aphant_objX[aphant_objX < 10], 
      y = aphant_objY[aphant_objX < 10]
    ),
    size = 4,
    color = "red"
    ) +
  theme_classic() + 
  ggtitle("Object Memory") +
  xlab("Vividness") + 
  ylab("Error") +
  theme(
    plot.title = element_text(size = 32, hjust = 0.5),
    axis.text = element_text(size = 26),
    axis.title = element_text(size = 26),
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 26),
    strip.background = element_blank(),
    strip.text = element_text(size = 32),
    strip.text.y = element_blank()
    )

# Spatial memory vividness vs MS in aphantasics
aphant_spatX = avgSpatViv[wide_subjectData$group == "Aphantasic"]
aphant_spatY = avgSpatMS[wide_subjectData$group == "Aphantasic"]
ggplot(mapping = aes(x = aphant_spatX, y = aphant_spatY)) + 
  geom_point(
    stat = "identity",
    size = 4,
    position = position_dodge(width = 0.9)
    ) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point(mapping = aes(
      x = aphant_spatX[aphant_spatX < 10], 
      y = aphant_spatY[aphant_spatX < 10]
    ),
    size = 4,
    color = "red"
    ) +
  theme_classic() + 
  ggtitle("Spatial Memory") +
  xlab("Vividness") + 
  ylab("MS") +
  theme(
    plot.title = element_text(size = 32, hjust = 0.5),
    axis.text = element_text(size = 26),
    axis.title = element_text(size = 26),
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 26),
    strip.background = element_blank(),
    strip.text = element_text(size = 32),
    strip.text.y = element_blank()
    )

## Bar charts used to visualise results of main analysis
# Spatial memory vividness
subjectBarplot_spat_Viv <- subjectData |> 
  group_by(group, testPerspective, switchStatus) |> 
  summarise(
    n = n(), 
    mean = mean(SpatViv, na.rm = TRUE), 
    sd = sd(SpatViv, na.rm = TRUE), 
    se = sd / sqrt(n)
  ) |>
  ggplot(mapping = aes(
    x = testPerspective, 
    y = mean, 
    fill = switchStatus)
  ) + 
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9)
  ) +
  geom_errorbar(
    mapping = aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(width = 0.9),
    width = 0.5,
    size = 0.5
  ) +
  facet_wrap(
    . ~ group, 
    ncol = 2,
    strip.position = "top"
  ) +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    strip.text.y = element_blank()
  ) + 
  scale_fill_manual(
    values=c("steelblue4", "steelblue2"), 
    name = "Switch status"
  )

# Spatial memory score
subjectBarplot_spat_MS <- subjectData |> 
  group_by(group, testPerspective, switchStatus) |> 
  summarise(
    n = n(), 
    mean = mean(SpatMS, na.rm = TRUE), 
    sd = sd(SpatMS, na.rm = TRUE), 
    se = sd / sqrt(n)
  ) |>
  ggplot(
    mapping = aes(
      x = testPerspective, 
      y = mean, 
      fill = switchStatus)
  ) + 
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9)
  ) +
  geom_errorbar(
    mapping = aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(width = 0.9),
    width = 0.5,
    size = 0.5
  ) +
  facet_wrap(
    . ~ group, 
    ncol = 2,
    strip.position = "top"
  ) +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    strip.text.y = element_blank()
  ) + 
  scale_fill_manual(
    values=c("steelblue4", "steelblue2"), 
    name = "Switch status"
  )

# Object memory vividness
subjectBarplot_obj_viv <- subjectData_objMemOnly |>
  group_by(Group, perspective) |> 
  summarise(
    n = n(), 
    mean = mean(ObjViv, na.rm = TRUE), 
    sd = sd(ObjViv, na.rm = TRUE), 
    se = sd / sqrt(n)
  ) |>
  ggplot(
    mapping = aes(
      x = perspective, 
      y = mean,
      fill = perspective)
  ) + 
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9),
    width = 1
  ) +
  geom_errorbar(
    mapping = aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(width = 0.9),
    width = 0.5,
    size = 0.5
  ) +
  facet_wrap(
    . ~ Group, 
    ncol = 2,
    strip.position = "top"
  ) +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    strip.background = element_blank(),
    strip.text = element_text(size = 24),
    strip.text.y = element_blank(),
    legend.position = "none"
  ) +
  xlab("Study perspective") +
  ylab("Mean vividness") +
  scale_fill_manual(values = c("red4", "red2")) + 
  scale_x_discrete(labels = c("First person", "Third person"))

# Object memory accuracy
subjectBarplot_obj_succ <- mixtureModelData_pT |> 
  group_by(Group, perspective) |> 
  summarise(
    n = n(), 
    mean = mean(pT, na.rm = TRUE), 
    sd = sd(pT, na.rm = TRUE), 
    se = sd / sqrt(n)
  ) |>
  ggplot(
    mapping = aes(
      x = perspective, 
      y = mean,
      fill = perspective)
  ) + 
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9),
    width = 1
  ) +
  geom_errorbar(
    mapping = aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(width = 0.9),
    width = 0.5,
    size = 0.5
  ) +
  facet_wrap(
    . ~ Group, 
    ncol = 2,
    strip.position = "top"
  ) +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    strip.background = element_blank(),
    strip.text = element_text(size = 24),
    strip.text.y = element_blank(),
    legend.position = "none"
  ) +
  xlab("Study perspective") +
  ylab("Mean retrieval success") +
  scale_fill_manual(values = c("steelblue4", "steelblue2")) + 
  scale_x_discrete(labels = c("First person", "Third person"))

# Object memory precision
subjectBarplot_obj_prec <- mixtureModelData_pK |> 
  group_by(Group, perspective) |> 
  summarise(
    n = n(), 
    mean = mean(K, na.rm = TRUE), 
    sd = sd(K, na.rm = TRUE), 
    se = sd / sqrt(n)
  ) |>
  ggplot(
    mapping = aes(
      x = perspective, 
      y = mean,
      fill = perspective)
  ) + 
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9),
    width = 1
  ) +
  geom_errorbar(
    mapping = aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(width = 0.9),
    width = 0.5,
    size = 0.5
  ) +
  facet_wrap(
    . ~ Group, 
    ncol = 2,
    strip.position = "top"
  ) +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    strip.background = element_blank(),
    strip.text = element_text(size = 24),
    strip.text.y = element_blank(),
    legend.position = "none"
  ) +
  xlab("Study perspective") +
  ylab("Mean retrieval precision") +
  scale_fill_manual(values = c("steelblue4", "steelblue2")) + 
  scale_x_discrete(labels = c("First person", "Third person"))

# Object memory raw error
subjectBarplot_obj_err <- subjectData_objMemOnly |> 
  group_by(Group, perspective) |> 
  summarise(
    n = n(), 
    mean = mean(ObjErr, na.rm = TRUE), 
    sd = sd(ObjErr, na.rm = TRUE), 
    se = sd / sqrt(n)
  ) |>
  ggplot(
    mapping = aes(x = perspective, y = mean)
  ) + 
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9)
  ) +
  geom_errorbar(
    mapping = aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(width = 0.9),
    width = 0.5,
    size = 0.5
  ) +
  facet_wrap(
    . ~ Group, 
    ncol = 2,
    strip.position = "top"
  ) +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    strip.text.y = element_blank()
  ) + 
  scale_fill_manual(
    values=c("steelblue4", "steelblue2"), 
    name = "Switch status"
  ) +
  xlab("Study perspective") +
  ylab("Mean retrieval success") +
  scale_x_discrete(labels = c("First person", "Third person"))