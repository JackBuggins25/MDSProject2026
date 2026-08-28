##library
library(readxl)
library(writexl)
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(lme4)
library(knitr)
library(kableExtra)
library(lmerTest)
library(car)
library(performance)
library(factoextra)
library(corrplot)
library(effectsize)
library(interactions)
library(parameters)
library(sjPlot)
library(cluster)
library(ranger)
library(caret)
library(MuMIn)
library(ggeffects)
library(emmeans)

##Dataset
data_raw <- read_excel("project_data_new.xlsx", sheet = "rateatt_acl-recent")
names(data_raw)
dim(data_raw)

##Cleaning & Preprocessing
data_rem <- data_raw %>%
  select(-starts_with("...")) %>%
  select(-"ID...196") %>%
  rename(ID = "ID...1")

#filters any rows that are all blank, ignoring ID and subject
data_clean <- data_rem %>%
  filter(!if_all(-c(ID, subject), ~is.na(.))) 
dim(data_clean) #930,213

#remove rows that are all blank in trial_1 through image_order_60
trial_cols <- grep("^trial_", names(data_clean)) #all cols start with trial_
image_cols <- grep("^image_order_", names(data_clean)) #all cols start with image_order_
cols <- c(trial_cols, image_cols)

data_clean <- data_clean %>%
  filter(!if_all(all_of(cols), is.na))
dim(data_clean) #889,213

#duplicates
data_clean <- data_clean %>%
  distinct(ID, .keep_all = TRUE)
dim(data_clean) #889,213, no dupes

#uninformative rows
trial_cols <- paste0("trial_", 1:60) #selects all trial columns
data_clean$trial_sd <- apply(
  data_clean[, trial_cols],
  1,
  sd,
  na.rm = TRUE) #adds new column for SD of trials

data_clean$n_unique <- apply(
  data_clean[, trial_cols],
  1,
  function(x) length(unique(na.omit(x)))) #new column for n of unqiue responses within a pp

data_clean$uninformative <- data_clean$trial_sd < 0.15 | data_clean$n_unique < 3 #new column, TRUE when SD = 0
data_clean %>% filter(uninformative == TRUE) #inspect
data_clean <- data_clean %>%
  filter(!uninformative) %>%
  select(-"trial_sd", -"uninformative", -"n_unique") 

dim(data_clean)
#834, 213

#pairing
trial_cols <- paste0("trial_", 1:60)
order_cols <- paste0("image_order_", 1:60)
rt_cols    <- paste0("timeselect_", 1:60)
face_ids <- 1:60

# create empty columns
for (f in face_ids) {
  data_clean[[paste0("Face", f, "_Attr")]] <- NA
  data_clean[[paste0("Face", f, "_RT")]] <- NA}
for (row in 1:nrow(data_clean)) {
  for (i in 1:60) {
    face_id <- data_clean[[order_cols[i]]][row]
    attr    <- data_clean[[trial_cols[i]]][row]
    rt      <- data_clean[[rt_cols[i]]][row]
    data_clean[[paste0("Face", face_id, "_Attr")]][row] <- attr
    data_clean[[paste0("Face", face_id, "_RT")]][row]   <- rt}}
summary(is.na(data_clean)) #all good
data_clean <- data_clean %>%
  mutate(
    sexorient_clean = str_to_lower(str_trim(sexorient)), #lowercase & trim
    sexorient_clean = case_when(
      sexorient_clean == "hetero" ~ "Heterosexual",
      sexorient_clean %in% c("homo", "same") ~ "Homosexual",
      sexorient_clean == "bi" ~ "Bisexual",
      sexorient_clean == "asex" ~ "Asexual",
      sexorient_clean == "demi" ~ "Demisexual",
      sexorient_clean == "grey" ~ "Grey-sexual",
      sexorient_clean == "unsure" ~ "Unsure",
      TRUE ~ NA_character_),.after = sexorient) #cleans out the cells

##Fixing genderbirth & sexother

data_clean <- data_clean %>%
  mutate(
    gender_identity = case_when(
      genderbirth == "yes" ~ "Cisgender",
      genderbirth == "no" ~ sexother,
      TRUE ~ NA_character_),.after = sexother)

data_clean <- data_clean %>%
  mutate(
    sexother_clean = str_to_lower(sexother),
    
    gender_identity_clean = case_when(
      genderbirth == "yes" ~ "Cisgender",
      str_detect(sexother_clean, "transmasc|trans man|trans male|transfem|trans woman") ~ "Trans binary",
      str_detect(sexother_clean, "non[- ]?binary|genderqueer|genderfluid|genderflux|demigirl|demiboy|femby|girlflux|boyflux|nonbinary man|nonbinary woman|nonbinary male|nonbinary female") ~ "Non-binary",
      str_detect(sexother_clean, "agender|agenderflux|greygender") ~ "Agender",
      str_detect(sexother_clean, "not sure|i am not sure|unsure|none|queer|i am not sure yet|fae?") ~ "Unsure",
      TRUE ~ "Other / self-described"),.after = gender_identity)
##Fixing Country
data_clean <- data_clean %>%
  mutate(
    country_clean = str_trim(str_to_lower(country)), #converts everything to lowercase
    country_clean = case_when(
      country_clean %in% c(
        "uk", "u.k.", "united kingdom", "britain",
        "england", "scotland", "wales",
        "northern ireland", "london",
        "uk scotland", "united kingdom (scotland)"
      ) ~ "UK", #converts UK variants
      country_clean %in% c(
        "usa", "us", "u.s.", "united states", "america"
      ) ~ "USA", #convert any USA variants
      country_clean == "argetina" ~ "Argentina", #fix typo
      country_clean %in% c("hong kong", "hksar") ~ "Hong Kong", #convert to Hong Kong
      str_detect(country_clean, "prefer not") ~ "NA",
      country_clean == "white" ~ "NA",
      country_clean == "" ~ "NA", #sets any other entries to NA
      TRUE ~ str_to_title(country_clean)),.after = country) #Replaces captial letters
##Fixing Ethnicity
data_clean <- data_clean %>%
  mutate(
    ethnicity_clean = str_to_lower(ethnicity),
    ethnicity_clean = case_when(
      ethnicity_clean == "white" ~ "White", #Capitalised
      ethnicity_clean %in% c(
        "asian", "chinese", "indian", "pakistani",
        "bangladeshi", "japanese", "sea"
      ) ~ "Asian", #grouped all subtypes of Asian
      ethnicity_clean == "black" ~ "Black", #Capitalised
      ethnicity_clean == "hispanic" ~ "Hispanic", #Capitalised
      ethnicity_clean == "amerindian" ~ "Indigenous", #google suggests Indigenous
      ethnicity_clean %in% c("unselected", "other") ~ "NA", #Sets Others to NA
      TRUE ~ "Other"), .after = ethnicity)

png(
  "orientation_plot.png",
  width = 2400,
  height = 1600,
  res = 300) 
ggplot(orientation_data_prop, aes(x = Sex, y = prop, fill = Orientation)) +
  geom_col(width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Percentage of Participants",
    fill = "Orientation Classification"
  ) +
  theme_classic(base_size = 12)
dev.off()

##Long format
long_data <- data_clean %>%
  pivot_longer(
    cols = matches("^Face\\d+_(Attr|RT)$"),
    names_to = c("face_id", ".value"),
    names_pattern = "Face(\\d+)_(Attr|RT)"
  ) %>%
  mutate(face_id = as.integer(face_id))

long_data <- long_data %>%
  select(
    -matches("^trial_\\d+"),
    -matches("^timeselect_\\d+"),
    -matches("^image_order_\\d+")) 
#remove the unnecessary rows due to long data form

write_xlsx(
  list(data_raw = data_raw,
       data_clean = data_clean,
       long_data = long_data), "project_data_clean1.xlsx")

##Pairwise ratings
wide_ratings <- long_data %>%
  select(ID, face_id, Attr) %>% #a data set of just ID, face number and rating
  pivot_wider(
    names_from = face_id,
    values_from = Attr)

matrix_ratings <- wide_ratings %>%
  column_to_rownames("ID") %>%
  as.matrix()
matrix_ratings #creating a usable matrix
pairwise_corrs <- cor(t(matrix_ratings))


##Pairwise dataset for MLMs
pairwise_corrs_data <- as.data.frame(as.table(pairwise_corrs)) %>%
  rename(
    participant_i = Var1,
    participant_j = Var2,
    Correlation = Freq) #creating a df and renaming columns

pairwise_corrs_data <- pairwise_corrs_data %>%
  mutate(participant_i = as.numeric(as.character(participant_i)),
         participant_j = as.numeric(as.character(participant_j))) %>% #sets IDs as numeric to do the comparisons below...
  filter(participant_i != participant_j) %>% #removes any pairs of ID1, ID1
  mutate(pair_id = if_else(participant_i < participant_j,
                           paste(participant_i, participant_j, sep = "_"),
                           paste(participant_j , participant_i, sep = "_"))) %>% #creates a new column of (IDi_IDj)
  distinct(pair_id, .keep_all = TRUE) %>% #then remove any repeated rows since IDi_IDj == IDj_IDi
  select(-pair_id) #drops the temporary column

##Building the pairwise dataset

metadata <- data_clean %>%
  select(
    ID,
    age,
    sex,
    genderbirth,
    gender_identity_clean,
    ethnicity_clean,
    cultident,
    sexorient_clean,
    sexattany,
    Asex_binary,
    AsexIdentScore,
    sexAtt_Same,
    sexAtt_Opp,
    sexAtt_Intensity,
    sexAtt_Intensity_binary,
    sexAtt_Direction,
    romAtt_Same,
    romAtt_Opp,
    romAtt_Intensity,
    romAtt_Direction,
    libidoself,
    partaffectsself,
    partnership,
    sexAtt_Direction_binary)
names(long_data)

pairwise_data <- pairwise_corrs_data %>%
  left_join(metadata, by = c("participant_i" = "ID")) %>%
  rename_with(~ paste0(.x, "_i"), -c("participant_i", "participant_j", "Correlation"))

pairwise_data <- pairwise_data %>%
  left_join(metadata, by = c("participant_j" = "ID")) %>%
  rename_with(~ paste0(.x, "_j"), -c("participant_i", "participant_j", "Correlation", ends_with("_i")))

pairwise_data <- pairwise_data %>%
  filter(!is.na(partaffectsself_i),
         !is.na(partaffectsself_j))

#creating pairwise predictors
names(pairwise_data)
pairwise_data <- pairwise_data %>%
  mutate(age_difference = abs(age_i - age_j), # greater the value, the more different the ages
         same_sex = if_else(sex_i == sex_j, 1, 0), #1 = same sex, 0 different
         same_sexorient = if_else(sexorient_clean_i == sexorient_clean_j, 1, 0), #1 = same sexorient, 0 = different
         same_ethnicity = if_else(ethnicity_clean_i == ethnicity_clean_j, 1, 0), #1 = same ethnicity, 0 = different
         same_cultident = if_else(cultident_i == cultident_j, 1, 0), #1 = same cultident, 0 = different
         same_genderbirth = if_else(genderbirth_i == genderbirth_j, 1, 0), #1 = same gendbirth, 0 = different
         same_genderidentity = if_else(gender_identity_clean_i == gender_identity_clean_j, 1, 0), #1 = same, 0 = different
         same_sexatany = if_else(sexattany_i == sexattany_j, 1, 0, missing = NA_real_), #special case, 1 for same sexatany, 0 = different, NA if blank
         same_asex_binary = if_else(Asex_binary_i == Asex_binary_j, 1, 0), #1 = same, 0 = different
         same_sexAtt_direction_binary = if_else(sexAtt_Direction_binary_i == sexAtt_Direction_binary_j, 1, 0), #1 = same, 0 = different
         sexAtt_intensity_combo = paste(pmin(sexAtt_Intensity_binary_i, sexAtt_Intensity_binary_j),
                                        pmax(sexAtt_Intensity_binary_i, sexAtt_Intensity_binary_j),
                                        sep = "_"),
         sex_combo = paste(pmin(sex_i, sex_j),
                           pmax(sex_i, sex_j),
                           sep = "_"),
         same_partnership_status = if_else(partnership_i == partnership_j, 1, 0, missing = NA_real_), #both have partner = 1, 0 otherwie
         orientation_combo = paste(pmin(sexorient_clean_i, sexorient_clean_j),
                                   pmax(sexorient_clean_i, sexorient_clean_j),
                                   sep = "_"),
         identity_combo = paste(pmin(gender_identity_clean_i, gender_identity_clean_j),
                                pmax(gender_identity_clean_i, gender_identity_clean_j),
                                sep = "_"),
         Asex_binary_combo = paste(pmin(Asex_binary_i, Asex_binary_j),
                                   pmax(Asex_binary_i, Asex_binary_j),
                                   sep = "_"),
         asexIdentScore_dist = abs(AsexIdentScore_i - AsexIdentScore_j),
         sexAtt_intensity_dist = abs(sexAtt_Intensity_i - sexAtt_Intensity_j),
         sexAtt_direction_dist = abs(sexAtt_Direction_i - sexAtt_Direction_j),
         sexAtt_Opp_dist = abs(sexAtt_Opp_i - sexAtt_Opp_j),
         sexAtt_Same_dist = abs(sexAtt_Same_i - sexAtt_Same_j),
         romAtt_intensity_dist = abs(romAtt_Intensity_i - romAtt_Intensity_j),
         romAtt_direction_dist = abs(romAtt_Direction_i - romAtt_Direction_j),
         romAtt_Opp_dist = abs(romAtt_Opp_i - romAtt_Opp_j),
         romAtt_Same_dist = abs(romAtt_Same_i - romAtt_Same_j),
         partaffectself_dist = abs(partaffectsself_i - partaffectsself_j),
         libidoself_dist = abs(libidoself_i - libidoself_j))

names(pairwise_data)
write_xlsx(
  list(data_raw = data_raw,
       data_clean = data_clean,
       long_data = long_data,
       wide_ratings = wide_ratings,
       pairwise_data = pairwise_data), "project_data_clean1.xlsx")
pairwise_data <- read_excel("project_data_clean1.xlsx", sheet = "pairwise_data")

##model building

#scaling cont variables
pairwise_data <- pairwise_data %>%
  mutate(
    age_difference = scale(age_difference)[,1],
    sexAtt_intensity_dist = scale(sexAtt_intensity_dist, center = TRUE)[,1],
    romAtt_intensity_dist = scale(romAtt_intensity_dist)[,1],
    sexAtt_direction_dist = scale(sexAtt_direction_dist)[,1],
    romAtt_direction_dist = scale(romAtt_direction_dist)[,1],
    sexAtt_Same_dist = scale(sexAtt_Same_dist)[,1],
    romAtt_Same_dist = scale(romAtt_Same_dist)[,1],
    sexAtt_Opp_dist = scale(sexAtt_Opp_dist)[,1],
    romAtt_Opp_dist = scale(romAtt_Opp_dist)[,1],
    partaffectself_dist = scale(partaffectself_dist)[,1],
    asexIdentScore_dist = scale(asexIdentScore_dist)[,1])

## Correlations & Plots
meta_numeric <- metadata %>%
  select(where(is.numeric)) %>%
  select(-"ID") %>%
  select(-"age")
cor_mat1 <- cor(meta_numeric, use = "pairwise.complete.obs")
corrplot(cor_mat1, method = "color")

pairwise_numeric <- pairwise_data %>%
  select(where(is.numeric)) %>%
  select(-ends_with("_i")) %>%
  select(-ends_with("_j")) %>%
  select(-"Correlation_z",
         -"Correlation",
         -"partaffectself_dist",
         - "same_sexorient",
         - "same_cultident",
         - "same_genderbirth",
         - "same_sexAtt_direction_binary",
         - "sexAtt_Opp_dist",
         - "sexAtt_Same_dist",
         - "romAtt_Opp_dist",
         - "romAtt_Same_dist",
         - "same_genderidentity",
         - "libidoself_dist",
         -"same_sex",
         -"same_ethnicity",
         -"same_partnership_status")
cor_mat2 <- cor(pairwise_numeric, use = "pairwise.complete.obs")

colnames(cor_mat2) <- c("Age Dist",
                        "Same Attraction Classification",
                        "AIS-12 Dist",
                        "Sexual Intensity Dist",
                        "Sexual Direction Dist",
                        "Romantic Intensity Dist",
                        "Romantic Direction Dist")
rownames(cor_mat2) <- colnames(cor_mat2)

png(
  "corrplot.png",
  width = 2400,
  height = 2400,
  res = 300)
corrplot(cor_mat2, 
         method = "color",
         diag = FALSE,
         type = 'lower',
         addgrid.col = "black",
         addCoef.col = "black",
         number.cex = 0.65,
         tl.cex = 0.9)
dev.off()

png(
  "AgePlot.png",
  width = 2400,
  height = 1600,
  res = 300)
ggplot(pairwise_data, aes(x = age_difference)) +
  geom_histogram(
    bins = 20,
    fill = rgb(31,119,180, alpha = 60, maxColorValue = 255),
    colour = "black",
    linewidth = 0.4
  ) +
  labs(
    title = "Distribution of Age Difference",
    x = "Age Difference (Scaled)",
    y = "Frequency of Dyad Observations"
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
  )
dev.off()

ethnicity_counts <- as.data.frame(table(data_clean$ethnicity_clean))
ethnicity_counts$percentage <- ethnicity_counts$Freq / sum(ethnicity_counts$Freq) * 100
png(
  "EthnicPlot.png",
  width = 2400,
  height = 1600,
  res = 300)
ggplot(ethnicity_counts, aes(
  x = reorder(Var1, Freq),
  y = Freq
)) +
  geom_col(
    fill = rgb(31,119,180, alpha = 60, maxColorValue = 255),
    colour = "black",
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(Freq, " (", round(percentage, 1), "%)")),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  labs(
    title = "Ethnic Composition of the Participant Sample",
    x = "Ethnicity",
    y = "Number of Participants"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )
dev.off()
### Models

## Null Model
model_null <- lmer(Correlation ~ 1 +
                     (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(model_null)
model_parameters(model_null, ci = 0.95, digits = 5)

var_i <- as.data.frame(VarCorr(model_null))$vcov[1]
var_j <- as.data.frame(VarCorr(model_null))$vcov[2]
var_res <- as.data.frame(VarCorr(model_null))$vcov[3]

var_total <- var_i + var_j + var_res

vpc_i <- var_i / var_total
vpc_j <- var_j / var_total
vpc_res <- var_res / var_total

vpc_i #variance explained by participant i level effect, 0.013...
vpc_j #variance explained by participant j level effect, 0.011...
vpc_res #residual variability, 0.976...
  
### Sex Att Model
##Full sex model
names(pairwise_data)

modela <- lmer(Correlation ~ 1 + asexIdentScore_dist +
                     (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(modela)
modelb <- lmer(Correlation ~ 1 + sexAtt_intensity_dist +
                     (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(modelb)
anova(modela,modelb)

sexAtt_model_full <- lmer(Correlation ~ 1 + 
                            same_ethnicity + age_difference + same_partnership_status +
                            sexAtt_direction_dist * same_sex + 
                            sexAtt_intensity_dist * same_sex + 
                            (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(sexAtt_model_full)
AIC(sexAtt_model_full)
fullrs <- r2(sexAtt_model_full)
print(fullrs, digits = 6)

m2 <-  lmer(Correlation ~ 1 + 
              same_ethnicity + age_difference + same_partnership_status +
              sexAtt_intensity_dist * same_sex + 
              sexAtt_direction_dist +
              (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(m2)
AIC(m2)
anova(m2, sexAtt_model_full)
m2rs <- r2(m2)
print(m2rs, digits = 6)

m3 <-  lmer(Correlation ~ 1 + 
              same_ethnicity + same_partnership_status +
              sexAtt_intensity_dist * same_sex + 
              sexAtt_direction_dist +
              (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(m3)
AIC(m3)
anova(m3, m2)
m3rs <- r2(m3)
print(m3rs, digits = 6)

m4 <-  lmer(Correlation ~ 1 + 
              same_ethnicity +
              sexAtt_intensity_dist * same_sex + 
              sexAtt_direction_dist +
              (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(m4)
AIC(m4)
anova(m4, m3)
m4rs <- r2(m4)
print(m4rs, digits = 6)

m5 <-  lmer(Correlation ~ 1 + 
              same_ethnicity +
              sexAtt_intensity_dist + same_sex + 
              sexAtt_direction_dist +
              (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(m5)
AIC(m5)
anova(m5, m4)
m5rs <- r2(m5)
print(m5rs, digits = 6)

summary(m4)
model_parameters(m4, ci = 0.95, digits = 5)


sexAtt_interaction_model <- lmer(Correlation ~ 1 + 
                                   same_ethnicity + 
                                   sexAtt_direction_dist + 
                                   same_sex * sexAtt_intensity_dist + 
                                   (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(sexAtt_interaction_model)
model_parameters(sexAtt_interaction_model, ci = 0.95, digits = 6)

anova(sexAtt_model_full, sexAtt_interaction_model)

inter_rs <- r2(sexAtt_interaction_model)
print(inter_rs, digits = 6)

sexAtt_additive_model <- lmer(Correlation ~ 1 + 
                             same_ethnicity + 
                             sexAtt_direction_dist + 
                             same_sex + sexAtt_intensity_dist + 
                             (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(sexAtt_additive_model)
model_parameters(sexAtt_additive_model, ci = 0.95, digits = 6)

add_rs <- r2(sexAtt_additive_model)
print(add_rs, digits = 6)

anova(sexAtt_interaction_model, sexAtt_additive_model)
anova(sexAtt_additive_model, sexAtt_interaction_model, sexAtt_model_full)

add_rs <- r2(sexAtt_additive_model)
inter_rs <- r2(sexAtt_interaction_model)

print(add_rs, digits = 6)
print(inter_rs, digits = 6)

sexAtt_model_final <- lmer(Correlation ~ 1 + 
                               same_ethnicity + 
                               sexAtt_direction_dist +
                               same_sex * sexAtt_intensity_dist +
                               (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(sexAtt_model_final)

summary(emtrends(
  sexAtt_model_final,
  ~ same_sex,
  var = "sexAtt_intensity_dist"
), infer = c(TRUE,TRUE))

sexAtt_model_final2 <- lmer(Correlation ~ 1 + 
                             same_ethnicity + 
                             sexAtt_direction_dist +
                             same_sex * asexIdentScore_dist +
                             (1|participant_i) + (1|participant_j), data = pairwise_data, REML = FALSE)
summary(sexAtt_model_final2)
anova(sexAtt_model_final, sexAtt_model_final2)
model_parameters(sexAtt_model_final, ci = 0.95, digits = 8)

##Interact plot
pred <- ggpredict(
  sexAtt_model_final,
  terms = c("sexAtt_intensity_dist", "same_sex"))
pred$group <- factor(
  pred$group,
  levels = c("0", "1"),
  labels = c("Mixed-Sex Dyads", "Same-Sex Dyads"))

png(
  "interaction_term.png",
  width = 2400,
  height = 1600,
  res = 300)
ggplot(pred, aes(
  x = x,
  y = predicted,
  colour = group,
  fill = group
)) +
  geom_line(linewidth = 0.9) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.2,
    colour = NA
  ) +
  labs(
    x = "Difference in sexual attraction intensity",
    y = "Predicted facial attraction similarity",
    colour = "Dyad composition",
    fill = "Dyad composition"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.text = element_text(size = 12))
dev.off()

png(
  "sexAtt_model_diagnostics.png",
  width = 2400,
  height = 1600,
  res = 300)
par(mfrow = c(2, 2),
    mar = c(4, 4, 3, 1))

##SexAtt Model Diagnostic plots
# 1. Residuals vs fitted
plot(
  fitted(sexAtt_model_final),
  residuals(sexAtt_model_final),
  pch = 16,
  cex = 0.4,
  col = rgb(31,119,180, alpha = 60, maxColorValue = 255),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted",
  las = 1)

# 2. Residual Q-Q plot
qqnorm(
  residuals(sexAtt_model_final),
  pch = 16,
  cex = 0.7,
  col = rgb(31,119,180, alpha = 60, maxColorValue = 255),
  main = "Q-Q Plot \n (Residuals)",
  las = 1)
qqline(
  residuals(sexAtt_model_final),
  lwd = 2)


# 3. Participant i random effects Q-Q plot
qqnorm(
  ranef(sexAtt_model_final)$participant_i[,1],
  pch = 16,
  cex = 0.8,
  col = rgb(31,119,180, alpha = 60, maxColorValue = 255),
  main = "Q-Q Plot\n(Participant i)",
  las = 1)
qqline(
  ranef(sexAtt_model_final)$participant_i[,1],
  lwd = 2)

# 4. Participant j random effects Q-Q plot
qqnorm(
  ranef(sexAtt_model_final)$participant_j[,1],
  pch = 16,
  cex = 0.8,
  col = rgb(31,119,180, alpha = 60, maxColorValue = 255),
  main = "Q-Q Plot\n(Participant j)",
  las = 1)
qqline(
  ranef(sexAtt_model_final)$participant_j[,1],
  lwd = 2)
dev.off()

table(pairwise_data$Asex_binary_combo, pairwise_data$sex_combo)

interact_plot(sexAtt_model_final,
              pred = sexAtt_intensity_dist,
              modx = same_sex,
              interval = TRUE)

AIC(sexAtt_model_final)
check_collinearity(sexAtt_model_final)
mean(pairwise_data$sexAtt_intensity_dist)

### Refit sexAtt model with sex_combo

pairwise_data$sex_combo <- factor(pairwise_data$sex_combo)
levels(pairwise_data$sex_combo)
pairwise_data$sex_combo <- relevel(pairwise_data$sex_combo, ref = "female_male")

sexAtt_combo_model <- lmer(Correlation ~ 1 + 
                         same_ethnicity + 
                         sexAtt_direction_dist + 
                         sex_combo*sexAtt_intensity_dist +
                         (1|participant_i) +(1|participant_j), data = pairwise_data, REML = FALSE)
summary(sexAtt_combo_model)
model_parameters(sexAtt_combo_model, ci = 0.95, digits = 6)

slopes <- emtrends(
  sexAtt_combo_model,
  ~ sex_combo,
  var = "sexAtt_intensity_dist")

slopes
summary(slopes, infer = c(TRUE,TRUE))
pairs(slopes)
plot(slopes)

pred2 <- ggpredict(
  sexAtt_combo_model,
  terms = c("sexAtt_intensity_dist", "sex_combo"))
pred2$group <- factor(
  pred2$group,
  labels = c(
    "Female Dyads",
    "Mixed-Sex Dyads",
    "Male Dyads"))

png(
  "threeInter.png",
  width = 2400,
  height = 1600,
  res = 300)

plot(pred2) +
  labs(
    x = "Sexual attraction intensity difference (SD units)",
    y = "Predicted facial attraction Correlation",
    colour = "Dyad composition",
    title = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title.position = "top",
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold", hjust = 0.5))
dev.off()

### Investigate the FF dyads, decomposing into orientation combos
ff_data %>%
  group_by(Asex_binary_combo) %>%
  summarise(mean_correlation = mean(Correlation, na.rm = TRUE))

ff_data <- pairwise_data %>%
  subset(sex_combo == "female_female")
pairwise_data$Asex_binary_combo <- factor(pairwise_data$Asex_binary_combo)
pairwise_data$Asex_binary_combo <- relevel(pairwise_data$Asex_binary_combo, ref = "MoreAllo_MoreAsex")

ff_data$Asex_binary_combo <- factor(ff_data$Asex_binary_combo)
ff_data$Asex_binary_combo <- factor(ff_data$Asex_binary_combo)
ff_data$Asex_binary_combo <- relevel(ff_data$Asex_binary_combo, ref = "MoreAllo_MoreAsex")

combo_interact <- lmer(Correlation ~ 1 + 
                          same_ethnicity + 
                          Asex_binary_combo*sex_combo +
                          (1|participant_i) +(1|participant_j), data = pairwise_data, REML = FALSE)
summary(combo_interact) #use as suppoorting evidence

ff_combo <- lmer(Correlation ~ 1 + 
                         same_ethnicity +
                         Asex_binary_combo*sexAtt_intensity_dist + sexAtt_direction_dist + 
                         (1|participant_i) +(1|participant_j), data = ff_data, REML = FALSE)
summary(ff_combo) #use (2)
model_parameters(ff_combo, ci = 0.95, digits = 5)
check_collinearity(ff_combo)
emt <- emtrends(
  ff_combo,
  ~ Asex_binary_combo, var = "sexAtt_intensity_dist")
emt
summary(emt, infer = TRUE)
pairs(emt)
confint(emm, level = 0.95)
plot(emm, comparisons = TRUE)

### RomAtt Model
names(pairwise_data)
romFull <- lmer(Correlation ~ 1 +
                  same_ethnicity +
                  romAtt_intensity_dist*same_sex + 
                  romAtt_direction_dist*same_sex +
                  age_difference +
                  same_partnership_status +
                  (1|participant_i) +(1|participant_j), data = pairwise_data, REML = FALSE)
summary(romFull)
model_parameters(romFull, ci = 0.95, digits = 5)
AIC(romFull)
n0rs <- r2(romFull)
print(n0rs, digits = 6)
model_parameters(romFull, ci = 0.95, digits = 5)


n1 <- update(romFull, .~. - romAtt_intensity_dist:same_sex)
summary(n1)
anova(n1,romFull)
AIC(n1)
n1rs <- r2(n1)
print(n1rs, digits = 6)

n2 <- update(n1, .~. - same_sex:romAtt_direction_dist)
summary(n2)
anova(n2,n1)
AIC(n2)
n2rs <- r2(n2)
print(n2rs, digits = 6)
model_parameters(n2, ci = 0.95, digits = 5)
check_collinearity(n1)
check_collinearity(n2)
cov2cor(vcov(romFull))

n3 <- update(n2, .~. - romAtt_intensity_dist)
summary(n3)
anova(n3,n2)
AIC(n3)
n3rs <- r2(n3)
print(n3rs, digits = 6)

n4 <- update(n3, .~. - age_difference)
summary(n4)
anova(n4,n3)
AIC(n4)
n4rs <- r2(n4)
print(n4rs, digits = 6)

n5 <- update(n4, .~. - same_partnership_status)
summary(n5)
anova(n5,n4)
AIC(n5)
n5rs <- r2(n5)
print(n5rs, digits = 6)

n6 <- update(n5, .~. - same_sex)
summary(n6)
anova(n6, n5)
AIC(n6)
n6rs <- r2(n6)
print(n6rs, digits = 6)

romAtt_model_final <- lmer(Correlation ~ 1 +
                             same_ethnicity +
                             romAtt_direction_dist +
                             (1|participant_i) +(1|participant_j), data = pairwise_data, REML = FALSE)
summary(romAtt_model_final)
model_parameters(romAtt_model_final, ci = 0.95, digits = 5)

n7 <- update(romAtt_model_final, .~. - romAtt_direction_dist)
anova(n7, romAtt_model_final)
AIC(n7)
n7rs <- r2(n7)
print(n7rs, digits = 6)

table(data_clean$ethnicity_clean)
pairwise_data %>%
  count(ethnicity_clean_i, ethnicity_clean_j) %>%
  arrange(desc(n))

AIC(romAtt_model_final)
check_collinearity(romAtt_model_final)
anova(sexAtt_model_final, m8)
##RomAtt Diagnostic plots
png(
  "romAtt_model_diagnostics.png",
  width = 2400,
  height = 1600,
  res = 300)
par(mfrow = c(2, 2),
    mar = c(4, 4, 3, 1))

# 1. Residuals vs fitted
plot(
  fitted(m8),
  residuals(m8),
  pch = 16,
  cex = 0.4,
  col = rgb(31,119,180, alpha = 60, maxColorValue = 255),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted",
  las = 1)

# 2. Residual Q-Q plot
qqnorm(
  residuals(m8),
  pch = 16,
  cex = 0.7,
  col = rgb(31,119,180, alpha = 60, maxColorValue = 255),
  main = "Q-Q Plot \n (Residuals)",
  las = 1)

qqline(
  residuals(m8),
  lwd = 2)

# 3. Participant i random effects Q-Q plot
qqnorm(
  ranef(m8)$participant_i[,1],
  pch = 16,
  cex = 0.8,
  col = rgb(31,119,180, alpha = 60, maxColorValue = 255),
  main = "Q-Q Plot\n(Participant i)",
  las = 1)
qqline(
  ranef(m8)$participant_i[,1],
  lwd = 2)


# 4. Participant j random effects Q-Q plot
qqnorm(
  ranef(m8)$participant_j[,1],
  pch = 16,
  cex = 0.8,
  col = rgb(31,119,180, alpha = 60, maxColorValue = 255),
  main = "Q-Q Plot\n(Participant j)",
  las = 1)
qqline(
  ranef(m8)$participant_j[,1],
  lwd = 2)
dev.off()