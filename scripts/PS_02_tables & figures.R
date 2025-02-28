#-------------------------------------------------------------------------------
# MTF POSSIBLE SELVES PROJECT
# PS_02_tables & figures.R
# Joanna R. Pepin & Melissa Milkie
#-------------------------------------------------------------------------------

# Create table 01 --------------------------------------------------------------
tab1 <- mtf_svy %>%
  gtsummary::select(c(gdwk, gdsp, gdpa,  
                      #decade,
                      sex, race, momed, region,
                      religion, famstru.d, 
                      selfconcept)) %>%
  tbl_svysummary(
#    by = decade,
#    type = list(c(happy_N_std, lifesat_N_std) ~ "continuous2"),
    type  = list(
      c(sex, race, momed) ~ "dichotomous"),
    value = list(
      sex = "Women", 
      race = "Black", 
      momed = "Completed college"),
    label = list(
      gdwk            ~ "Expectations as worker",
      gdsp            ~ "Expectations as spouse",
      gdpa            ~ "Expectations as parent",
      sex             ~ "Women",
      race            ~ "Black",
      momed           ~ "Mom completed college",
      religion        ~ "Religiosity",
      famstru.d       ~ "Both parents",
      region          ~ "Region",
      #     happy_N_std    ~ "Happiness",
      #     lifesat_N_std  ~ "Life satisfaction",
      selfconcept     ~ "Self-concept"),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{p}%"))  %>%
  modify_header(
    label  = '**Variable**',
    stat_0 = '**N (unweighted) = 50200**') %>%
  modify_caption("Table 01. Weighted statistics of the pooled analytic sample") %>%
  as_flex_table() 
#  add_footer_lines("notes")

tab1 # show table

save_as_docx(tab1, path = file.path(outDir, "PS_table01.docx"))

## WFE Trends ------------------------------------------------------------------
# Averages
avg_sp <- mtf_svy %>%
  select(year, gdsp_num) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdsp_num, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_pa <- mtf_svy %>%
  select(year, gdpa_num) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdpa_num, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_wk <- mtf_svy %>%
  select(year, gdwk_num) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdwk_num, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

#### Combine dfs
avg_sp$cat    <- "Spouse" 
avg_pa$cat    <- "Parent" 
avg_wk$cat    <- "Worker" 

df_avg <- rbind(avg_sp, avg_pa, avg_wk)

p1 <- df_avg %>%
  ggplot(aes(x = year, y = vals, color = cat, 
             ymin = vals_low, ymax = vals_upp)) +
#  geom_ribbon(fill = "lightgrey", linetype = "dotted", alpha=0.1) +
#  geom_line(linewidth = 1) +
  geom_smooth(alpha = .2, method="loess", se=TRUE, fullrange=FALSE, level=0.95) +
  geom_point(alpha = .5) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    panel.grid.minor = element_blank()) +
  guides(color = guide_legend(reverse=TRUE)) +
  ylim(3.5, 5) +
  labs( x        = " ", 
        y        = " ", 
        color    = " ",
        title    = "How good do you think you would be as a _________",
        subtitle = "Scale: (1) Poor - (5) Very good",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)
        Note: Survey years 2020, 2021, & 2022 were combined due to small sample sizes.")

p1 

ggsave(file.path(here(outDir, figDir),"PS_fig01.png"), p1, 
       width = 6, height = 5.5, dpi = 300, bg = 'white')




## PS Descriptives -------------------------------------------------------------

data_long <- data %>%
  pivot_longer(
    cols = c(happy, lifesat, 
             posatt, worth, welloth, satself,
             proud, nogood, wrong, lifeuse,
             meaning, enjoy, hopeless, alive, anxiety),
    names_to = "attribute",
    values_to = "response") %>%
  mutate(scale = case_when(
    response == "Completely dissatisfied"    | response == "Disagree"        | response == "Not too happy"  ~ 1,
    response == "Quite dissatisfied"         | response == "Mostly disagree" | response == "Pretty happy"   ~ 2,
    response == "Somewhat dissatisfied"      | response == "Neither"         | response == "Very happy"     ~ 3,
    response == "Neither, or mixed feelings" | response == "Mostly agree"                                   ~ 4,
    response == "Somewhat satisfied"         | response == "Agree"                                          ~ 5,
    response == "Quite satisfied"                                                                           ~ 6,
    response == "Completely satisfied"                                                                      ~ 7,
    TRUE                                                                                                    ~ NA_integer_)) %>%
  group_by(attribute) %>%
  mutate_at(c('scale'), ~(scale(.) %>% as.vector)) %>%
  ungroup()

table(data_long$response, data_long$attribute)

mtf_svy_l <- data_long %>%
  # weight data
  as_survey_design(id = 1,
                   weights = svyweight)

avg_mh <- mtf_svy_l %>%
  select(year, attribute, scale) %>%
  group_by(year, attribute) %>%
  summarize(vals  = survey_mean(scale, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2)) %>%
  mutate(index = fct_case_when(
    attribute == "happy"                                                        ~ "Happiness",
    attribute == "lifesat"                                                      ~ "Life satisfaction",
    attribute == "posatt"   | attribute == "worth"  | attribute == "welloth"  | 
      attribute == "satself"                                                    ~ "Self-esteem",
    attribute == "proud"    | attribute == "nogood" | attribute == "wrong"    | 
      attribute == "lifeuse"                                                    ~ "Self-derogation",
    attribute == "meaning"  | attribute == "enjoy"  | attribute == "hopeless" | 
      attribute == "alive"                                                      ~ "Depression",
    attribute == "anxiety"                                                      ~ "Anxiety",
    TRUE                                                                        ~  NA_character_)) 

p3 <- avg_mh %>%  
  filter(index != "Anxiety" & index != "Depression") %>%
  filter(vals != 0.00 & vals_low != 0.00 & vals_upp != 0.00) %>%
  ggplot(aes(x = year, y = vals, color = attribute, ymin = vals_low, ymax = vals_upp)) +
#  geom_ribbon(fill = "lightgrey", linetype = "dotted", alpha=0.1) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(. ~ index) +
  theme(panel.grid.minor = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends",
        subtitle = " ",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)")

p3

ggsave(file.path(here(outDir, figDir),"PS-trends.png"), p3, 
       width = 8.5, height = 6.5, dpi = 300, bg = 'white')

## Combined scale trends -------------------------------------------------------

# Averages
avg_esteem <- mtf_svy_l %>%
  select(year, esteem_std) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(esteem_std, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_derogation <- mtf_svy_l %>%
  select(year, derogation_std) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(derogation_std, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_selfconcept <- mtf_svy_l %>%
  select(year, selfconcept_std) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(selfconcept_std, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

#### Combine dfs
avg_esteem$index          <- "Self-esteem" 
avg_derogation$index      <- "Self-derogation" 
avg_selfconcept$index     <- "Self-concept" 

avg_esteem$attribute      <- "AVERAGE"
avg_derogation$attribute  <- "AVERAGE"
avg_selfconcept$attribute <- "AVERAGE"

df_scale_avg <- rbind(avg_mh, avg_esteem, avg_derogation, avg_selfconcept)

index_names <- c(
  `Anxiety`         = "Anxiety",
  `Depression`      = "Depression",
  `Self-derogation` = "Self-derogation",
  `Self-esteem`     = "Self-esteem",
  `Well-being`      = "Happiness",
  `Self-concept`    = "Self-concept")

p4 <- df_scale_avg %>%  
  filter((attribute == "AVERAGE" | attribute == "happy" | attribute == "lifesat") & index != "Self-concept") %>%
  filter(vals != 0.00 & vals_low != 0.00 & vals_upp != 0.00) %>%
  ggplot(aes(x = year, y = vals, color = index, ymin = vals_low, ymax = vals_upp)) +
  #  geom_ribbon(fill = "lightgrey", linetype = "dotted", alpha=0.1) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(. ~ index) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends",
        subtitle = " ",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)")

p4 

ggsave(file.path(here(outDir, figDir),"scale_averages.png"), p4, 
       width = 8.5, height = 6.5, dpi = 300, bg = 'white')


p5 <- df_scale_avg %>%  
  filter(attribute == "happy" | attribute == "lifesat" | index == "Self-concept") %>%
  filter(vals != 0.00 & vals_low != 0.00 & vals_upp != 0.00) %>%
  ggplot(aes(x = year, y = vals, color = index, ymin = vals_low, ymax = vals_upp)) +
  #  geom_ribbon(fill = "lightgrey", linetype = "dotted", alpha=0.1) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(. ~ index) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends in happiness, life satisfaction, & self-concept (esteem + derogation)",
        subtitle = " ",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)")

p5 

ggsave(file.path(here(outDir, figDir),"scale_averages2.png"), p5, 
       width = 8.5, height = 6.5, dpi = 300, bg = 'white')


## REGRESSIONS -----------------------------------------------------------------

write.dta(data, file = file.path(outDir, "data.dta"))

data$gdsp   <- relevel(data$gdsp,   ref = "Very good")
data$gdpa   <- relevel(data$gdpa,   ref = "Very good")
data$gdwk   <- relevel(data$gdwk,   ref = "Very good")

## Happiness
mod.ha.sp   <- glm(happy_N_std ~ gdsp * year, data = data, weights=svyweight)
mod.ha.pa   <- glm(happy_N_std ~ gdpa * year, data = data, weights=svyweight)
mod.ha.wk   <- glm(happy_N_std ~ gdwk * year, data = data, weights=svyweight)

avg.ha.sp   <- avg_predictions(mod.ha.sp, by = c("year", "gdsp"))
avg.ha.pa   <- avg_predictions(mod.ha.pa, by = c("year", "gdpa"))
avg.ha.wk   <- avg_predictions(mod.ha.wk, by = c("year", "gdwk"))

## Life satisfaction
mod.ls.sp   <- glm(lifesat_N_std ~ gdsp * year, data = data, weights=svyweight)
mod.ls.pa   <- glm(lifesat_N_std ~ gdpa * year, data = data, weights=svyweight)
mod.ls.wk   <- glm(lifesat_N_std ~ gdwk * year, data = data, weights=svyweight)

avg.ls.sp   <- avg_predictions(mod.ls.sp, by = c("year", "gdsp"))
avg.ls.pa   <- avg_predictions(mod.ls.pa, by = c("year", "gdpa"))
avg.ls.wk   <- avg_predictions(mod.ls.wk, by = c("year", "gdwk"))

## Self-concept
mod.sc.sp   <- glm(selfconcept_std ~ gdsp * year, data = data, weights=svyweight)
mod.sc.pa   <- glm(selfconcept_std ~ gdpa * year, data = data, weights=svyweight)
mod.sc.wk   <- glm(selfconcept_std ~ gdwk * year, data = data, weights=svyweight)

avg.sc.sp   <- avg_predictions(mod.sc.sp, by = c("year", "gdsp"))
avg.sc.pa   <- avg_predictions(mod.sc.pa, by = c("year", "gdpa"))
avg.sc.wk   <- avg_predictions(mod.sc.wk, by = c("year", "gdwk"))

# Matching and identifying columns
colnames(avg.ha.sp)[colnames(avg.ha.sp)=="gdsp"] <- "good"
colnames(avg.ha.pa)[colnames(avg.ha.pa)=="gdpa"] <- "good"
colnames(avg.ha.wk)[colnames(avg.ha.wk)=="gdwk"] <- "good"

colnames(avg.ls.sp)[colnames(avg.ls.sp)=="gdsp"] <- "good"
colnames(avg.ls.pa)[colnames(avg.ls.pa)=="gdpa"] <- "good"
colnames(avg.ls.wk)[colnames(avg.ls.wk)=="gdwk"] <- "good"

colnames(avg.sc.sp)[colnames(avg.sc.sp)=="gdsp"] <- "good"
colnames(avg.sc.pa)[colnames(avg.sc.pa)=="gdpa"] <- "good"
colnames(avg.sc.wk)[colnames(avg.sc.wk)=="gdwk"] <- "good"

avg.ha.sp$cat     <- "Spouse" 
avg.ha.pa$cat     <- "Parent" 
avg.ha.wk$cat     <- "Worker" 
avg.ls.sp$cat     <- "Spouse" 
avg.ls.pa$cat     <- "Parent" 
avg.ls.wk$cat     <- "Worker" 
avg.sc.sp$cat     <- "Spouse" 
avg.sc.pa$cat     <- "Parent" 
avg.sc.wk$cat     <- "Worker" 

avg.ha.sp$index   <- "Happiness" 
avg.ha.pa$index   <- "Happiness" 
avg.ha.wk$index   <- "Happiness" 
avg.ls.sp$index   <- "Life satisfaction" 
avg.ls.pa$index   <- "Life satisfaction" 
avg.ls.wk$index   <- "Life satisfaction" 
avg.sc.sp$index   <- "Self-concept" 
avg.sc.pa$index   <- "Self-concept" 
avg.sc.wk$index   <- "Self-concept" 

avg_all <- rbind(avg.ha.sp, avg.ha.pa, avg.ha.wk, 
                 avg.ls.sp, avg.ls.pa, avg.ls.wk,
                 avg.sc.sp, avg.sc.pa, avg.sc.wk)

avg_all$good <- factor(avg_all$good, 
                       levels=c('Very good', 'Good', 'Fairly good', 
                                'Not so good', 'Poor'))

p6 <- avg_all %>%
  ggplot(aes(x = year, y = estimate, color = cat, ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(good ~ index) +
  theme(panel.grid.minor = element_blank()) +
  ylim(-1.5, 0.5) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends in happiness, life satisfaction, & self-concept",
        subtitle = "by expected work-family roles",
        caption  = "Monitoring the Future 12th Grade Surveys (1984-2022)")

p6

ggsave(file.path(here(outDir, figDir),"predicted trends.png"), p6, 
       width = 8.5, height = 6.5, dpi = 300, bg = 'white')


