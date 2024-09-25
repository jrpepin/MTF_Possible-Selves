#-------------------------------------------------------------------------------
# MTF MENTAL HEALTH PROJECT
# MH_02_tables & figures.R
# Joanna R. Pepin & Melissa Milkie
#-------------------------------------------------------------------------------

# Create table 01 --------------------------------------------------------------
tabA <- mtf_svy %>%
  select(c(gdsp, gdpa, gdwk, sex, race, momed)) %>%
  tbl_svysummary(
    label = list(gdsp     ~ "Good as a spouse",
                 gdpa     ~ "Good as a parent",
                 gdwk     ~ "Good as a worker",
                 sex      ~ "Gender",
                 race     ~ "Race",
                 momed    ~ "Mothers' education"))  %>%
  modify_header(
    label = '**Variable**',
    stat_0 = '**N (unweighted) = 102212**') %>%
  modify_caption("Weighted statistics of the pooled analytic sample") %>%
  as_flex_table() 
#  add_footer_lines("notes")

tabA # show table

save_as_docx(tabA, path = file.path(outDir, "tabA.docx"))


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

# Proportions per level
prop_sp <- mtf_svy %>%
  group_by(decade, gdsp) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

prop_pa <- mtf_svy %>%
  group_by(decade, gdpa) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

prop_wk <- mtf_svy %>%
  group_by(decade, gdwk) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

#### Combine dfs
prop_sp$cat    <- "Spouse" 
prop_pa$cat    <- "Parent" 
prop_wk$cat    <- "Worker" 

colnames(prop_sp)[colnames(prop_sp)=="gdsp"] <- "good"
colnames(prop_pa)[colnames(prop_pa)=="gdpa"] <- "good"
colnames(prop_wk)[colnames(prop_wk)=="gdwk"] <- "good"

df_prop <- rbind(prop_sp, prop_pa, prop_wk)


## Figures

p1 <- df_avg %>%
  ggplot(aes(x = year, y = vals, color = cat, ymin = vals_low, ymax = vals_upp)) +
  geom_ribbon(fill = "lightgrey", linetype = "dotted", alpha=0.1) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  guides(color = guide_legend(reverse=TRUE)) +
  ylim(3, 5) +
  labs( x        = " ", 
        y        = " ", 
        color    = " ",
        title    = "How good do you think you would be as a _________",
        subtitle = "annual averages (1 = Poor 5 = Very Good)",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)")

p1 

ggsave(file.path(here(outDir, figDir),"averages.png"), p1, width = 6.5, height = 8.5, dpi = 300, bg = 'white')


p2 <- df_prop %>%
  ggplot(aes(y = decade, x = vals, fill = cat, ymin = vals_low, ymax = vals_upp)) +
  geom_col(position = position_dodge()) +
  facet_grid(vars(good), vars(fct_rev(cat))) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  # stat_smooth(method = "lm", size = .5, fill = "grey80") +
  scale_y_discrete(limits = rev) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs( x        = " ", 
        y        = " ", 
        fill     = " ",
        title    = "How good do you think you would be as a _________",
        subtitle = "annual proportions",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)") 
p2

ggsave(file.path(here(outDir, figDir),"proportions.png"), p2, width = 6.5, height = 8.5, dpi = 300, bg = 'white')


## MH Descriptives -------------------------------------------------------------

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
    attribute == "happy"    | attribute == "lifesat"                                                   ~ "Well-being",
    attribute == "posatt"   | attribute == "worth"  | attribute == "welloth"  | attribute == "satself" ~ "Self-esteem",
    attribute == "proud"    | attribute == "nogood" | attribute == "wrong"    | attribute == "lifeuse" ~ "Self-derogation",
    attribute == "meaning"  | attribute == "enjoy"  | attribute == "hopeless" | attribute == "alive"   ~ "Depression",
    attribute == "anxiety"                                                                             ~ "Anxiety",
    TRUE                                                                                               ~  NA_character_)) 

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

ggsave(file.path(here(outDir, figDir),"MH-trends.png"), p3, width = 8.5, height = 6.5, dpi = 300, bg = 'white')

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

df_scale_avg$index <- factor(df_scale_avg$index, 
                             levels=c('Well-being', 
                                      'Self-concept',
                                      'Self-esteem',
                                      'Self-derogation',
                                      'Anxiety', 
                                      'Depression'))

p4 <- df_scale_avg %>%  
  filter((attribute == "AVERAGE" | attribute == "happy") & index != "Self-concept") %>%
  filter(vals != 0.00 & vals_low != 0.00 & vals_upp != 0.00) %>%
  ggplot(aes(x = year, y = vals, color = index, ymin = vals_low, ymax = vals_upp)) +
  #  geom_ribbon(fill = "lightgrey", linetype = "dotted", alpha=0.1) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(. ~ index, labeller = as_labeller(index_names)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends",
        subtitle = " ",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)")

p4 

ggsave(file.path(here(outDir, figDir),"scale_averages.png"), p4, width = 6.5, height = 8.5, dpi = 300, bg = 'white')


p5 <- df_scale_avg %>%  
  filter(attribute == "happy" | index == "Self-concept") %>%
  filter(vals != 0.00 & vals_low != 0.00 & vals_upp != 0.00) %>%
  ggplot(aes(x = year, y = vals, color = index, ymin = vals_low, ymax = vals_upp)) +
  #  geom_ribbon(fill = "lightgrey", linetype = "dotted", alpha=0.1) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(. ~ index, labeller = as_labeller(index_names)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends in happiness & self-concept (esteem + derogation)",
        subtitle = " ",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)")

p5 

ggsave(file.path(here(outDir, figDir),"scale_averages2.png"), p5, width = 6.5, height = 8.5, dpi = 300, bg = 'white')


## REGRESSIONS -----------------------------------------------------------------

summary(glm(selfconcept ~ gdsp + gdpa + gdwk + year, data = data, weights=svyweight))
summary(glm(happy_N     ~ gdsp + gdpa + gdwk + year, data = data, weights=svyweight)) # running as linear for now

