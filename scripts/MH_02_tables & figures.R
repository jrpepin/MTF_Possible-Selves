#-------------------------------------------------------------------------------
# MTF MENTAL HEALTH PROJECT
# MH_02_tables & figures.R
# Joanna R. Pepin & Melissa Milkie
#-------------------------------------------------------------------------------


# Create table 01 -------------------------------------------------------------
tabA <- mtf_svy %>%
  select(c(goodsp, goodpa, goodwk, sex, race, momed)) %>%
  tbl_svysummary(
    label = list(goodsp   ~ "Good as a spouse",
                 goodpa   ~ "Good as a parent",
                 goodwk   ~ "Good as a worker",
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

## Construct Scales ------------------------------------------------------------

cor_matrix <- cor(data[, c('posatt_N', 'worth_N', 'welloth_N', 'satself_N',
                           'proud_N', 'nogood_N', 'wrong_N', 'lifeuse_N')], use = "pairwise.complete.obs")
p0 <- ggcorrplot(cor_matrix, type = "lower", lab = TRUE)

p0

ggsave(file.path(here(outDir, figDir),"alphas.png"), p0, width = 6.5, height = 6.5, dpi = 300, bg = 'white')

data %>%
  select('posatt_N', 'worth_N', 'welloth_N', 'satself_N') %>%
  drop_na() %>% # how many does this drop??
  cov() %>%
  psych::alpha()

# Averages
avg_sp <- mtf_svy %>%
  select(year, gdspnum) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdspnum, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_pa <- mtf_svy %>%
  select(year, gdpanum) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdpanum, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round(vals, digits = 2), 
         vals_low = round(vals_low, digits = 2), 
         vals_upp = round(vals_upp, digits = 2))

avg_wk <- mtf_svy %>%
  select(year, gdwknum) %>%
  group_by(year) %>%
  summarize(vals  = survey_mean(gdwknum, na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
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
  group_by(decade, goodsp) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

prop_pa <- mtf_svy %>%
  group_by(decade, goodpa) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

prop_wk <- mtf_svy %>%
  group_by(decade, goodwk) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

#### Combine dfs
prop_sp$cat    <- "Spouse" 
prop_pa$cat    <- "Parent" 
prop_wk$cat    <- "Worker" 

colnames(prop_sp)[colnames(prop_sp)=="goodsp"] <- "good"
colnames(prop_pa)[colnames(prop_pa)=="goodpa"] <- "good"
colnames(prop_wk)[colnames(prop_wk)=="goodwk"] <- "good"


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
    cols = c(goodsp, goodpa, goodwk),
    names_to = "role",
    values_to = "good")

#meaning, enjoy, hopeless, alive, anxiety, # Depression & Anxiety (2022+)

data_longer <- data_long %>%
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

table(data_longer$response, data_longer$attribute)

mtf_svy_l <- data_longer %>%
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

ggsave(file.path(here(outDir, figDir),"scale-trends.png"), p3, width = 8.5, height = 6.5, dpi = 300, bg = 'white')


