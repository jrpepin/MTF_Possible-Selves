#-------------------------------------------------------------------------------
# MTF MENTAL HEALTH PROJECT
# MH_02_tables & figures.R
# Joanna R. Pepin & Melissa Milkie
#-------------------------------------------------------------------------------

data_long <- data %>%
  pivot_longer(
    cols = c(goodsp, goodpa, goodwk),
    names_to = "good",
    values_to = "scale")

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

colnames(prop_sp)[colnames(prop_sp)=="goodsp"] <- "scale"
colnames(prop_pa)[colnames(prop_pa)=="goodpa"] <- "scale"
colnames(prop_wk)[colnames(prop_wk)=="goodwk"] <- "scale"


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
  facet_grid(vars(scale), vars(fct_rev(cat))) +
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

