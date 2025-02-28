# MTF POSSIBLE SELVES PROJECT

# Proportions per level --------------------------------------------------------
prop_sp <- mtf_svy %>%
  group_by(decade, gdsp) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% 
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

prop_pa <- mtf_svy %>%
  group_by(decade, gdpa) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% 
  mutate(vals     = round_percent(vals)/100, # round with preserving to 100%
         vals_low = round_percent(vals_low)/100, 
         vals_upp = round_percent(vals_upp)/100)

prop_wk <- mtf_svy %>%
  group_by(decade, gdwk) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% 
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

p00 <- df_prop %>%
  ggplot(aes(y = decade, x = vals, fill = cat, 
             ymin = vals_low, ymax = vals_upp)) +
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
p00

ggsave(file.path(here(outDir, figDir),"proportions.png"), p00, 
       width = 6.5, height = 8.5, dpi = 300, bg = 'white')





## Spouse
mod.sp      <- polr(gdsp ~ year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.sp.ha   <- polr(gdsp ~ happy_N_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.sp.ls   <- polr(gdsp ~ lifesat_N_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.sp.sc   <- polr(gdsp ~ selfconcept_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.sp.all  <- polr(gdsp ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                    year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)

## Parent
mod.pa      <- polr(gdpa ~ year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.pa.ha   <- polr(gdpa ~ happy_N_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.pa.ls   <- polr(gdpa ~ lifesat_N_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.pa.sc   <- polr(gdpa ~ selfconcept_std + year + sex + 
                    race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.pa.all  <- polr(gdpa ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                    year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)

## Worker
mod.wk      <- polr(gdwk ~ year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.wk.ha   <- polr(gdwk ~ happy_N_std + year + sex + 
                      race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.wk.ls   <- polr(gdwk ~ lifesat_N_std + year + sex + 
                      race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.wk.sc   <- polr(gdwk ~ selfconcept_std + year + sex + 
                      race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)
mod.wk.all  <- polr(gdwk ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                      year + sex + race + momed + religion + famstru + region, 
                    data = data, weights = svyweight, Hess = T)

broom::tidy(mod.sp)
m1 <- broom::tidy(mod.wk.all)
m2 <- broom::tidy(mod.sp.all)
m3 <- broom::tidy(mod.pa.all)

mods <- list(
  Worker = m1,
  Spouse = m2,
  Parent = m3)

class(mods) <- "modelsummary_list"

modelsummary(mods)


# this is the key step to use tidy data with modelsummary()
# need tidy day to exponentiate results (but not the cuts)
mod <- list(
  tidy = m1)
class(mod) <- "modelsummary_list"

modelsummary(mod)

# Combine the models
combined_results <- bind_rows(
  m1 %>% mutate(Model = "Worker"),
  m2 %>% mutate(Model = "Spouse"),
  m3 %>% mutate(Model = "Parent"))





# Get average predictions for specific predictor values
newdata <- data.frame(
  year = c(1984, 1995, 2022),
  selfconcept_std = c(-3.69, 0, 1.15))

predictions <- avg_predictions(mod.sp.all, 
                               variables = list(year = c(1992, 2002, 2012, 2022),
                               selfconcept_std = c(-3.69, 0, 1.15))) %>%
  mutate(selfconcept_std = fct_case_when(
    selfconcept_std == 1.15  ~ "Maximum",
    selfconcept_std == 0     ~ "Average",
    selfconcept_std == -3.69 ~ "Minimum"))

         
p7 <- predictions %>%
  ggplot(aes(x = as_factor(year) , y = estimate, 
             ymin = conf.low, ymax = conf.high, 
             fill = group )) +
  geom_col() +
  facet_wrap(vars(selfconcept_std)) +
  labs( x        = " ", 
        y        = " ", 
        fill     = "Spouse",
        title    = "Predicted proportions by self-concept scale",
        subtitle = "for key years",
        caption  = "Monitoring the Future 12th Grade Surveys (1976-2022)")

p7 

ggsave(file.path(here(outDir, figDir),"pp_spouse.png"), p7, 
       width = 6.5, height = 6.5, dpi = 300, bg = 'white')





models <- list(
  "Worker"     = polr(gdwk ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                        year + sex + race + momed + religion + famstru + region, 
                      data = data, weights = svyweight, Hess = T),
  "Spouse"     = polr(gdsp ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                        year + sex + race + momed + religion + famstru + region, 
                      data = data, weights = svyweight, Hess = T),
  "Parent"     = polr(gdpa ~ happy_N_std + lifesat_N_std +  selfconcept_std + 
                        year + sex + race + momed + religion + famstru + region, 
                      data = data, weights = svyweight, Hess = T))

summary_table_wk <- coef(summary(models[[1]]))
pval <- pnorm(abs(summary_table_wk[, "t value"]),lower.tail = FALSE)* 2
summary_table_wk <- cbind(summary_table_wk, "p value" = round(pval,3))
summary_table_wk


rows <- tribble(~term,          ~Worker,  ~Spouse, ~Parent,
                'Religiosity (ref = Never)', ' ',   ' ', ' ',
                'Family structure (ref = Both mother/father)', ' ', ' ', ' ',
                'Region (ref = Northeast)', ' ', ' ', ' ')
attr(rows, 'position') <- c(23, 30, 37)

tab2  <- modelsummary(models,
             gof_map = NA, stars = c("*" =.05, "**" = .01, "***" = .001),
             fmt = fmt_decimal(digits = 2, pdigits = 3),
             output = "huxtable", add_rows = rows,
             coef_rename = c("happy_N_std"      = "Happiness", 
                             "lifesat_N_std"    = "Life satisfaction",
                             "selfconcept_std"  = "Self-concept",
                             "year"             = "Year",
                             "sexWomen"         = "Women",
                             "raceBlack"        = "Black",
                             "momedCompleted college" = "Mom completed college",
                             "religionRarely"  = "Rarely",
                             "religionOnce or twice a month" = "1x - 2x a month",
                             "religionAbout once a week or more" = "1x + a week",
                             "famstruMother Only" = "Mother only",
                             "famstruFather Only" = "Father only",
                             "famstruNeither Mother/Father" = "Neither mother/father",
                             "regionMidwest"      = "Midwest",
                             "regionSouth"        = "South",
                             "regionWest"         = "West"))           %>%
  huxtable::as_flextable()                                             %>%
  add_footer_lines("Notes: N=50,200. Standard errors in parentheses.")

tab2

read_docx() %>% 
  body_add_par(paste("Table 2. Ordinal logistic regressions", sep="")) %>% 
  body_add_flextable(value = tab2)                                     %>% 
  print(target = file.path(outDir, "PS_table02.docx"))                 

avg.sp      <- avg_predictions(mod.sp, variables =  c("year"))
avg.pa      <- avg_predictions(mod.pa, variables =  c("year"))
avg.wk      <- avg_predictions(mod.wk, variables =  c("year"))
avg.sp.ha   <- avg_predictions(mod.sp.ha, variables =  c("year"))
avg.sp.ls   <- avg_predictions(mod.sp.ls, variables =  c("year"))
avg.sp.sc   <- avg_predictions(mod.sp.sc, variables =  c("year"))
avg.pa.sc   <- avg_predictions(mod.pa.sc, variables =  c("year"))
avg.wk.sc   <- avg_predictions(mod.wk.sc, variables =  c("year"))

avg.sp.ha      <- avg_predictions(mod.sp.all, variables =  c("happy_N_std"), 
                                   newdata = datagrid(year = c(1984, 2022)))
avg.sp.ls      <- avg_predictions(mod.sp.all, variables =  c("lifesat_N_std"), 
                                  newdata = datagrid(year = c(1984, 2022)))
avg.sp.sc      <- avg_predictions(mod.sp.all, variables =  c("selfconcept_std"), 
                                  newdata = datagrid(year = c(1984, 2022)))



 

avg.pa.all      <- avg_predictions(mod.pa.all, variables =  c("year"))
avg.wk.all      <- avg_predictions(mod.wk.all, variables =  c("year"))

avg.sp$cat        <- "Spouse"
avg.pa$cat        <- "Parent" 
avg.wk$cat        <- "Worker" 
avg.sp.ha$cat     <- "Spouse" 
avg.sp.ls$cat     <- "Spouse" 
avg.sp.sc$cat     <- "Spouse" 
avg.pa.sc$cat     <- "Parent" 
avg.wk.sc$cat     <- "Worker" 

avg.sp$index      <- "None" 
avg.pa$index      <- "None" 
avg.wk$index      <- "None" 
avg.sp.ha$index   <- "Happiness" 
avg.sp.ls$index   <- "Life satisfaction" 
avg.sp.sc$index   <- "Self-concept" 
avg.pa.sc$index   <- "Self-concept" 
avg.wk.sc$index   <- "Self-concept" 

avg_all <- rbind(avg.sp, avg.pa, avg.wk, avg.sp.ha, avg.sp.ls, avg.sp.sc, avg.pa.sc, avg.wk.sc)


avg_all %>%
  filter(index == "None" | index == "Self-concept") %>%
  ggplot(aes(x = year, y = estimate, color = group, ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_grid(cat ~ index) +
  theme(panel.grid.minor = element_blank()) +
  labs( x        = " ", 
        y        = " ", 
        title    = "Trends in expected work-family roles ",
        subtitle = "controlling for self-concept ",
        caption  = "Monitoring the Future 12th Grade Surveys (1984-2022)")


avg.sp.sc %>%
  ggplot(aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, color = group, fill = group)) +
  geom_line(linewidth = 1, na.rm=TRUE)



# year * gender
avg.sc.sp   <- avg_predictions(mod.sp.sc, variables =  c("year", "sex"))

avg.sc.sp %>%
  ggplot(aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, color = group, fill = group)) +
  geom_line(linewidth = 1, na.rm=TRUE) +
  facet_wrap(vars(sex))

