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

# Create 3 tables to make by hand Table 2 (for now) ----------------------------

## Spouse ----------------------------------------------------------------------
polr1.sp  <- polr(gdsp ~ year.c + I(year.c^2),
                  data = data, weights = svyweight, Hess = T)

polr2.sp  <- polr(gdsp ~ year.c + I(year.c^2) + 
                    sex + race + momed + region + religion + famstru.d,
                  data = data, weights = svyweight, Hess = T)

polr3.sp  <- polr(gdsp ~ year.c + I(year.c^2) +  
                    selfconcept +
                    sex + race + momed + region + religion + famstru.d,
                  data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidy1.sp <- broom::tidy(polr1.sp)
tidy2.sp <- broom::tidy(polr2.sp)
tidy3.sp <- broom::tidy(polr3.sp)

## Transform output
tidy1.sp <- tidy1.sp %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidy2.sp <- tidy2.sp %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidy3.sp <- tidy3.sp %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

## Turn into modelsummary objects
mod1.sp        <- list(tidy = tidy1.sp)
class(mod1.sp) <- "modelsummary_list"

mod2.sp        <- list(tidy = tidy2.sp)
class(mod2.sp) <- "modelsummary_list"

mod3.sp        <- list(tidy = tidy3.sp)
class(mod3.sp) <- "modelsummary_list"

mods.sp <- list(
  "Model 1" = mod1.sp,
  "Model 2" = mod2.sp,
  "Model 3" = mod3.sp)

cm <- c('year.c'                             = 'Year',
        'I(year.c^2)'                        = 'Year squared',
        'sexWomen'                           = 'Women',
        'raceBlack'                          = 'Black',
        'momedCompleted college'             = 'Mom BA or more',
        'selfconcept'                        = 'Self-concept',
        'Poor|Not so good'                   = 'Poor|Not so good',
        'Not so good|Fairly good'            = 'Not so good|Fairly good',
        'Fairly good|Good'                   = 'Fairly good|Good',
        'Good|Very good'                     = 'Good|Very good')

tab2.sp <- modelsummary(mods.sp,
                        shape = term ~ model + statistic,
                        stars = c("*" =.05, "**" = .01, "***" = .001),
                        coef_map = cm,
                        fmt = fmt_decimal(digits = 3, pdigits = 3),
                        output = "huxtable") %>%
  huxtable::as_flextable()

tab2.sp

read_docx() %>% 
  body_add_par(paste("Table 2. Spouse", sep="")) %>% 
  body_add_flextable(value = tab2.sp)          %>% 
  print(target = file.path(outDir, "MH_table02_sp.docx"))                 


## Parent ----------------------------------------------------------------------
polr1.pa  <- polr(gdpa ~ year.c + I(year.c^2),
                  data = data, weights = svyweight, Hess = T)

polr2.pa  <- polr(gdpa ~ year.c + I(year.c^2) + 
                    sex + race + momed + region + religion + famstru.d,
                  data = data, weights = svyweight, Hess = T)

polr3.pa  <- polr(gdpa ~ year.c + I(year.c^2) +  
                    selfconcept +
                    sex + race + momed + region + religion + famstru.d,
                  data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidy1.pa <- broom::tidy(polr1.pa)
tidy2.pa <- broom::tidy(polr2.pa)
tidy3.pa <- broom::tidy(polr3.pa)

## Transform output
tidy1.pa <- tidy1.pa %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidy2.pa <- tidy2.pa %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidy3.pa <- tidy3.pa %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

## Turn into modelsummary objects
mod1.pa        <- list(tidy = tidy1.pa)
class(mod1.pa) <- "modelsummary_list"

mod2.pa        <- list(tidy = tidy2.pa)
class(mod2.pa) <- "modelsummary_list"

mod3.pa        <- list(tidy = tidy3.pa)
class(mod3.pa) <- "modelsummary_list"

mods.pa <- list(
  "Model 1" = mod1.pa,
  "Model 2" = mod2.pa,
  "Model 3" = mod3.pa)

cm <- c('year.c'                             = 'Year',
        'I(year.c^2)'                        = 'Year squared',
        'sexWomen'                           = 'Women',
        'raceBlack'                          = 'Black',
        'momedCompleted college'             = 'Mom BA or more',
        'selfconcept'                        = 'Self-concept',
        'Poor|Not so good'                   = 'Poor|Not so good',
        'Not so good|Fairly good'            = 'Not so good|Fairly good',
        'Fairly good|Good'                   = 'Fairly good|Good',
        'Good|Very good'                     = 'Good|Very good')

tab2.pa <- modelsummary(mods.pa,
                        shape = term ~ model + statistic,
                        stars = c("*" =.05, "**" = .01, "***" = .001),
                        coef_map = cm,
                        fmt = fmt_decimal(digits = 3, pdigits = 3),
                        output = "huxtable") %>%
  huxtable::as_flextable()  %>%
  add_footer_lines("Notes: N=50,200.")

tab2.pa

read_docx() %>% 
  body_add_par(paste("Table 2. Parent", sep="")) %>% 
  body_add_flextable(value = tab2.pa)          %>% 
  print(target = file.path(outDir, "MH_table02_pa.docx"))                 

## Worker ----------------------------------------------------------------------

# polr1.wk  <- polr(gdwk ~ poly(year.c, 2, raw = TRUE), data = data, weights = svyweight, Hess = T) <- same as I(year.c^2)

polr1.wk  <- polr(gdwk ~ year.c + I(year.c^2),
                  data = data, weights = svyweight, Hess = T)

polr2.wk  <- polr(gdwk ~ year.c + I(year.c^2) + 
                    sex + race + momed + region + religion + famstru.d,
                  data = data, weights = svyweight, Hess = T)

polr3.wk  <- polr(gdwk ~ year.c + I(year.c^2) +  
                    selfconcept +
                    sex + race + momed + region + religion + famstru.d,
                  data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidy1.wk <- broom::tidy(polr1.wk)
tidy2.wk <- broom::tidy(polr2.wk)
tidy3.wk <- broom::tidy(polr3.wk)

## Transform output
tidy1.wk <- tidy1.wk %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidy2.wk <- tidy2.wk %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

tidy3.wk <- tidy3.wk %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

## Turn into modelsummary objects
mod1.wk        <- list(tidy = tidy1.wk)
class(mod1.wk) <- "modelsummary_list"

mod2.wk        <- list(tidy = tidy2.wk)
class(mod2.wk) <- "modelsummary_list"

mod3.wk        <- list(tidy = tidy3.wk)
class(mod3.wk) <- "modelsummary_list"

mods.wk <- list(
  "Model 1" = mod1.wk,
  "Model 2" = mod2.wk,
  "Model 3" = mod3.wk)

cm <- c('year.c'                             = 'Year',
        'I(year.c^2)'                        = 'Year squared',
        'sexWomen'                           = 'Women',
        'raceBlack'                          = 'Black',
        'momedCompleted college'             = 'Mom BA or more',
        'selfconcept'                        = 'Self-concept',
        'Poor|Not so good'                   = 'Poor|Not so good',
        'Not so good|Fairly good'            = 'Not so good|Fairly good',
        'Fairly good|Good'                   = 'Fairly good|Good',
        'Good|Very good'                     = 'Good|Very good')

tab2.wk <- modelsummary(mods.wk,
                        shape = term ~ model + statistic,
                        stars = c("*" =.05, "**" = .01, "***" = .001),
                        coef_map = cm,
                        fmt = fmt_decimal(digits = 3, pdigits = 3),
                        output = "huxtable") %>%
  huxtable::as_flextable()

tab2.wk

read_docx() %>% 
  body_add_par(paste("Table 2. Work", sep="")) %>% 
  body_add_flextable(value = tab2.wk)          %>% 
  print(target = file.path(outDir, "MH_table02_wk.docx"))                 


# Create Figure 1 --------------------------------------------------------------

## Average Predictions 
pp_sp   <- predict_response(polr1.sp, terms = c("year.c [all]"))
pp_pa   <- predict_response(polr1.pa, terms = c("year.c [all]"))
pp_wk   <- predict_response(polr1.wk, terms = c("year.c [all]"))

### These all do the same thing, I think -- check against Stata later
#pp_sp   <- ggaverage(polr1.sp, terms = c("year.c [all]"))
#pp_sp   <- predict_response(polr1.sp, terms = c("year.c [all]"), margin = "marginalmeans")

## Combine dfs
pp_sp$cat    <- "Spouse" 
pp_pa$cat    <- "Parent" 
pp_wk$cat    <- "Worker" 

df_pp <- rbind(pp_wk, pp_sp, pp_pa)

## Tidy variables
df_pp$response.level <- factor(df_pp$response.level, 
                               levels=c("Very good", 
                                        "Good", 
                                        "Fairly good", 
                                        "Not so good", 
                                        "Poor"))

df_pp$cat <- factor(df_pp$cat, 
                    levels=c("Worker", 
                             "Spouse", 
                             "Parent"))

lables_year <- c("1984", "1995", "2005", "2015", "2023")


## Plot panels

p1_very <- df_pp %>%
  filter(response.level == "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = cat, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  facet_wrap(~response.level) +
  theme_minimal() + 
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position = "top",
    legend.title=element_blank(),
    strip.text.x = element_text(face = "bold"),
    panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.5, -1.54, 8.45, 18.45, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ")

p1_other <- df_pp %>%
  filter(response.level != "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = cat, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  facet_wrap(~response.level) +
  theme_minimal() + 
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position = "none",
    legend.title=element_blank(),
    strip.text.x = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5), limits=c(0, .5), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.5, -1.54, 8.45, 18.45, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ")

## Combine plots
p1 <- (p1_very | p1_other) + plot_layout(widths = c(1, 2)) +
  plot_annotation('How good do you think you would be as a _________', 
                  caption = "Monitoring the Future 12th Grade Surveys (1984-2023)")

p1

## Save Fig 1
agg_tiff(filename = file.path(here(outDir, figDir), "fig1.tif"), 
         width=8, height=6, units="in", res = 800, scaling = 1)

plot(p1)
invisible(dev.off())


# Figure 2 ---------------------------------------------------------------------

polr2.sp.sex  <- polr(gdsp ~ year.c * sex + I(year.c^2) + 
                    race + momed + region + religion + famstru.d,
                  data = data, weights = svyweight, Hess = T)

polr2.pa.sex  <- polr(gdpa ~ year.c * sex + I(year.c^2) + 
                        race + momed + region + religion + famstru.d,
                      data = data, weights = svyweight, Hess = T)

polr2.wk.sex  <- polr(gdwk ~ year.c * sex + I(year.c^2) + 
                        race + momed + region + religion + famstru.d,
                      data = data, weights = svyweight, Hess = T)

## Average Predictions 

pp_sp_sex   <- predict_response(polr2.sp.sex, terms = c("year.c [all]", "sex"))
pp_pa_sex   <- predict_response(polr2.pa.sex, terms = c("year.c [all]", "sex"))
pp_wk_sex   <- predict_response(polr2.wk.sex, terms = c("year.c [all]", "sex"))

## Combine dfs
pp_sp_sex$cat    <- "Spouse" 
pp_pa_sex$cat    <- "Parent" 
pp_wk_sex$cat    <- "Worker" 

df_pp_sex <- rbind(pp_wk_sex, pp_sp_sex, pp_pa_sex)

## Tidy variables
df_pp_sex$response.level <- factor(df_pp_sex$response.level, 
                               levels=c("Very good", 
                                        "Good", 
                                        "Fairly good", 
                                        "Not so good", 
                                        "Poor"))

df_pp_sex$cat <- factor(df_pp_sex$cat, 
                    levels=c("Spouse", 
                             "Parent",
                             "Worker"))

lables_year <- c("1984", "1995", "2005", "2015", "2023")

## Draw figure
p2 <- df_pp_sex %>%
  filter(response.level == "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  #  geom_bar(position="fill", stat="identity") +
  facet_grid(cols = vars(cat),
             scales = "free") +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position  = "right",
    legend.title     = element_blank(),
    strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.5, -1.54, 8.45, 18.45, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "% who think they would be a 'very good' ______")

p2

## Save Fig 2
agg_tiff(filename = file.path(here(outDir, figDir), "fig2.tif"), 
         width=8, height=6, units="in", res = 800, scaling = 1)

plot(p2)
invisible(dev.off())


# Figure 3 ---------------------------------------------------------------------

polr2.sp.race  <- polr(gdsp ~ year.c * race + I(year.c^2) + 
                        sex + momed + region + religion + famstru.d,
                      data = data, weights = svyweight, Hess = T)

polr2.pa.race  <- polr(gdpa ~ year.c * race + I(year.c^2) + 
                        sex + momed + region + religion + famstru.d,
                      data = data, weights = svyweight, Hess = T)

polr2.wk.race  <- polr(gdwk ~ year.c * race + I(year.c^2) + 
                        sex + momed + region + religion + famstru.d,
                      data = data, weights = svyweight, Hess = T)

## Average Predictions 

pp_sp_race   <- predict_response(polr2.sp.race, terms = c("year.c [all]", "race"))
pp_pa_race   <- predict_response(polr2.pa.race, terms = c("year.c [all]", "race"))
pp_wk_race   <- predict_response(polr2.wk.race, terms = c("year.c [all]", "race"))

## Combine dfs
pp_sp_race$cat    <- "Spouse" 
pp_pa_race$cat    <- "Parent" 
pp_wk_race$cat    <- "Worker" 

df_pp_race <- rbind(pp_wk_race, pp_sp_race, pp_pa_race)

## Tidy variables
df_pp_race$response.level <- factor(df_pp_race$response.level, 
                                   levels=c("Very good", 
                                            "Good", 
                                            "Fairly good", 
                                            "Not so good", 
                                            "Poor"))

df_pp_race$cat <- factor(df_pp_race$cat, 
                        levels=c("Spouse", 
                                 "Parent",
                                 "Worker"))

lables_year <- c("1984", "1995", "2005", "2015", "2023")

## Draw figure
p3 <- df_pp_race %>%
  filter(response.level == "Very good") %>%
  ggplot(aes(x = x, y = predicted, color = group, 
             ymin = conf.low, ymax = conf.high)) +
  geom_line(linewidth = 1) +
  #  geom_bar(position="fill", stat="identity") +
  facet_grid(cols = vars(cat),
             scales = "free") +
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    #legend.justification = c(1, 0),
    legend.position  = "right",
    legend.title     = element_blank(),
    strip.text.x     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.1, "cm", data = NULL)) +
  scale_y_continuous(breaks = c(0., .25, .5, .75), limits=c(0, .75), labels = scales::percent) +
  scale_x_continuous(breaks=c(-12.5, -1.54, 8.45, 18.45, 26.45), labels = lables_year) +
  labs( x        = " ", 
        y        = " ",
        title    = "% who think they would be a 'very good' ______")

p3

## Save Fig 3
agg_tiff(filename = file.path(here(outDir, figDir), "fig3.tif"), 
         width=8, height=6, units="in", res = 800, scaling = 1)

plot(p3)
invisible(dev.off())

