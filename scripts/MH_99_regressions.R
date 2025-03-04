# sex + race + momed + region + year.c + year.sq + religion + famstru.d + selfconcept, 

polr0.wk  <- polr(gdwk ~ sex + race + momed + region,
                    data = data, weights = svyweight, Hess = T)
polr1.wk  <- polr(gdwk ~ sex + race + momed + region + year.c + year.sq,
                 data = data, weights = svyweight, Hess = T)
polr2.wk  <- polr(gdwk ~ sex + race + momed + region + year.c + year.sq + religion + famstru.d,
                 data = data, weights = svyweight, Hess = T)
polr3.wk  <- polr(gdwk ~ sex + race + momed + region + year.c + year.sq + religion + famstru.d + selfconcept,
                 data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidy0.wk <- broom::tidy(polr0.wk)
tidy1.wk <- broom::tidy(polr1.wk)
tidy2.wk <- broom::tidy(polr2.wk)
tidy3.wk <- broom::tidy(polr3.wk)

## Transform output
tidy0.wk <- tidy0.wk %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

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
mod0.wk        <- list(tidy = tidy0.wk)
class(mod0.wk) <- "modelsummary_list"

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

cm <- c('year.c'      = 'Year',
        'year.sq'     = 'Year squared',
        'religionRarely' = 'Rarely',
        'religionOnce or twice a month' = '1x-2x a month',
        'religionAbout once a week or more' = '1x a week +',
        'famstru.d' = "Live with both mom & dad",
        'selfconcept' = 'Self-concept',
        'Poor|Not so good' = 'Poor|Not so good',
        'Not so good|Fairly good' = 'Not so good|Fairly good',
        'Fairly good|Good' = 'Fairly good|Good',
        'Good|Very good' = 'Good|Very good')

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

## Spouse ----------------------------------------------------------------------
polr0.sp  <- polr(gdsp ~ sex + race + momed + region,
                  data = data, weights = svyweight, Hess = T)
polr1.sp  <- polr(gdsp ~ sex + race + momed + region + year.c + year.sq,
                  data = data, weights = svyweight, Hess = T)
polr2.sp  <- polr(gdsp ~ sex + race + momed + region + year.c + year.sq + religion + famstru.d,
                  data = data, weights = svyweight, Hess = T)
polr3.sp  <- polr(gdsp ~ sex + race + momed + region + year.c + year.sq + religion + famstru.d + selfconcept,
                  data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidy0.sp <- broom::tidy(polr0.sp)
tidy1.sp <- broom::tidy(polr1.sp)
tidy2.sp <- broom::tidy(polr2.sp)
tidy3.sp <- broom::tidy(polr3.sp)

## Transform output
tidy0.sp <- tidy0.sp %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

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
mod0.sp        <- list(tidy = tidy0.sp)
class(mod0.sp) <- "modelsummary_list"

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

cm <- c('year.c'      = 'Year',
        'year.sq'     = 'Year squared',
        'religionRarely' = 'Rarely',
        'religionOnce or twice a month' = '1x-2x a month',
        'religionAbout once a week or more' = '1x a week +',
        'famstru.d' = "Live with both mom & dad",
        'selfconcept' = 'Self-concept',
        'Poor|Not so good' = 'Poor|Not so good',
        'Not so good|Fairly good' = 'Not so good|Fairly good',
        'Fairly good|Good' = 'Fairly good|Good',
        'Good|Very good' = 'Good|Very good')

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
polr0.pa  <- polr(gdpa ~ sex + race + momed + region,
                  data = data, weights = svyweight, Hess = T)
polr1.pa  <- polr(gdpa ~ sex + race + momed + region + year.c + year.sq,
                  data = data, weights = svyweight, Hess = T)
polr2.pa  <- polr(gdpa ~ sex + race + momed + region + year.c + year.sq + religion + famstru.d,
                  data = data, weights = svyweight, Hess = T)
polr3.pa  <- polr(gdpa ~ sex + race + momed + region + year.c + year.sq + religion + famstru.d + selfconcept,
                  data = data, weights = svyweight, Hess = T)

## Turn into tidy dataframes
tidy0.pa <- broom::tidy(polr0.pa)
tidy1.pa <- broom::tidy(polr1.pa)
tidy2.pa <- broom::tidy(polr2.pa)
tidy3.pa <- broom::tidy(polr3.pa)

## Transform output
tidy0.pa <- tidy0.pa %>%
  mutate(z_scores = estimate/std.error,
         p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
         estimate = case_when(
           coef.type == "coefficient" ~ exp(estimate),
           coef.type == "scale"       ~ estimate))

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
mod0.pa        <- list(tidy = tidy0.pa)
class(mod0.pa) <- "modelsummary_list"

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

cm <- c('year.c'      = 'Year',
        'year.sq'     = 'Year squared',
        'religionRarely' = 'Rarely',
        'religionOnce or twice a month' = '1x-2x a month',
        'religionAbout once a week or more' = '1x a week +',
        'famstru.d' = "Live with both mom & dad",
        'selfconcept' = 'Self-concept',
        'Poor|Not so good' = 'Poor|Not so good',
        'Not so good|Fairly good' = 'Not so good|Fairly good',
        'Fairly good|Good' = 'Fairly good|Good',
        'Good|Very good' = 'Good|Very good')

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



