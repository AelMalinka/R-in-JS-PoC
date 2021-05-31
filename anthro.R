needs("anthro")		# used for z_scores
needs("here")		# used to load files
needs("tidyverse")	# used to map data to what
needs("yaml")		# used for settings file
needs("srvyr")		# used for "Replication -> Proportions Tables"

# load file
file <- here::here("Anthro.as")
colspec <- cols_only(
	SURVDATE = col_date(format = "%Y-%m-%d"),
	CLUSTER = col_character(),
	TEAM = col_character(),
	ID = col_character(),
	HH = col_character(),
	SEX = col_factor(levels = c("m", "f")),
	BIRTHDAT = col_date(format = "%Y-%m-%d"),
	MONTHS = col_double(),
	WEIGHT = col_double(),
	HEIGHT = col_double(),
	EDEMA = col_factor(levels = c("n", "y")),
	MUAC = col_double(),
	MEASURE = col_factor(levels = c("l", "h")),
	CLOTHES = col_factor(levels = c("y"))
)

# read until blank lines (proprietary metadata after this point)
blank_line <- which(read_lines(file, skip = 1) == "")[1]

# mostly tab seperated data; this may generate warnings
raw_data <- read_tsv(file, col_types = colspec, skip = 1, n_max = blank_line - 3) %>%
	drop_na(SURVDATE)

# load in settings file
s <- read_yaml(here::here("settings.yaml"))

# generate z_scores using anthro lib
z_scores <- raw_data %>%
	mutate(
		weight_adj = case_when(
			CLOTHES == "y" ~ WEIGHT - (s$clothes_g / 1e3),
			TRUE ~ WEIGHT
		),
		height_adj = case_when(
			MONTHS < 24  & MEASURE == "h" ~ HEIGHT + 0.7,
			MONTHS >= 24 & MEASURE == "l" ~ HEIGHT - 0.7,
			TRUE ~ HEIGHT
		),
		refclass = case_when(
			is.na(MONTHS) & height_adj >= 87 ~ "child",
			MONTHS >= 24 ~ "child",
			TRUE ~ "infant"
		),
		across(SEX, as.integer)
	) %>%
	with(anthro_zscores(
		sex = SEX,
		age = MONTHS,
		weight = weight_adj,
		lenhei = height_adj,
		oedema = EDEMA,
		is_age_in_month = TRUE
	)) %>%
	select(
		waz = zwei,
		haz = zlen,
		whz = zwfl
	)

# cleanup data and attach z_scores
dat <- raw_data %>%
	bind_cols(z_scores) %>%
	rename_all(tolower) %>%
	mutate(
		across(sex, fct_recode, Boys = "m", Girls = "f"),
		age_grp = case_when(
			months < 6 ~ "< 6",
			months <= 17 ~ "6 to 17",
			months <= 29 ~ "18 to 29",
			months <= 41 ~ "30 to 41",
			months <= 53 ~ "42 to 53",
			months > 53  ~ ">= 54",
			TRUE ~ NA_character_
		),
		muac_grp = case_when(
			muac < 115  ~ "%<115mm",
			muac < 125  ~ "%115-125mm",
			muac >= 125 ~ "%>=125mm",
			TRUE ~ NA_character_
		)
	) %>%
	arrange(as.integer(cluster)) %>%
	mutate(
		across(age_grp, fct_reorder, .x = months),
		across(muac_grp, fct_reorder, .x = -muac),
		across(cluster, fct_inorder)
	)

# note: only the last value returned by this script will be sent to node; notating like below for easy finding since they will be ignored
# output (if last)
dat

# single stage cluster design object?
dsg <- as_survey_design(dat, ids = cluster)

# build exclusions
excl_name <- "who"
whz_exclusions <- quo(whz > s$z[[excl_name]]$whz[1] & whz < s$z[[excl_name]]$whz[2])

simple_prevalence <- function(grouping, exclusions, flag, dsg = dsg) {
	dsg %>%
		filter({{ exclusions }}) %>%
		mutate(.flag = {{ flag }}, All = "All") %>%
		group_by(level = {{ grouping }}, .flag) %>%
		summarise(x = n(), prop = survey_mean(vartype = c("ci"), proportion = TRUE)) %>%
		mutate(n = sum(x)) %>%
		filter(.flag) %>%
		ungroup %>%
		select(level, n, x, starts_with("prop"))
}

prevalence <- function(grouping, exclusions, flag, dsg = dsg) {
	res <- list()
	res[[1]] <- simple_prevalence(All, {{exclusions}}, {{flag}}, dsg)
	res[[2]] <- simple_prevalence({{grouping}}, {{exclusions}}, {{flag}}, dsg)
	bind_rows(res)
}

whz_prevalences <- function(grouping, svydsg = dsg) {
	res <- list()
	res$GAM <- prevalence(
		grouping	= {{ grouping}},
		exclusions	= {{ whz_exclusions }},
		flag		= whz < -2,
		svydsg
	)
	
	res$MAM <- prevalence(
		grouping	= {{ grouping}},
		exclusions	= {{ whz_exclusions }},
		flag		= (whz < -2 & whz >= -3) & edema == "n",
		svydsg
	)
	
	res$SAM <- prevalence(
		grouping	= {{ grouping}},
		exclusions	= {{ whz_exclusions }},
		flag		= whz < -3 | edema == "y",
		svydsg
	)
	
	dat2 <- filter(dat, {{ whz_exclusions }})
	
	res$footer$percent_edema <- dat %>%
		count(edema, .drop = FALSE) %>%
		mutate(prop = n / sum(n)) %>%
		filter(edema == "y") %>%
		pull(prop)

	res$footer$mean <- mean(dat2$whz, na.rm = TRUE)
	res$footer$sd	<- sd(dat2$whz, na.rm = TRUE)
	res$footer$n	<- length(dat2$whz)

	prop <- dsg %>%
		filter({{ whz_exclusions }}) %>%
		group_by(flag = whz < -2 & edema == "n") %>%
		summarise(n = n(), p = survey_prop(flag, vartype = "var"))
	
	vclus <- prop$p_var[1]
	vsrs <- prod(prop$p) / sum(prop$n)
	res$footer$deff <- vclus / vsrs
	
	res$footer$zscores_not_available <- sum(is.na(dat$whz))
	res$footer$zscores_out_of_range <- sum(!is.na(dat$whz)) - sum(!is.na(dat2$whz))
	
	res
}

# output (if last)
whz_prevalences(sex)