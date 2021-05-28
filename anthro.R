needs("anthro")
needs("here")
needs("tidyverse")
needs("yaml")

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

blank_line <- which(read_lines(file, skip = 1) == "")[1]
raw_data <- read_tsv(file, col_types = colspec, skip = 1, n_max = blank_line - 3) %>%
	drop_na(SURVDATE)

s <- read_yaml(here::here("settings.yaml"))

raw_data %>%
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
	with(antro_zscores(
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