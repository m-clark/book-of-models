sd <- function(x, na.rm = TRUE) {
    sqrt(var(x, na.rm = na.rm))
}

# mean <- function(x, na.rm = TRUE) {
#     mean(x, na.rm = na.rm)
# }

word_sign <- function(value, words) {
    ifelse(sign(value) == 1, words[1], words[2])
}


round_any = function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

skimmer = function() {
	skimr::skim_with(
		# reordering/naming/slimming numeric output
		numeric = skimr::sfl(
			mean = ~ mean(., na.rm = TRUE),
			sd   = ~ sd(., na.rm = TRUE),
			min  = ~ min(., na.rm = TRUE),
			med  = ~ median(., na.rm = TRUE),
			max  = ~ max(., na.rm = TRUE),
			iqr  = NULL,
			hist = NULL,
			p0   = NULL,  # renamed
			p25  = NULL,
			p50  = NULL,  # renamed
			p75  = NULL,
			p100 = NULL   # renamed
		),

		character = skimr::sfl(
			empty  = \(x) skimr::n_empty(x) + skimr::n_whitespace(x), # replace default which is only n_empty
      whitespace = NULL,
      min = NULL,  # these refer to nchar which I doubt anyone would know
      max = NULL,
		),
		append = TRUE
	)
}

summarize_data = function(data, types = 'all') {
	init = skimmer()(data)
	summ = skimr::partition(init)

	if (!all(types == 'all')) {
		summ = summ[tolower(names(summ)) %in% tolower(types)]
	}

	summ = purrr::map(summ, tibble::tibble)

	return(summ)
}