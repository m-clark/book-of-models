sd <- function(x, na.rm = TRUE) {
    sqrt(var(x, na.rm = na.rm))
}

# mean <- function(x, na.rm = TRUE) {
#     mean(x, na.rm = na.rm)
# }

word_sign <- function(value, words) {
    ifelse(sign(value) == 1, words[1], words[2])
}
