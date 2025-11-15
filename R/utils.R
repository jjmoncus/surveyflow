
#'
is_full <- function(x) {
  # full = all not NA
  all(!is.na(x))
}
#'
is_empty <- function(x) {
  # empty = all NA
  all(is.na(x))
}
#'
is_semifull <- function(x) {

  # neither empty nor full
  (!is_empty(x)) && (!is_full(x))
}

#'
is_pure <- function(x) {{
  # all present values are the same

  # if empty, it is trivially pure
  empty <- is_empty(x)
  pure <- empty

  if (!empty) {
    # if not integer-like, make integer like, then cast as integer
    if (typeof(x) != "integer") x <- factor(x, exclude = NA)
    x <- as.integer(x)

    pure <- ifelse(
      # if x only has 1 non-empty value, sd will break by default, and it is pure by default
      length(x[!is.na(x)]) == 1,
      TRUE,
      # if more than 1 non-empty value, sd tells us variation
      ifelse(sd(x, na.rm = TRUE) > 0,
             FALSE,
             TRUE)) # removing NAs to account for whether sparse or dense
  }

  return( pure )
}}

#'
is_dense <- function(x) {

  # if not a factor, then we have no concept of spread across "some" candidate values, so shortcut exit
  if (!is.factor(x)) return( "undefined" )
  # if pure, trivially not mixed
  pure <- is_pure(x)
  dense <- !pure
  # if not pure, check has density over all candidate levels
  if (!pure) dense <- table(x) %>% {. > 0} %>% all()

  return( dense )
}

#'
is_sparse <- function(x) {
  # returns either TRUE, FALSE, or undefined
  return( (!is_pure(x)) && (!is_dense(x)) )
}

#'
fullness <- function(x) {

  if (is_empty(x)) return( "empty" )
  if (is_full(x)) return( "full")
  return( "semi_full")
}

#'
density <- function(x) {

  if (is_dense(x) == "undefined") return( "undefined")

  if (is_pure(x)) return( "pure" )
  if (is_dense(x)) return( "dense" )
  return( "sparse" )
}

#'
#'@importFrom purrr map_chr
#'
birds_eye <- function(data) {

  densities <- map_chr(data, density) %>% factor(levels = c("pure", "sparse",  "dense", "undefined"))
  fullies <- map_chr(data, fullness) %>% factor(levels = c("empty", "semi_full", "full"))
  return( table(densities, fullies, useNA = "always") )
}

