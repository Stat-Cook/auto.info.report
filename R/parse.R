parse_is <- function(is=is.character){
  function(vec){
    bool <- is(vec)
    nans <- rep(NA, length(vec))
    if (bool) {
      return(vec)
    }
    rep(NA, length(vec))
  }
}

parse_character <- parse_is(is.character)
parse_factor <- parse_is(is.factor)

# df |> mutate(across(.fn = parse_factor))

