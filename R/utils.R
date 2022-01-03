#' title2id
#' 
#' @export

title2id <- function(titles, movies = movierecommendations::tmdb_movies()) {
    x <- movies |>
        dplyr::filter(unique_title %in% titles) |>
        dplyr::pull(id)
    if (length(x) == 0) x <- NA
    return(as.character(x))
}

#' id2title
#' 
#' @export

id2title <- function(ids, movies = movierecommendations::tmdb_movies()) {
    x <- movies |>
        dplyr::filter(id %in% ids) |>
        dplyr::pull(unique_title)
    if (length(x) == 0) x <- NA
    return(as.character(x))
}

#' searchField
#' 
#' @export

searchField <- function(pattern, movies = movierecommendations::tmdb_movies(), field = 'unique_title', return_first = FALSE) {
    entries <- dplyr::pull(movies, field)
    res <- agrep(pattern, entries, ignore.case = TRUE, value = TRUE)
    res <- res[order(stringdist::stringdist(pattern, res))]
    if (return_first | res[1] == pattern) {
        res[1]
    }
    else {
        res
    }
}

#' searchMovie
#' 
#' @export

searchMovie <- function(x, movies = movierecommendations::tmdb_movies(), field = 'title') {
    # x <- searchField(x, movies = movies, field = field, return_first = TRUE)
    x <- movies |>
        dplyr::filter(eval(rlang::parse_expr(field)) == x) |>
        dplyr::pull(id)
    dplyr::filter(movies, id == x)
}
