#' get_TMDB
#' 
#' @export

get_TMDB <- function(path) {
    try(
        httr::GET(
            url = httr::modify_url(
                url = 'https://api.themoviedb.org/', 
                path = glue::glue('3/{path}'), 
                query = list(
                    'api_key' = '01bcd861e341d2974588b841d9e8e0fc', 
                    'language' = 'en-US'
                )
            ), 
            httr::add_headers('content-type' = "application/json;charset=utf-8"), 
            httr::timeout(1)
        )
    )
}

get_TMDB_recommendation <- function(movie_id, n_recommendations = 10) {
    movie <- get_TMDB(glue::glue('movie/{movie_id}')) |>
        httr::content() |> 
        purrr::pluck('title')
    path <- glue::glue('movie/{movie_id}/recommendations')
    try({
        req <- get_TMDB(path) 
        movies <- httr::content(req) |> 
            purrr::pluck('results') |> 
            purrr::map_int(\(x) x$id)
        names(movies) <- httr::content(req) |> 
            purrr::pluck('results') |> 
            purrr::map_chr(\(x) x$title)
        movies <- movies[1:min(nrow(movies), n_recommendations)]
        movies[movie] <- movie_id
        return(movies)
    })
}