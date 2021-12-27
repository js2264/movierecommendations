#' get_TMDB
#' 
#' @export

get_TMDB <- function(path) {
    print(path)
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
            httr::timeout(3)
        )
    )
}
