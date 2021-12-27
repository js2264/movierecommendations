#' loadTMDB
#' 
#' @import magrittr
#' @export

tmdb <- function() {
    pathDB <- system.file("extdata", "tmdb.sqlite", package = "movierecommendations")
    TMDB <- RSQLite::dbConnect(RSQLite::SQLite(), pathDB)
    db <- RSQLite::dbListTables(TMDB) |> purrr::map(\(x) dplyr::tbl(x, src = TMDB) |> dplyr::collect())
    names(db) <- RSQLite::dbListTables(TMDB)
    db$lists$listID <- as.numeric(db$lists$listID)
    RSQLite::dbDisconnect(TMDB)
    return(db)
}

#' co_occurrences
#' 
#' @export

co_occurrences <- function() {
    # co_occurrences <- co_occurrencesGraph(readRDS(system.file("extdata", "co_occurrences.rds", package = "movierecommendations")) , tmdb())
    pathco_occurrences <- system.file("extdata", "co_occurrences.rds", package = "movierecommendations")
    co_occurrences <- readRDS(pathco_occurrences) 
    return(co_occurrences)
}


