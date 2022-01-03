#' tmdb
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

#' tmdb_movies
#' 
#' @import magrittr
#' @export

tmdb_movies <- function() {
    pathtmdb_movies <- system.file("extdata", "tmdb_movies.rds", package = "movierecommendations")
    tmdb_movies <- readRDS(pathtmdb_movies) 
    return(tmdb_movies)
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

co_occurrencesGraph <- function(co_occurrences, tmdb) {
    nodes <- tibble::tibble(name = unique(c(co_occurrences$from, co_occurrences$to))) |>
        dplyr::left_join(tmdb$movies, by = c('name' = 'title'))
    edges <- tibble::tibble(from = co_occurrences$from, to = co_occurrences$to, weight = co_occurrences$co_occur) |>
        dplyr::filter(weight > 10)
    gr <- tidygraph::tbl_graph(edges = edges, nodes = nodes, node_key = 'name', directed = FALSE) |>
        dplyr::mutate(degree = tidygraph::centrality_degree()) |>
        dplyr::filter(!tidygraph::node_is_isolated()) |>
        dplyr::mutate(community = )
    return(gr)
}


