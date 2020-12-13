#' This creates a simple cache with a fixed maximal size. When it is full, it removes those entries that have not
#' been used for the longest time.
#'
#' @param cache_size (integer)
ts_create_cache <- function (cache_size) {
  cache <- list()
  cache_dataset_date <- NULL

  get <- function (id, dataset_date) {
    if (!identical(dataset_date, cache_dataset_date)) {
      return(NULL)
    }
    result <- cache[[id]]
    if (!is.null(result)) {
      cache[[id]]$last_used_date <<- lubridate::now()
    }
    return(result$data)
  }

  add <- function (id, dataset_date, data) {
    # When a new dataset arrives, the current cache will be invalidated.
    if (!identical(dataset_date, cache_dataset_date)) {
      cache <<- list()
      cache_dataset_date <<- dataset_date
    }

    # If the cache is full, the value that have not been used for the longest time will be removed.
    if (length(cache) >= cache_size) {
      cache <<- cache[order(sapply(cache, function(x) x$last_used_date))]
      cache[[1]] <<- NULL
    }

    # Add data to cache
    cache[[id]] <<- list(
      data = data,
      last_used_date = lubridate::now()
    )

    return(NULL)
  }

  return(list(
    get = get,
    add = add
  ))
}
