get_postgresql_datastore <- function (host, port, username, password, dbname) {
  db_connection <- NULL
  # TODO Introduce a connection pool to allow parallel/asynchronous execution.

  .open_database_connection_if_needed <- function () {
    if (is.null(db_connection) || !DBI::dbIsValid(db_connection)) {
      can_connect_response <- DBI::dbCanConnect(
        RPostgres::Postgres(),
        host = host,
        port = port,
        user = username,
        password = password,
        dbname = dbname
      )

      if (!can_connect_response) {
        error_msg <- paste("Database connection failed. Reason:", attr(can_connect_response, "reason"))
        stop(error_msg)
      }

      db_connection <<- DBI::dbConnect(
        RPostgres::Postgres(),
        host = host,
        port = port,
        user = username,
        password = password,
        dbname = dbname
      )
    }
  }

  load_dashboard_state <- function () {
    .open_database_connection_if_needed()
    return(tbl(db_connection, "dashboard_state"))
  }

  load_main_data <- function() {
    .open_database_connection_if_needed()
    return(tbl(db_connection, "dashboard_main_view"))
  }

  load_population_data <- function () {
    .open_database_connection_if_needed()
    return(tbl(db_connection, "dashboard_population_view"))
  }

  return(list(
    load_dashboard_state = load_dashboard_state,
    load_main_data = load_main_data,
    load_population_data = load_population_data,
    .db_connection = db_connection
  ))
}
