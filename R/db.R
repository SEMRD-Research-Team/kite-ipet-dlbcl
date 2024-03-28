kiteDb <- R6::R6Class("kiteDb",
  private = list(
    get_project_code_lists = function(project_id) {
      lookup <- self$table("kite_code_lists", "kite")

      dplyr::filter(lookup, project_id == !!project_id) |>
        dplyr::select("code_type", "code", "term", everything())
    }
  ),
  public = list(
    schema = "kite",
    schema_public = "public",
    connection = NULL,
    initialize = function() {
      self$connection <- coRanalysis::connect_to_db(database = "hesdata_processed_hiq")
      # Set seed on Redshift. Crucial for reproducing control sampling
      DBI::dbExecute(self$connection, "SET SESSION SEED TO 0.1234")
    },
    table = function(name, schema = self$schema) {
      dplyr::tbl(self$connection, dbplyr::in_schema(schema, name))
    },
    upload = function(df, name) {
      coRanalysis::upload_to_db(
        df,
        name,
        con = self$connection,
        schema_name_db = self$schema
      )
    }
  )
)
