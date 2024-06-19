#' @export
get_task_id_cols <- function(model_out_tbl) {
  model_out_cols <- colnames(model_out_tbl)
  task_id_cols <- model_out_cols[!model_out_cols %in% std_colnames]
  return(task_id_cols)
}
