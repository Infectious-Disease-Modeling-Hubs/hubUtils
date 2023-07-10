# create_model_out_submit_tmpl works correctly

    Code
      str(create_model_out_submit_tmpl(hub_con, round_id = "2023-01-30"))
    Output
      tibble [3,132 x 6] (S3: tbl_df/tbl/data.frame)
       $ forecast_date : chr [1:3132] "2023-01-30" "2023-01-30" "2023-01-30" "2023-01-30" ...
       $ target        : chr [1:3132] "wk flu hosp rate change" "wk flu hosp rate change" "wk flu hosp rate change" "wk flu hosp rate change" ...
       $ horizon       : int [1:3132] 2 1 2 1 2 1 2 1 2 1 ...
       $ location      : chr [1:3132] "US" "US" "01" "01" ...
       $ output_type   : chr [1:3132] "pmf" "pmf" "pmf" "pmf" ...
       $ output_type_id: chr [1:3132] "large_decrease" "large_decrease" "large_decrease" "large_decrease" ...
       - attr(*, "out.attrs")=List of 2
        ..$ dim     : Named int [1:6] 1 1 2 54 1 5
        .. ..- attr(*, "names")= chr [1:6] "forecast_date" "target" "horizon" "location" ...
        ..$ dimnames:List of 6
        .. ..$ forecast_date : chr "forecast_date=2023-01-30"
        .. ..$ target        : chr "target=wk flu hosp rate change"
        .. ..$ horizon       : chr [1:2] "horizon=2" "horizon=1"
        .. ..$ location      : chr [1:54] "location=US" "location=01" "location=02" "location=04" ...
        .. ..$ output_type   : chr "output_type=pmf"
        .. ..$ output_type_id: chr [1:5] "output_type_id=large_decrease" "output_type_id=decrease" "output_type_id=stable" "output_type_id=increase" ...

---

    Code
      str(create_model_out_submit_tmpl(hub_con, round_id = "2023-01-16"))
    Output
      tibble [3,132 x 6] (S3: tbl_df/tbl/data.frame)
       $ forecast_date : chr [1:3132] "2023-01-16" "2023-01-16" "2023-01-16" "2023-01-16" ...
       $ target        : chr [1:3132] "wk flu hosp rate change" "wk flu hosp rate change" "wk flu hosp rate change" "wk flu hosp rate change" ...
       $ horizon       : int [1:3132] 2 1 2 1 2 1 2 1 2 1 ...
       $ location      : chr [1:3132] "US" "US" "01" "01" ...
       $ output_type   : chr [1:3132] "pmf" "pmf" "pmf" "pmf" ...
       $ output_type_id: chr [1:3132] "large_decrease" "large_decrease" "large_decrease" "large_decrease" ...
       - attr(*, "out.attrs")=List of 2
        ..$ dim     : Named int [1:6] 1 1 2 54 1 5
        .. ..- attr(*, "names")= chr [1:6] "forecast_date" "target" "horizon" "location" ...
        ..$ dimnames:List of 6
        .. ..$ forecast_date : chr "forecast_date=2023-01-16"
        .. ..$ target        : chr "target=wk flu hosp rate change"
        .. ..$ horizon       : chr [1:2] "horizon=2" "horizon=1"
        .. ..$ location      : chr [1:54] "location=US" "location=01" "location=02" "location=04" ...
        .. ..$ output_type   : chr "output_type=pmf"
        .. ..$ output_type_id: chr [1:5] "output_type_id=large_decrease" "output_type_id=decrease" "output_type_id=stable" "output_type_id=increase" ...

---

    Code
      str(create_model_out_submit_tmpl(hub_con, round_id = "2023-01-16",
        required_vals_only = TRUE))
    Message <cliMessage>
      ! Column "target" whose values are all optional included as all `NA` column.
      ! Round contains more than one modeling task (2)
      i See Hub's 'tasks.json' file or <hub_connection> attribute "config_tasks" for
        details of optional task ID/output_type/output_type ID value combinations.
    Output
      tibble [29 x 6] (S3: tbl_df/tbl/data.frame)
       $ forecast_date : chr [1:29] "2023-01-16" "2023-01-16" "2023-01-16" "2023-01-16" ...
       $ target        : chr [1:29] NA NA NA NA ...
       $ horizon       : int [1:29] 2 2 2 2 2 2 2 2 2 2 ...
       $ location      : chr [1:29] "US" "US" "US" "US" ...
       $ output_type   : chr [1:29] "pmf" "pmf" "pmf" "pmf" ...
       $ output_type_id: chr [1:29] "large_decrease" "decrease" "stable" "increase" ...
       - attr(*, "out.attrs")=List of 2
        ..$ dim     : Named int [1:5] 1 1 1 1 5
        .. ..- attr(*, "names")= chr [1:5] "forecast_date" "horizon" "location" "output_type" ...
        ..$ dimnames:List of 5
        .. ..$ forecast_date : chr "forecast_date=2023-01-16"
        .. ..$ horizon       : chr "horizon=2"
        .. ..$ location      : chr "location=US"
        .. ..$ output_type   : chr "output_type=pmf"
        .. ..$ output_type_id: chr [1:5] "output_type_id=large_decrease" "output_type_id=decrease" "output_type_id=stable" "output_type_id=increase" ...

---

    Code
      str(create_model_out_submit_tmpl(hub_con, round_id = "2023-01-16",
        required_vals_only = TRUE, remove_empty_cols = TRUE))
    Message <cliMessage>
      ! Column "target" whose values are all optional not included in template.
      ! Round contains more than one modeling task (2)
      i See Hub's 'tasks.json' file or <hub_connection> attribute "config_tasks" for
        details of optional task ID/output_type/output_type ID value combinations.
    Output
      tibble [29 x 5] (S3: tbl_df/tbl/data.frame)
       $ forecast_date : chr [1:29] "2023-01-16" "2023-01-16" "2023-01-16" "2023-01-16" ...
       $ horizon       : int [1:29] 2 2 2 2 2 2 2 2 2 2 ...
       $ location      : chr [1:29] "US" "US" "US" "US" ...
       $ output_type   : chr [1:29] "pmf" "pmf" "pmf" "pmf" ...
       $ output_type_id: chr [1:29] "large_decrease" "decrease" "stable" "increase" ...
       - attr(*, "out.attrs")=List of 2
        ..$ dim     : Named int [1:5] 1 1 1 1 5
        .. ..- attr(*, "names")= chr [1:5] "forecast_date" "horizon" "location" "output_type" ...
        ..$ dimnames:List of 5
        .. ..$ forecast_date : chr "forecast_date=2023-01-16"
        .. ..$ horizon       : chr "horizon=2"
        .. ..$ location      : chr "location=US"
        .. ..$ output_type   : chr "output_type=pmf"
        .. ..$ output_type_id: chr [1:5] "output_type_id=large_decrease" "output_type_id=decrease" "output_type_id=stable" "output_type_id=increase" ...

---

    Code
      str(create_model_out_submit_tmpl(hub_con, round_id = "2022-10-01"))
    Output
      tibble [5,184 x 6] (S3: tbl_df/tbl/data.frame)
       $ origin_date   : chr [1:5184] "2022-10-01" "2022-10-01" "2022-10-01" "2022-10-01" ...
       $ target        : chr [1:5184] "wk inc flu hosp" "wk inc flu hosp" "wk inc flu hosp" "wk inc flu hosp" ...
       $ horizon       : int [1:5184] 1 2 3 4 1 2 3 4 1 2 ...
       $ location      : chr [1:5184] "US" "US" "US" "US" ...
       $ output_type   : chr [1:5184] "mean" "mean" "mean" "mean" ...
       $ output_type_id: num [1:5184] NA NA NA NA NA NA NA NA NA NA ...

---

    Code
      str(create_model_out_submit_tmpl(hub_con, round_id = "2022-10-01",
        required_vals_only = TRUE))
    Message <cliMessage>
      ! Column "location" whose values are all optional included as all `NA` column.
      i See Hub's 'tasks.json' file or <hub_connection> attribute "config_tasks" for
        details of optional task ID/output_type/output_type ID value combinations.
    Output
      tibble [24 x 6] (S3: tbl_df/tbl/data.frame)
       $ origin_date   : chr [1:24] "2022-10-01" "2022-10-01" "2022-10-01" "2022-10-01" ...
       $ target        : chr [1:24] "wk inc flu hosp" "wk inc flu hosp" "wk inc flu hosp" "wk inc flu hosp" ...
       $ horizon       : int [1:24] 1 1 1 1 1 1 1 1 1 1 ...
       $ location      : chr [1:24] NA NA NA NA ...
       $ output_type   : chr [1:24] "mean" "quantile" "quantile" "quantile" ...
       $ output_type_id: num [1:24] NA 0.01 0.025 0.05 0.1 0.15 0.2 0.25 0.3 0.35 ...

---

    Code
      str(create_model_out_submit_tmpl(hub_con, round_id = "2022-10-29",
        required_vals_only = TRUE))
    Message <cliMessage>
      ! Column "location" whose values are all optional included as all `NA` column.
      i See Hub's 'tasks.json' file or <hub_connection> attribute "config_tasks" for
        details of optional task ID/output_type/output_type ID value combinations.
    Output
      tibble [24 x 7] (S3: tbl_df/tbl/data.frame)
       $ origin_date   : chr [1:24] "2022-10-29" "2022-10-29" "2022-10-29" "2022-10-29" ...
       $ target        : chr [1:24] "wk inc flu hosp" "wk inc flu hosp" "wk inc flu hosp" "wk inc flu hosp" ...
       $ horizon       : int [1:24] 1 1 1 1 1 1 1 1 1 1 ...
       $ location      : chr [1:24] NA NA NA NA ...
       $ age_group     : chr [1:24] "65+" "65+" "65+" "65+" ...
       $ output_type   : chr [1:24] "mean" "quantile" "quantile" "quantile" ...
       $ output_type_id: num [1:24] NA 0.01 0.025 0.05 0.1 0.15 0.2 0.25 0.3 0.35 ...

---

    Code
      str(create_model_out_submit_tmpl(hub_con, round_id = "2022-10-29",
        required_vals_only = TRUE, remove_empty_cols = TRUE))
    Message <cliMessage>
      ! Column "location" whose values are all optional not included in template.
      i See Hub's 'tasks.json' file or <hub_connection> attribute "config_tasks" for
        details of optional task ID/output_type/output_type ID value combinations.
    Output
      tibble [24 x 6] (S3: tbl_df/tbl/data.frame)
       $ origin_date   : chr [1:24] "2022-10-29" "2022-10-29" "2022-10-29" "2022-10-29" ...
       $ target        : chr [1:24] "wk inc flu hosp" "wk inc flu hosp" "wk inc flu hosp" "wk inc flu hosp" ...
       $ horizon       : int [1:24] 1 1 1 1 1 1 1 1 1 1 ...
       $ age_group     : chr [1:24] "65+" "65+" "65+" "65+" ...
       $ output_type   : chr [1:24] "mean" "quantile" "quantile" "quantile" ...
       $ output_type_id: num [1:24] NA 0.01 0.025 0.05 0.1 0.15 0.2 0.25 0.3 0.35 ...

# create_model_out_submit_tmpl errors correctly

    Code
      create_model_out_submit_tmpl(hub_con, round_id = "random_round_id")
    Error <rlang_error>
      `round_id` must be one of "2022-10-01", "2022-10-08", "2022-10-15", "2022-10-22", or "2022-10-29", not "random_round_id".

---

    Code
      create_model_out_submit_tmpl(hub_con)
    Error <rlang_error>
      `round_id` must be a character vector, not absent.

---

    Code
      create_model_out_submit_tmpl(hub_con)
    Error <rlang_error>
      `round_id` must be a character vector, not absent.

---

    Code
      create_model_out_submit_tmpl(list())
    Error <simpleError>
      Assertion on 'hub_con' failed: Must inherit from class 'hub_connection', but has class 'list'.
