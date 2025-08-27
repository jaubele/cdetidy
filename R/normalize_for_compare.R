normalize_for_compare <- function(df) {
  df %>%
    dplyr::select(dplyr::any_of(c(
      "table_name","column_name","sql_type","null_flag","is_primary_key"
    ))) %>%
    dplyr::mutate(
      table_name  = trimws(as.character(table_name)),
      column_name = trimws(as.character(column_name)),

      sql_type = trimws(as.character(sql_type)),
      # DECIMAL/NUMERIC(p,0) -> INT, INT(n) -> INT, then upper-case
      sql_type = ifelse(grepl("^(decimal|numeric)\\s*\\(\\s*\\d+\\s*,\\s*0\\s*\\)$",
                              sql_type, ignore.case = TRUE), "INT", sql_type),
      sql_type = ifelse(grepl("^int\\s*\\(\\s*\\d+\\s*\\)$",
                              sql_type, ignore.case = TRUE), "INT", sql_type),
      sql_type = toupper(sql_type),

      null_flag = toupper(trimws(as.character(null_flag))),
      null_flag = dplyr::case_when(
        null_flag %in% c("NULL","IS NULL") ~ "NULL",
        null_flag %in% c("NOT NULL","NOTNULL","NO NULLS") ~ "NOT NULL",
        TRUE ~ null_flag
      ),

      is_primary_key = toupper(trimws(as.character(is_primary_key))),
      is_primary_key = dplyr::case_when(
        is_primary_key %in% c("Y","YES","TRUE","1") ~ "YES",
        is_primary_key %in% c("N","NO","FALSE","0","") ~ "NO",
        TRUE ~ is_primary_key
      )
    )
}
