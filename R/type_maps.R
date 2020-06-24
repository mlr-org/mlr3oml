type_map_data_features = c(
  "index" = "int",
  "distinct" = "int",
  "missing" = "int",
  "min" = "dbl",
  "max" = "dbl",
  "mean" = "dbl",
  "stdev" = "dbl",
  "distr" = NA,
  "target" = NA
)

convert_type = function(dt, map) {
  Map(function(name, type) {
    value = dt[[name]]
    value = if (is.na(type)) {
      NULL
    } else {
      switch(type,
        "lgl" = as.logical(value),
        "int" = as.integer(value),
        "dbl" = as.double(value)
      )
    }
    set(dt, i = NULL, j = name, value = value)
  }, name = names(map), type = map)

  dt[]
}
