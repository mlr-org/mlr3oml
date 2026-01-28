# Publish a Collection to OpenML

Publish a collection to OpenML This can also be achieved through the
[website](https://www.openml.org).

## Usage

``` r
publish_collection(
  ids,
  name,
  desc,
  main_entity_type = "task",
  alias = NULL,
  api_key = NULL,
  test_server = test_server_default()
)
```

## Arguments

- ids:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The IDs to include in the collection. Depending on the main entity
  tupe, these can be task or run IDs.

- name:

  (`character(1)`)  
  The name for the collection.

- desc:

  (`character(1)`)  
  The description of the collection.

- main_entity_type:

  (`character(1)`)  
  The main entity type of the collection. Can be either "task" or "run".

- alias:

  (`character(1)`)  
  The alias for the collection.

- api_key:

  (`character(1)`) The API key to perform the action, if left NULL it
  first tries the "mlr3oml.api_key" R option and then the environment
  variable `OPENMLAPIKEY`.

  In case `test_server` is TRUE (only relevant for developers) the test
  server API key is used, i.e. first the option "mlr3oml.test_api_key"
  and then the environment variable `TESTOPENMLAPIKEY`.

- test_server:

  (`character(1)`)  
  Whether to use the OpenML test server or public server. Defaults to
  value of option `"mlr3oml.test_server"`, or `FALSE` if not set.
