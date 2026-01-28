# Publish a task on OpenML

Publish a task on OpenML. This can also be achieved through the
[website](https://www.openml.org).

## Usage

``` r
publish_task(
  id,
  type,
  estimation_procedure,
  target,
  api_key = NULL,
  test_server = test_server_default()
)
```

## Arguments

- id:

  (`integer(1)`)  
  The dataset id.

- type:

  (`character(1)` or `integer(1)`)  
  Can either be `"classif"` or `"regr"` or an integer indicating the
  task type.

- estimation_procedure:

  (`integer(1)`)  
  The id of the estimation procedure.

- target:

  (`character(1)`)  
  The target variable (if applicable).

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
