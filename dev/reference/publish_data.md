# Upload data to OpenML

Upload a dataset to OpenML. This can also be achieved through the
[website](https://www.openml.org).

## Usage

``` r
publish_data(
  data,
  name,
  desc,
  license = NULL,
  default_target = NULL,
  citation = NULL,
  row_identifier = NULL,
  ignore_attribute = NULL,
  original_data_url = NULL,
  paper_url = NULL,
  test_server = test_server_default(),
  api_key = NULL
)
```

## Arguments

- data:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html))  
  The data to upload.

- name:

  (`character(1)`)  
  The name of the dataset.

- desc:

  (`character(1)`)  
  The description of the dataset.

- license:

  (`character(1)`)  
  The license of the dataset

- default_target:

  (`character(1)`)  
  The default target variable.

- citation:

  (`character(1)`)  
  How to cite the dataset.

- row_identifier:

  (`character(1)`)  
  Whether any of the columns is a row identifier.

- ignore_attribute:

  (`character(1)`)  
  Which columns to ignore during modeling.

- original_data_url:

  (character(1))  
  The URL of the original data set.

- paper_url:

  (`character(1)`)  
  The URL of the paper describing the data set.

- test_server:

  (`character(1)`)  
  Whether to use the OpenML test server or public server. Defaults to
  value of option `"mlr3oml.test_server"`, or `FALSE` if not set.

- api_key:

  (`character(1)`) The API key to perform the action, if left NULL it
  first tries the "mlr3oml.api_key" R option and then the environment
  variable `OPENMLAPIKEY`.

  In case `test_server` is TRUE (only relevant for developers) the test
  server API key is used, i.e. first the option "mlr3oml.test_api_key"
  and then the environment variable `TESTOPENMLAPIKEY`.
