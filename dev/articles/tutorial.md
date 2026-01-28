# Getting Started

## mlr3oml

This tutorial will give you a quick overview of the main features of
`mlr3oml`. If you are not familiar with
[OpenML](https://docs.openml.org/), we recommend to read [its
documentation](https://docs.openml.org) first, as we will not explain
the OpenML concepts in detail here. Further coverage of some selected
`mlr3oml` features can be found in the [Large-Scale Benchmarking
chapter](https://mlr3book.mlr-org.com/chapters/chapter11/large-scale_benchmarking.html)
of the `mlr3book`. Note that `mlr3oml` currently only supports
downloading objects from OpenML. Uploading can for example be achieved
through the [website](https://www.openml.org).

First, we will briefly cover the different OpenML objects that can be
downloaded using `mlr3oml`. Then we will show how to find objects with
certain properties on OpenML. Finally, we will quickly discuss some
further aspects of `mlr3oml`, which includes caching, file formats,
laziness, the logger, and the API key.

### OpenML Objects

`mlr3oml` supports five different types of OpenML objects that are
listed below. All objects can be converted to their corresponding `mlr3`
pandeaunt.

- [`OMLData`](https://mlr3oml.mlr-org.com/reference/oml_data.html)
  represents an [OpenML
  dataset](https://www.openml.org/search?type=data&sort=runs&status=active).
  These are (usually tabular) sets with additional meta-data, which
  includes e.g. a description of the dataset or a license. The most
  similar `mlr3` class is the
  [`mlr3::DataBackend`](https://mlr3.mlr-org.com/reference/DataBackend.html).
- [`OMLTask`](https://mlr3oml.mlr-org.com/reference/oml_task.html)
  represents an [OpenML
  task](https://www.openml.org/search?type=task&sort=runs). This is a
  concrete problem speficiation on top of an OpenML dataset. While being
  similar to
  [`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html) objects,
  a major difference is that the OpenML task also contains the
  resampling splits and can therefore also be converted to an
  [`mlr3::Resampling`](https://mlr3.mlr-org.com/reference/Resampling.html).
- [`OMLFlow`](https://mlr3oml.mlr-org.com/reference/oml_flow.html)
  represents an [OpenML
  flow](https://www.openml.org/search?type=flow&sort=runs). This is a
  reusable and executable representation of a machine learning pipeline
  or workflow. The closest `mlr3` class is the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html).
- [`OMLRun`](https://mlr3oml.mlr-org.com/reference/oml_run.html)
  represents an [OpenML
  run](https://www.openml.org/search?type=run&sort=date). An OpenML run
  refers to the execution of a specific machine learning flow on a
  particular task, recording all relevant information such as
  hyperparameters, performance metrics, and intermediate results. This
  is similar to an
  [`mlr3::ResampleResult`](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  object.
- [`OMLCollection`](https://mlr3oml.mlr-org.com/reference/oml_collection.html)
  represents an OpenML collection, which can either be a [run
  collection](https://www.openml.org/search?type=study&study_type=run)
  or a [task
  collection](https://www.openml.org/search?type=study&study_type=task).
  These are container objects that allow to bundle tasks (resulting in
  benchmarking suites) or runs (which can be used to represent benchmark
  experiments). There is no `mlr3` pendant for the former (other than a
  list of tasks), while the latter would correspond to an
  [`mlr3::BenchmarkResult`](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).

Each object on OpenML has a unique identifier, by which it can be
retrieved. We will now briefly show how to access and work with these
objects.

#### Data

Below, we retrieve the dataset with ID 31, which is the credit-g data
and can be viewed online
[here](https://www.openml.org/search?type=data&sort=runs&status=active&id=31).
Like in other `mlr3` packages, sugar functions exist for the
construction of `R6` classes. We always show both ways to construct the
objects.

``` r
library(mlr3oml)
library(mlr3)

oml_data = OMLData$new(id = 31)
# is the same as
oml_data = odt(id = 31)
oml_data
```

    ## <OMLData:31:credit-g> (1000x21)
    ##  * Default target: class

The full meta data can be accessed using the `$desc` field. Some fields,
such as the number of rows and columns can be accessed directly.

``` r
# the usage licence
oml_data$desc$licence
```

    ## [1] "Public"

``` r
# the data dimension
c(n_rows = oml_data$nrow, n_cols = oml_data$ncol)
```

    ## n_rows n_cols 
    ##   1000     21

Information about the features can be accessed through the `$features`
field. This includes information regarding the data types, missing
values, whether they should be ignored for learning or whether they are
the row identifier.

``` r
head(oml_data$features)
```

    ## Key: <index>
    ##    index            name data_type
    ##    <int>          <char>    <fctr>
    ## 1:     0 checking_status   nominal
    ## 2:     1        duration   numeric
    ## 3:     2  credit_history   nominal
    ## 4:     3         purpose   nominal
    ## 5:     4   credit_amount   numeric
    ## 6:     5  savings_status   nominal
    ##                                                                                   nominal_value
    ##                                                                                          <list>
    ## 1:                                                                0<=X<200,<0,>=200,no checking
    ## 2:                                                                                       [NULL]
    ## 3: all paid,critical/other existing credit,delayed previously,existing paid,no credits/all paid
    ## 4:              business,domestic appliance,education,furniture/equipment,new car,other,...[11]
    ## 5:                                                                                       [NULL]
    ## 6:                                          100<=X<500,500<=X<1000,<100,>=1000,no known savings
    ##    is_target is_ignore is_row_identifier number_of_missing_values
    ##       <lgcl>    <lgcl>            <lgcl>                    <int>
    ## 1:     FALSE     FALSE             FALSE                        0
    ## 2:     FALSE     FALSE             FALSE                        0
    ## 3:     FALSE     FALSE             FALSE                        0
    ## 4:     FALSE     FALSE             FALSE                        0
    ## 5:     FALSE     FALSE             FALSE                        0
    ## 6:     FALSE     FALSE             FALSE                        0

The data itself can be accessed using the `$data` field. We only show a
subset of the data here for readability.

``` r
oml_data$data[1:5, 1:3]
```

    ##    checking_status duration                 credit_history
    ##             <fctr>    <int>                         <fctr>
    ## 1:              <0        6 critical/other existing credit
    ## 2:        0<=X<200       48                  existing paid
    ## 3:     no checking       12 critical/other existing credit
    ## 4:              <0       42                  existing paid
    ## 5:              <0       24             delayed previously

We can convert this object to an
[`mlr3::DataBackend`](https://mlr3.mlr-org.com/reference/DataBackend.html)
using the
[`as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.html)
function.

``` r
backend = as_data_backend(oml_data)
backend
```

    ## 
    ## ── <DataBackendDataTable> (1000x22) ────────────────────────────────────────────
    ##  checking_status duration                 credit_history             purpose
    ##           <fctr>    <int>                         <fctr>              <fctr>
    ##               <0        6 critical/other existing credit            radio/tv
    ##         0<=X<200       48                  existing paid            radio/tv
    ##      no checking       12 critical/other existing credit           education
    ##               <0       42                  existing paid furniture/equipment
    ##               <0       24             delayed previously             new car
    ##      no checking       36                  existing paid           education
    ##  credit_amount   savings_status employment installment_commitment
    ##          <int>           <fctr>     <fctr>                  <int>
    ##           1169 no known savings        >=7                      4
    ##           5951             <100     1<=X<4                      2
    ##           2096             <100     4<=X<7                      2
    ##           7882             <100     4<=X<7                      2
    ##           4870             <100     1<=X<4                      3
    ##           9055 no known savings     1<=X<4                      2
    ##     personal_status other_parties residence_since property_magnitude   age
    ##              <fctr>        <fctr>           <int>             <fctr> <int>
    ##         male single          none               4        real estate    67
    ##  female div/dep/mar          none               2        real estate    22
    ##         male single          none               3        real estate    49
    ##         male single     guarantor               4     life insurance    45
    ##         male single          none               4  no known property    53
    ##         male single          none               4  no known property    35
    ##  other_payment_plans  housing existing_credits                job
    ##               <fctr>   <fctr>            <int>             <fctr>
    ##                 none      own                2            skilled
    ##                 none      own                1            skilled
    ##                 none      own                1 unskilled resident
    ##                 none for free                1            skilled
    ##                 none for free                2            skilled
    ##                 none for free                1 unskilled resident
    ##  num_dependents own_telephone foreign_worker  class ..row_id
    ##           <int>        <fctr>         <fctr> <fctr>    <int>
    ##               1           yes            yes   good        1
    ##               1          none            yes    bad        2
    ##               2          none            yes   good        3
    ##               2          none            yes   good        4
    ##               2          none            yes    bad        5
    ##               2           yes            yes   good        6
    ## [...] (994 rows omitted)

Because this specific dataset has a default target in its meta data, we
can also directly convert it to an
[`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html).

``` r
# the default target
oml_data$target_names
```

    ## [1] "class"

``` r
# convert the OpenML data to an mlr3 task
task = as_task(oml_data)
```

With either the `backend` or the `task`, we are now in `mlr3` land
again, and can work with the objects as usual:

``` r
rr = resample(task, lrn("classif.rpart"), rsmp("holdout"))
```

#### Task

Below, we access the OpenML task with [ID
261](https://www.openml.org/search?type=task&id=261&sort=runs), which is
a classification task built on top of the credit-g data used above. Its
associated resampling is a 2/3 holdout split.

``` r
oml_task = OMLTask$new(id = 261)
# is the same as
oml_task = otsk(id = 261)
oml_task
```

    ## <OMLTask:261>
    ##  * Type: Supervised Classification
    ##  * Data: credit-g (id: 31; dim: 1000x21)
    ##  * Target: class
    ##  * Estimation: holdout (id: 6; test size: 33%)

The OpenML data that the task is built on top of can be accessed through
`$data`.

``` r
oml_task$data
```

    ## <OMLData:31:credit-g> (1000x21)
    ##  * Default target: class

We can also access the target columns and the features. Note that this
target *can* differ from the default target shown in the previous
section.

``` r
oml_task$target_names
```

    ## [1] "class"

``` r
oml_task$feature_names
```

    ##  [1] "checking_status"        "duration"               "credit_history"        
    ##  [4] "purpose"                "credit_amount"          "savings_status"        
    ##  [7] "employment"             "installment_commitment" "personal_status"       
    ## [10] "other_parties"          "residence_since"        "property_magnitude"    
    ## [13] "age"                    "other_payment_plans"    "housing"               
    ## [16] "existing_credits"       "job"                    "num_dependents"        
    ## [19] "own_telephone"          "foreign_worker"

The associated resampling splits can be accessed using `$task_splits`.

``` r
oml_task$task_splits
```

    ##         type rowid repeat.  fold
    ##       <fctr> <int>   <int> <int>
    ##    1:   TEST   490       0     0
    ##    2:   TEST   539       0     0
    ##    3:   TEST   694       0     0
    ##    4:   TEST   646       0     0
    ##    5:   TEST   150       0     0
    ##   ---                           
    ##  996:  TRAIN   875       0     0
    ##  997:  TRAIN   549       0     0
    ##  998:  TRAIN   195       0     0
    ##  999:  TRAIN   241       0     0
    ## 1000:  TRAIN   360       0     0

The conversion to an
[`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html) is possible
using the [`as_task()`](https://mlr3.mlr-org.com/reference/as_task.html)
converter.

``` r
# Convert OpenML task to mlr3 task
task = as_task(oml_task)
task
```

    ## 
    ## ── <TaskClassif> (1000x21) ─────────────────────────────────────────────────────
    ## • Target: class
    ## • Target classes: good (positive class, 70%), bad (30%)
    ## • Properties: twoclass
    ## • Features (20):
    ##   • fct (13): checking_status, credit_history, employment, foreign_worker,
    ##   housing, job, other_parties, other_payment_plans, own_telephone,
    ##   personal_status, property_magnitude, purpose, savings_status
    ##   • int (7): age, credit_amount, duration, existing_credits,
    ##   installment_commitment, num_dependents, residence_since

The associated resampling can be obtained by calling
[`as_resampling()`](https://mlr3.mlr-org.com/reference/as_resampling.html).

``` r
# Convert OpenML task to mlr3 resampling
resampling = as_resampling(oml_task)
resampling
```

    ## 
    ## ── <ResamplingCustom> : Custom Splits ──────────────────────────────────────────
    ## • Iterations: 1
    ## • Instantiated: TRUE
    ## • Parameters: list()

To simplify this, there exist `"oml"` tasks and resamplings:

``` r
tsk("oml", task_id = 261)
```

    ## 
    ## ── <TaskClassif> (1000x21) ─────────────────────────────────────────────────────
    ## • Target: class
    ## • Target classes: good (positive class, 70%), bad (30%)
    ## • Properties: twoclass
    ## • Features (20):
    ##   • fct (13): checking_status, credit_history, employment, foreign_worker,
    ##   housing, job, other_parties, other_payment_plans, own_telephone,
    ##   personal_status, property_magnitude, purpose, savings_status
    ##   • int (7): age, credit_amount, duration, existing_credits,
    ##   installment_commitment, num_dependents, residence_since

``` r
rsmp("oml", task_id = 261)
```

    ## 
    ## ── <ResamplingCustom> : Custom Splits ──────────────────────────────────────────
    ## • Iterations: 1
    ## • Instantiated: TRUE
    ## • Parameters: list()

#### Flows and Runs

We can access the flow with [ID
1068](https://www.openml.org/search?type=flow&sort=runs&id=1068) as
shown below:

``` r
flow = OMLFlow$new(id = 1068)
# is the same as
flow = oflw(id = 1068)
flow
```

    ## <OMLFlow:1068>
    ##  * Name: weka.J48
    ##  * Dependencies: Weka_3.7.12

Flows themself only become interesting once they are applied to a task,
the result of which is an OpenML run.

For example, the run with [ID
169061](https://www.openml.org/search?type=run&sort=date&id=169061)
contains the result of applying the above flow to task 261 from above:

``` r
run = OMLRun$new(id = 169061)
# is the same as
run = orn(id = 169061)
run
```

    ## <OMLRun:169061>
    ##  * Task: credit-g (id: 261)
    ##  * Flow: weka.J48 (id: 1068)
    ##  * Estimation: holdout (id: 6; test size: 33%)

``` r
# the corresponding flow and and task can be accessed directly
run$flow
```

    ## <OMLFlow:1068>
    ##  * Name: weka.J48
    ##  * Dependencies: Weka_3.7.12

``` r
run$task
```

    ## <OMLTask:261>
    ##  * Type: Supervised Classification
    ##  * Data: credit-g (id: 31; dim: 1000x21)
    ##  * Target: class
    ##  * Estimation: holdout (id: 6; test size: 33%)

The result of this experiment are the predictions, as well as the
evaluation of these predictions.

``` r
head(run$prediction)
```

    ##    repeat.  fold row_id confidence.good confidence.bad prediction correct
    ##      <int> <int>  <int>           <num>          <num>     <fctr>  <fctr>
    ## 1:       0     0    490        0.920000       0.080000       good    good
    ## 2:       0     0    539        0.885714       0.114286       good    good
    ## 3:       0     0    694        0.920000       0.080000       good    good
    ## 4:       0     0    646        0.727273       0.272727       good    good
    ## 5:       0     0    150        0.727273       0.272727       good    good
    ## 6:       0     0    970        0.166667       0.833333        bad    good

``` r
head(run$evaluation)
```

    ##                             name     value        array_data repeat  fold
    ##                           <char>     <num>            <list> <char> <int>
    ## 1:          area_under_roc_curve  0.657856 0.657856,0.657856   <NA>    NA
    ## 2:                  average_cost  0.000000                NA   <NA>    NA
    ## 3:                     f_measure  0.705442 0.795745,0.494737   <NA>    NA
    ## 4:                         kappa  0.290990                NA   <NA>    NA
    ## 5: kb_relative_information_score 53.759911                NA   <NA>    NA
    ## 6:           mean_absolute_error  0.342722                NA   <NA>    NA

OpenML runs can be converted to
[`mlr3::ResampleResult`](https://mlr3.mlr-org.com/reference/ResampleResult.html)s
using the
[`as_resample_result()`](https://mlr3.mlr-org.com/reference/as_resample_result.html)
function.

``` r
rr = as_resample_result(run)
rr
```

    ## 
    ## ── <ResampleResult> with 2 resampling iterations ───────────────────────────────
    ##   task_id learner_id resampling_id iteration     prediction_test warnings
    ##  credit-g   oml.1068        custom         1 <PredictionClassif>        0
    ##  credit-g   oml.1068        custom         1 <PredictionClassif>        0
    ##  errors
    ##       0
    ##       0

#### Collection

Below, we access the
[OpenML-CC18](https://www.openml.org/search?type=study&study_type=task&id=99),
which is a curated collection of 72 OpenML classification tasks, i.e. a
task collection.

``` r
cc18 = OMLCollection$new(id = 99)
# is the same as
cc18 = ocl(id = 99)
```

The ids of the tasks and datasets contained in this benchmarking suite
can be accessed through the fields `$task_ids` and `$data_ids`
respectively.

``` r
# the first 10 task ids
cc18$task_ids[1:10]
```

    ##  [1]  3  6 11 12 14 15 16 18 22 23

``` r
# the first 10 data ids
cc18$data_ids[1:10]
```

    ##  [1]  3  6 11 12 14 15 16 18 22 23

We can, e.g., create an
[`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html) from the
first of these tasks as follows:

``` r
task1 = tsk("oml", task_id = cc18$task_ids[1])
task1
```

    ## 
    ## ── <TaskClassif> (3196x37) ─────────────────────────────────────────────────────
    ## • Target: class
    ## • Target classes: won (positive class, 52%), nowin (48%)
    ## • Properties: twoclass
    ## • Features (36):
    ##   • fct (36): bkblk, bknwy, bkon8, bkona, bkspr, bkxbq, bkxcr, bkxwp, blxwp,
    ##   bxqsq, cntxt, dsopp, dwipd, hdchk, katri, mulch, qxmsq, r2ar8, reskd, reskr,
    ##   rimmx, rkxwp, rxmsq, simpl, skach, skewr, skrxp, spcop, stlmt, thrsk, wkcti,
    ##   wkna8, wknck, wkovl, wkpos, wtoeg

### Listing

While we showed how to work with objects with known IDs, another
important question is how to find the relevant IDs. This can either be
achieved through the OpenML [website](https://www.openml.org) or through
the REST API. To access the latter, `mlr3oml` provides the following
[listing
functions](https://mlr3oml.mlr-org.com/reference/list_oml.html):

- [`list_oml_data()`](https://mlr3oml.mlr-org.com/dev/reference/list_oml.md) -
  Find datasets
- [`list_oml_tasks()`](https://mlr3oml.mlr-org.com/dev/reference/list_oml.md) -
  Find tasks
- [`list_oml_flows()`](https://mlr3oml.mlr-org.com/dev/reference/list_oml.md) -
  Find flows
- [`list_oml_runs()`](https://mlr3oml.mlr-org.com/dev/reference/list_oml.md) -
  Find runs

As an example, we will only show the usage of the first function, but
all others work analogously.

We can, for example, subset the datasets contained in the CC-18 even
further. Below, we only select datasets that have between 0 and 10
features.

``` r
cc18_filtered = list_oml_tasks(
  data_id = cc18$data_ids,
  number_features = c(0, 10)
)
cc18_filtered[1:5, c("task_id", "name")]
```

    ##    task_id                name
    ##      <int>              <char>
    ## 1:      11       balance-scale
    ## 2:      15            breast-w
    ## 3:      18 mfeat-morphological
    ## 4:      23                 cmc
    ## 5:      37            diabetes

Note that not all possible property specifications can be directly
queried on OpenML. As the resulting tables are `data.table`s containing
information about the datasets, they can be further filtered using the
usual `data.table` syntax.

### Uploading

You can currently upload datasets to OpenML or create tasks and
collections using the functions:

- [`publish_data()`](https://mlr3oml.mlr-org.com/dev/reference/publish_data.md)
  to upload a dataset,
- [`publish_task()`](https://mlr3oml.mlr-org.com/dev/reference/publish_task.md)
  to create a task, and
- [`publish_collection()`](https://mlr3oml.mlr-org.com/dev/reference/publish_collection.md)
  to create a collection.

For this, you need an API key.

### API Key

All download operations supported by this package work without an API
key, but you might get rate limited without an API key. For uploading to
OpenML, you need an API key. The API key can be specified via the option
`mlr3oml.api_key` or the environment variable `OPENMLAPIKEY` (where the
former has precedence over the latter). To obtain an API key, you must
[create an account on OpenML](https://www.openml.org/auth/sign-up).

### Further Aspects

#### Logging

`mlr3oml` has its own logger, which can be accessed using
`lgr::get_logger("mlr3oml")`. For more information about logging in
general (such as chaning the logging threshold), we refer to the
corresponding
[section](https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html#sec-logging)
in the `mlr3book`.

#### Laziness

All objects accessed through `mlr3oml` must be downloaded from the
OpenML server. This is done lazily, which means that data is only
downloaded when it is actually accessed. To show this, we change the
logging level, which was previously set to `"warn"` (to keep the output
clean), to `"info"`.

``` r
logger = lgr::get_logger("mlr3oml")
logger$set_threshold("info")

oml_data = odt(31)
# to print the object, some meta data must be downloaded
oml_data
```

    ## <OMLData:31:credit-g> (1000x21)
    ##  * Default target: class

To download all information associated with an object, the `$download()`
method can be called. This can be useful to ensure that all information
is available offline. In this case, only the actual underlying data is
downloaded, as everything else was already implicityly accessed above.

``` r
oml_data$download()
```

#### Caching

Caching of OpenML objects can be enabled by setting the `mlr3oml.cache`
option to either `TRUE` or `FALSE` (default), or to a specific folder to
be used as the cache directory. When this is enabled, many OpenML
objects are also available offline. Note that OpenML collections are not
cached, as IDs can be added or removed.

``` r
# Set a temporary directy as the cache folder
cache_dir = tempfile()
options(mlr3oml.cache = cache_dir)

odata = odt(31)
odata
```

    ## <OMLData:31:credit-g> (1000x21)
    ##  * Default target: class

``` r
# When accessing the data again, nothing has to be downloaded
# because the information is loaded from the cache
odata_again = odt(31)
odata_again
```

    ## <OMLData:31:credit-g> (1000x21)
    ##  * Default target: class

``` r
# set back the logger
logger$set_threshold("warn")
```

#### Data Types

The datasets on OpenML are available in two different formats, namely
[*arff*](https://www.cs.waikato.ac.nz/ml/weka/arff.html) and
[*parquet*](https://parquet.apache.org/). The former is used by default,
but this default can be changed by setting the `mlr3oml.parquet` option
to `TRUE`. It is also possible to specify this during construction of a
specific OpenML object.

While the parquet format is more efficient, arff was the original format
and might therefore considered to be more stable. Moreover, minor
differences for the two different formats for a given data ID can occur,
e.g. regarding the data type.

When converting an `OMLData` object to an
[`mlr3::DataBackend`](https://mlr3.mlr-org.com/reference/DataBackend.html)
using the parquet file type, the resulting backend is an
[`mlr3db::DataBackendDuckDB`](https://mlr3db.mlr-org.com/reference/DataBackendDuckDB.html)
object. For the arff file format, the resulting backend is a
[`mlr3::DataBackendDataTable`](https://mlr3.mlr-org.com/reference/DataBackendDataTable.html).

``` r
library(mlr3db)
odata_pq = odt(id = 31, parquet = TRUE)
backend_pq = as_data_backend(odata_pq)
class(backend_pq)
```

    ## [1] "DataBackendDuckDB" "DataBackend"       "R6"

``` r
# compare with arff
odata_arff = odt(id = 31, parquet = FALSE)
backend_arff = as_data_backend(odata_arff)
class(backend_arff)
```

    ## [1] "DataBackendDataTable" "DataBackend"          "R6"

For more information on data backends, see the corresponding
[section](https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html#sec-backends)
in the `mlr3book`.
