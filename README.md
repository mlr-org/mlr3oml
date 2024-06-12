
# mlr3oml

Package website: [release](https://mlr3oml.mlr-org.com/) \|
[dev](https://mlr3oml.mlr-org.com/dev/)

OpenML integration to the [mlr3 ecosystem](https://mlr-org.com/).

[![r-cmd-check](https://github.com/mlr-org/mlr3oml/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/mlr-org/mlr3oml/actions/workflows/r-cmd-check.yml)
[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/mlr3oml)](https://cran.r-project.org/package=mlr3oml)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)

## What is `mlr3oml`?

[OpenML](https://www.openml.org) is an open-source platform that
facilitates the sharing and dissemination of machine learning research
data. All entities on the platform have unique identifiers and
standardized (meta)data that can be accessed via an open-access REST API
or the web interface. `mlr3oml` allows to work with the REST API through
R and integrates [OpenML](https://www.openml.org) with the `mlr3`
ecosystem. Note that some upload options are currently not supported,
use the [OpenML package](https://cran.r-project.org/package=OpenML)
package for this.

As a brief demo, we show how to access an OpenML task, convert it to an
`mlr3::Task` and associated `mlr3::Resampling`, and conduct a simple
resample experiment.

``` r
library(mlr3oml)
library(mlr3)

# Download and print the OpenML task with ID 145953
oml_task = otsk(145953)
oml_task
```

    ## <OMLTask:145953>
    ##  * Type: Supervised Classification
    ##  * Data: kr-vs-kp (id: 3; dim: 3196x37)
    ##  * Target: class
    ##  * Estimation: crossvalidation (id: 1; repeats: 1, folds: 10)

``` r
# Access the OpenML data object on which the task is built
oml_task$data
```

    ## <OMLData:3:kr-vs-kp> (3196x37)
    ##  * Default target: class

``` r
# Convert the OpenML task to an mlr3 task and resampling
task = as_task(oml_task)
resampling = as_resampling(oml_task)

# Conduct a simple resample experiment
rr = resample(task, lrn("classif.rpart"), resampling)
rr$aggregate()
```

    ## classif.ce 
    ##  0.0319181

Besides working with objects with known IDs, data of interest can also
be queried using listing functions. Below, we search for datasets with
10 - 20 features, 100 to 10000 observations and 2 classes.

``` r
odatasets = list_oml_data(
  number_features = c(10, 20),
  number_instances = c(100, 10000),
  number_classes = 2
)

head(odatasets[, c("data_id", "name")])
```

    ##    data_id            name
    ## 1:      13   breast-cancer
    ## 2:      15        breast-w
    ## 3:      29 credit-approval
    ## 4:      49         heart-c
    ## 5:      50     tic-tac-toe
    ## 6:      51         heart-h

To retrieve individual datasets, you can use `odt` and either manually
construct a new `Task` object using `as_task()` or use it `data.table`
format.

``` r
odataset = odt(29)

# Dataset as data.table
str(odataset$data)
```

    ## Classes 'data.table' and 'data.frame':   690 obs. of  16 variables:
    ##  $ A1   : Factor w/ 2 levels "b","a": 1 2 2 1 1 1 1 2 1 1 ...
    ##  $ A2   : num  30.8 58.7 24.5 27.8 20.2 ...
    ##  $ A3   : num  0 4.46 0.5 1.54 5.62 ...
    ##  $ A4   : Factor w/ 4 levels "u","y","l","t": 1 1 1 1 1 1 1 1 2 2 ...
    ##  $ A5   : Factor w/ 3 levels "g","p","gg": 1 1 1 1 1 1 1 1 2 2 ...
    ##  $ A6   : Factor w/ 14 levels "c","d","cc","i",..: 10 9 9 10 10 7 8 3 6 10 ...
    ##  $ A7   : Factor w/ 9 levels "v","h","bb","j",..: 1 2 2 1 1 1 2 1 2 1 ...
    ##  $ A8   : num  1.25 3.04 1.5 3.75 1.71 ...
    ##  $ A9   : Factor w/ 2 levels "t","f": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ A10  : Factor w/ 2 levels "t","f": 1 1 2 1 2 2 2 2 2 2 ...
    ##  $ A11  : int  1 6 0 5 0 0 0 0 0 0 ...
    ##  $ A12  : Factor w/ 2 levels "t","f": 2 2 2 1 2 1 1 2 2 1 ...
    ##  $ A13  : Factor w/ 3 levels "g","p","s": 1 1 1 1 3 1 1 1 1 1 ...
    ##  $ A14  : int  202 43 280 100 120 360 164 80 180 52 ...
    ##  $ A15  : int  0 560 824 3 0 0 31285 1349 314 1442 ...
    ##  $ class: Factor w/ 2 levels "+","-": 1 1 1 1 1 1 1 1 1 1 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
# Creating a new task
otask = as_task(odataset)
otask
```

    ## <TaskClassif:credit-approval> (690 x 16)
    ## * Target: class
    ## * Properties: twoclass
    ## * Features (15):
    ##   - fct (9): A1, A10, A12, A13, A4, A5, A6, A7, A9
    ##   - int (3): A11, A14, A15
    ##   - dbl (3): A2, A3, A8

## Feature Overview

- Datasets, tasks, flows, runs, and collections can be downloaded from
  [OpenML](https://www.openml.org) and are represented as `R6` classes.
- OpenML objects can be easily converted to the corresponding `mlr3`
  counterpart.
- Filtering of OpenML objects can be achieved using listing functions.
- Downloaded objects can be cached by setting the `mlr3oml.cache`
  option.
- Both the `arff` and `parquet` filetype for datasets are supported.
- You can upload datasets, tasks, and collections to OpenML.

## Documentation

- Start by reading the [Large-Scale Benchmarking
  chapter](https://mlr3book.mlr-org.com/chapters/chapter11/large-scale_benchmarking.html)
  from the `mlr3` book.
- The [package website](https://mlr3oml.mlr-org.com/dev/) contains a
  getting started guide.
- The OpenML [API documentation](https://www.openml.org/apis) is also a
  good resource.

## Bugs, Questions, Feedback

*mlr3oml* is a free and open source software project that encourages
participation and feedback. If you have any issues, questions,
suggestions or feedback, please do not hesitate to open an “issue” about
it on the GitHub page!

In case of problems / bugs, it is often helpful if you provide a
“minimum working example” that showcases the behaviour (but don’t worry
about this if the bug is obvious).
