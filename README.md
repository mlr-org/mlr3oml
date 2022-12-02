
# mlr3oml

[![tic](https://github.com/mlr-org/mlr3oml/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3oml/actions?query=workflow%3Atic)
[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/mlr3oml)](https://cran.r-project.org/package=mlr3oml)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)

**mlr3oml** enables downloading [OpenML](https://www.openml.org)
datasets, tasks, flows, runs, and collections and allows to convert them
to the corresponding [mlr3](https://mlr3.mlr-org.com/) object.
Furthermore, you can also obtain the data and the resampling for a given
OpenML task. Caching can be enabled by setting the option
`"mlr3oml.cache"`. Uploading to OpenML is currently not supported, use
the [OpenML package](https://cran.r-project.org/package=OpenML) package
for this.

## Short Demo

``` r
library("mlr3")
library("mlr3oml")

# be less verbose
lgr::get_logger("mlr3oml")$set_threshold("warn")

# retrieve data set as task from OML
tsk("oml", data_id = 31)
```

    ## <TaskClassif:credit-g> (1000 x 21)
    ## * Target: class
    ## * Properties: twoclass
    ## * Features (20):
    ##   - fct (13): checking_status, credit_history, employment,
    ##     foreign_worker, housing, job, other_parties, other_payment_plans,
    ##     own_telephone, personal_status, property_magnitude, purpose,
    ##     savings_status
    ##   - int (7): age, credit_amount, duration, existing_credits,
    ##     installment_commitment, num_dependents, residence_since

``` r
# retrieve a regular task from OML
tsk("oml", task_id = 59)
```

    ## <TaskClassif:iris> (150 x 5)
    ## * Target: class
    ## * Properties: multiclass
    ## * Features (4):
    ##   - dbl (4): petallength, petalwidth, sepallength, sepalwidth

``` r
# retrieve resampling from OML
rsmp("oml", task_id = 59)
```

    ## <ResamplingCustom>: Custom Splits
    ## * Iterations: 10
    ## * Instantiated: TRUE
    ## * Parameters: list()

``` r
# R6 class for data sets
odata = OMLData$new(61) # or
odata = odt(61)

odata$name
```

    ## [1] "iris"

``` r
odata$nrow
```

    ## [1] 150

``` r
odata$ncol
```

    ## [1] 5

``` r
head(odata$data)
```

    ##    sepallength sepalwidth petallength petalwidth       class
    ## 1:         5.1        3.5         1.4        0.2 Iris-setosa
    ## 2:         4.9        3.0         1.4        0.2 Iris-setosa
    ## 3:         4.7        3.2         1.3        0.2 Iris-setosa
    ## 4:         4.6        3.1         1.5        0.2 Iris-setosa
    ## 5:         5.0        3.6         1.4        0.2 Iris-setosa
    ## 6:         5.4        3.9         1.7        0.4 Iris-setosa

``` r
backend = as_data_backend(odata)
backend
```

    ## <DataBackendDataTable> (150x6)
    ##  sepallength sepalwidth petallength petalwidth       class ..row_id
    ##          5.1        3.5         1.4        0.2 Iris-setosa        1
    ##          4.9        3.0         1.4        0.2 Iris-setosa        2
    ##          4.7        3.2         1.3        0.2 Iris-setosa        3
    ##          4.6        3.1         1.5        0.2 Iris-setosa        4
    ##          5.0        3.6         1.4        0.2 Iris-setosa        5
    ##          5.4        3.9         1.7        0.4 Iris-setosa        6
    ## [...] (144 rows omitted)

``` r
# list oml data sets with 5 features and 50 - 200 instances
tab = list_oml_data(number_features = 5, number_instances = c(50, 200))
head(tab[, .(data_id, name)])
```

    ##    data_id               name
    ## 1:      61               iris
    ## 2:     199           fruitfly
    ## 3:     214           baskball
    ## 4:     329         hayes-roth
    ## 5:     346               aids
    ## 6:     668 witmer_census_1980

``` r
# R6 class for tasks
otask = OMLTask$new(31) # or
otask = otsk(31)

task = as_task(otask)
task
```

    ## <TaskClassif:credit-g> (1000 x 21)
    ## * Target: class
    ## * Properties: twoclass
    ## * Features (20):
    ##   - fct (13): checking_status, credit_history, employment,
    ##     foreign_worker, housing, job, other_parties, other_payment_plans,
    ##     own_telephone, personal_status, property_magnitude, purpose,
    ##     savings_status
    ##   - int (7): age, credit_amount, duration, existing_credits,
    ##     installment_commitment, num_dependents, residence_since

``` r
resampling = as_resampling(otask)
resampling
```

    ## <ResamplingCustom>: Custom Splits
    ## * Iterations: 10
    ## * Instantiated: TRUE
    ## * Parameters: list()

``` r
otask$data
```

    ## <OMLData:31:credit-g> (1000x21)
    ##  * Default target: class

``` r
otask$name
```

    ## [1] "Task 31: credit-g (Supervised Classification)"

``` r
otask$nrow
```

    ## [1] 1000

``` r
otask$ncol
```

    ## [1] 21

``` r
otask$task
```

    ## NULL

``` r
otask$resampling
```

    ## NULL

``` r
# list first 10 oml tasks
tab = list_oml_tasks(limit = 10)
tab[, .(task_id, data_id, name)]
```

    ##     task_id data_id            name
    ##  1:       2       2          anneal
    ##  2:       3       3        kr-vs-kp
    ##  3:       4       4           labor
    ##  4:       5       5      arrhythmia
    ##  5:       6       6          letter
    ##  6:       7       7       audiology
    ##  7:       8       8 liver-disorders
    ##  8:       9       9           autos
    ##  9:      10      10           lymph
    ## 10:      11      11   balance-scale

``` r
# R6 class for flows
oflow = OMLFlow$new(100) # or
oflow = oflw(100)

oflow$dependencies
```

    ## [1] "Weka_3.7.5"

``` r
oflow$parameter
```

    ##     name data_type default_value
    ##  1:    A      flag              
    ##  2:    B      flag              
    ##  3:    C    option          0.25
    ##  4:    J      flag              
    ##  5:    L      flag              
    ##  6:    M    option             2
    ##  7:    N    option              
    ##  8:    O      flag              
    ##  9:    Q    option              
    ## 10:    R      flag              
    ## 11:    S      flag              
    ## 12:    U      flag

``` r
# non-executable pseudo learner
learner = as_learner(oflow, "regr")
learner
```

    ## <LearnerRegrOML100:oml.100>
    ## * Model: -
    ## * Parameters: list()
    ## * Packages: mlr3
    ## * Predict Types:  [response]
    ## * Feature Types: -
    ## * Properties: -

``` r
# R6Class for run

orun = OMLRun$new(538858) # o
orun = orn(538858)

orun$data
```

    ## <OMLData:952:prnn_fglass> (214x10)
    ##  * Default target: type

``` r
orun$task
```

    ## <OMLTask:3815>
    ##  * Type: Supervised Classification
    ##  * Data: prnn_fglass (id: 952; dim: 214x10)
    ##  * Target: type
    ##  * Estimation: crossvalidation (id: 1; repeats: 1, folds: 10)

``` r
orun$flow
```

    ## <OMLFlow:3364>
    ##  * Name: classif.boosting
    ##  * Dependencies: mlr_2.8, adabag_4.1, rpart_4.1.10

``` r
head(orun$prediction)
```

    ##    repeat. fold row_id             prediction              truth
    ## 1:       0    0     33     window_float_glass window_float_glass
    ## 2:       0    0     52     window_float_glass window_float_glass
    ## 3:       0    0     67     window_float_glass window_float_glass
    ## 4:       0    0     49     window_float_glass window_float_glass
    ## 5:       0    0     41     window_float_glass window_float_glass
    ## 6:       0    0      5 window_non-float_glass window_float_glass
    ##    confidence.containers confidence.tableware confidence.vehicle_glass
    ## 1:                     0                    0                        0
    ## 2:                     0                    0                        0
    ## 3:                     0                    0                        0
    ## 4:                     0                    0                        0
    ## 5:                     0                    0                        0
    ## 6:                     0                    0                        0
    ##    confidence.vehicle_headlamp_glass confidence.window_float_glass
    ## 1:                                 0                             1
    ## 2:                                 0                             1
    ## 3:                                 0                             1
    ## 4:                                 0                             1
    ## 5:                                 0                             1
    ## 6:                                 0                             0
    ##    confidence.window_non.float_glass
    ## 1:                                 0
    ## 2:                                 0
    ## 3:                                 0
    ## 4:                                 0
    ## 5:                                 0
    ## 6:                                 1

``` r
rr = as_resample_result(orun)
rr
```

    ## <ResampleResult> of 10 iterations
    ## * Task: prnn_fglass
    ## * Learner: oml.3364
    ## * Warnings: 0 in 0 iterations
    ## * Errors: 0 in 0 iterations

``` r
# R6 class for collection

ocol = OMLCollection$new(232) # or
ocol = ocl(232)

ocol$tasks
```

    ##    id          task     data                 task_type target  nrow ncol
    ## 1:  3 <OMLTask[25]> kr-vs-kp Supervised Classification  class  3196   37
    ## 2:  6 <OMLTask[25]>   letter Supervised Classification  class 20000   17
    ##    missing numeric symbolic binary     task_splits
    ## 1:       0       0       37     35 crossvalidation
    ## 2:       0      16        1      0 crossvalidation
