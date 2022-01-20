# TODO File for mlr3oml

Benchmark:
- Each dataset should appear only as one task

- [ ] implement list_oml_estimation_procedures
- [ ] implement list_oml_benchmark_suites
- [ ] Extract task and resampling from Task dictionary
- [x] Better efficiency when uploading models (learner once + states)
- [x] Require user confirmation when uploading (only do this once on every publish
    call, i.e. don't repeat on the recursive calls)
- [ ] Provide possibility to delete everything once the upload failed (when e.g.
    uploading a resample or benchmark result)
- [ ] Add trycatch interrupt, (https://community.rstudio.com/t/how-to-catch-the-keyboard-interruption-in-r/7336)
- [x] Implement cleanup in tests (i.e. deleting after upload)
- [x] When stuff already exists during upload, don't throw an error but simply return the id
  --> do we even need the manual checking for flow? probably not
- [ ] Add evaluations when publishing resample result
- [ ] mlr3oml.cache should also accept a directory
- [ ] Check that caching is working properly

Only check box when it is properly tested

- [ ] Download
  - [x] Flow
  - [ ] Run
  - [ ] Collection

- [ ] Publish
  - [ ] Learner
  - [ ] Run
  - [ ] BenchmarkSuite

- [ ] Convert
  - [ ] Flow: [Flow --> Learner]
  - [ ] Run [Run --> ResamplingResult]
  - [ ] Collection [Collection --> list)
  - [ ] Resampling [Resampling --> Resampling]

Other tasks:
- [ ] Verify existing functions
- [ ] Better console output when downloading collections.
- [ ] Add tags to flows (mlr3), must be done with seperate API Call
- [ ] Use stringi instead of paste
- [ ] Extend it to survival tasks and maybe also cluster


Optional:
- When doing multiuploads that fails, provide the possibility to delete those that were
uploaded. Most important for uploading benchmark results.



Into README:
- To test, setup mlr3oml.test_api_key
## Ziele:
- Run:
  - [ ] download
  - [ ] upload
  Um Run zu uploaden muss man auch Datensätze unf
- Flow:
  - [ ] download
  - [ ] upload

## Flow
- GraphLearner --> wie genau
- AutoTuner


## Design
- Unterscheiden zwischen runs mit dem tag "mlr3" und runs ohne den Tag
  --> besserer Support für runs mit dem tag mlr3.
- Tuning:
  is handled by Autotuner, fSelector
  --> Will be a own flow with parameters
- Pipelines:
  The individual flw's are uploaded seperately and the
  main experiment is the flow GraphLearner that simply stores the graph.structure + separate
  flow ids's
- Reprodubile seed: egal
- Zum Testen: CC18 benchmark
- Für Parquet Files: DuckDB
- upload: S3 Methode, zusätzliche measures als Argument
- Reproducibility? --> renv + callr (was ist das)
- Lerner außerbalb OpenML --> Nicht berücksichtigen


## Schierigkeiten
- [ ] Parameter die selbst wieder Funktionen sind

## Aufgaben
- Existierende Funktionalität checken und Tests entwickeln (z.B.
  auf Typen checken etc.)
- Objekt für OMLRun  --> Schauen wie es bei OMLTask gemacht wurde
- Task erstellen wenn vom user gewollt

## Fragen an OpenML
 - API muss laufen


Questions:
1. Which features from OML are to be included
1. How to deal with Benchmarks / Collections?
1. Difference between a collection and a benchmark suite?
1. Connect mlr3measures with Evaluation Measures?
1. What is a Setup --> Flow + Hyperpa ram config.
1. Is resampling part of a benchmark / collection?
1. It should be possible to download a task without
loading mlr3 for "tsk("oml", task_id  = n)" ?
1. Connect measures with mlr3measures


## Main Features

### List

- Bei collections und benchmarks auf feste  API warten
- Existierende Funktionen: Ausprobieren + Tests


| Feature: List      | OpenML | mlr3oml | Desired |
| ------------------ | ------ | ------- | ------- |
| DataSets           | 1      | 1       | 1       |
| Tasks              | 1      | 1       | 1       |
| Flows              | 1      | 1       | 0       |
| Runs               | 1      | 1       | 1       |
| Setups             | 1      | 1       | 1       |
| RunEvaluations     | 1      | 1       | 1       |
| EvaluationMeasures | 1      | 1       | 1       |
| Benchmarks         | 0      | 0       | 1       |
| Collections        | 0      | 0       | 1       |
| TaskTypes          | 1      | 0       | ?       |

### Download
Questions:
- Data format for OpenML Benchmark / Collection
- Predictions: Einfach als run$prediction()

| Feature: Download | OpenML | mlr3oml | Desired |
| ----------------- | ------ | ------- | ------- |
| DataSets          | 1      | 1       | 1       |
| Tasks             | 1      | 1       | 1       |
| Flows             | 1      | 0       | 0       |
| Runs              | 1      | 0       | 1       |
| Predictions       | 1      | 0       | 1       |

## Run
- Tasks indem man OpenML Task zu mlr3 Task konvertiert
- Benchmarks durch mlr3benchmark

| Feature: Run Model        | OpenML | mlr3oml | Desired |
| ------------------------- | ------ | ------- | ------- |
| Run models on Task        | 1      | 0       | 1       |
| Run models on Benchmarks  | 1      | 0       | 1       |
| Run models on Collections | 1      | 0       | 1       |
| Run flow                  | 1      | 0       | 0       |
| Run Setup                 | 1      | 0       | 0       |

## Upload
zu Arf datei konvertieren und dann uploaden.

| Feature: Upload | OpenML | mlr3oml | Desired |
| --------------- | ------ | ------- | ------- |
| Run             | 1      | 0       | 1       |
| Benchmark       | 1      | 0       | 1       |
| Collection      | 1      | 0       | 1       |

## Config file
| Config | OpenML | mlr3oml | Desired |
| ------ | ------ | ------- | ------- |
| Set    | 1      | 0       | 1       |
| Save   | 1      | 0       | 1       |
| Get    | 1      | 0       | 1       |


More questions:

- Add all mlr3 task_types to OpenML (?); not available are:
  - Density Task
  - Spatial Task
  - Ordinal Regression task
- What about the OpenML Tasks that are not in mlr3?
  - Subgroup discovery
  - Learning Curve / Supervised Data Stream Classification
- Why not remove incorrect runs / tasks etc. ? Some don't correspond to the
  current API (e.g. run 1 has multiple datasets)
- Should I assume that the data on OpenML is provided correctly
  or should I do additional tests that verify proper format?

# Run
#  - Task
#    - Data
#    - Resampling
#    - list of measures
#  - Setup
#    - Flow
#    - Parameter


run_id = 1

# Download a run
orun = OMLRun$new(run_id) # OMLRun object

# Extract mlr3 objects

# Task
otask = run$task # OMLTask object
task = otask$convert() # mlr3 Task object

## Data
odata = run$task$data # OMLData object
data = odata$convert() # mlr3 DataBackend / data.table

## Resampling
oresampling = run$task$resampling # OMLResampling object
resampling = oresampling$convert() # mlr3 Resampling object

## Measures
omeasures = run$task$measures # OMLMeasure / list(OMLMeasure1, OMLMeasure2, ... )
measures = omeasures$convert() # mlr3Measure / list(mlr3::measure1, mlr3::measure2, ... )

osetup = run$setup # OMLSetup
oflow = run$setup$flow # OMLFlow
oparams = run$setup$parameter # OMLParameter

setup = osetup$convert() # Initilizes the learner described in oflow with the oparams
learner = oflow$convert() # initializes learner with default values

# create instance of OMLResult
oml_result = run_oml(learner, otask)
oml_result = run_oml(osetup, otask)
oml_result = run_oml(learner, task, resampling, list(measures))

# Upload
oml_result$upload() # Uploads also those flows and tasks etc. that are not on OpenML yet

# Collections
run_collection = download_run_collection(1) # list of Runs
task_collection = download_task_collection(1) # list of Tasks






## Questions:
# - What is the best way to upload a learner / pipeline to be able to reconstruct it after download?
#   --> How to handle learners that are not in mlr3?
# - What about hyperparameter tuning and OpenML?
# --> autotuner
# - Reproduziertbarkeit mit renv / callr
