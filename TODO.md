# TODO File for mlr3oml 


## Aufgaben
- Existierende Funktionalität checken und Tests entwickeln (z.B.
  auf Typen checken etc.)
- Objekt für OMLRun  --> Schauen wie es bei OMLTask gemacht wurde 
- Task erstellen wenn vom user gewollt 

## Fragen an OpenML
 - API muss laufen 


Questions: 
1. Why the mlr3oml rewrite
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

