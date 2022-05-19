# Michel Codereview:

https://github.com/openml/OpenML/issues/1145

- [ ] Alle Tests laufen + fehlende Tests hinzufügen
      - row_ids checken --> primary_key muss auf die rowidentifier column gesetzt werden falls die
      existiert
      - Schauen dass das mit den parametern und den appendeten ids wirklich klappt
        --> an einem konkreten Beispiel testen
      - Das mit den row_ids kann man auch manuell testen

- [ ] Den ganzen Code selbst in Ruhe Korrektur lesen + Doku korrigieren

- [ ] vignette erstellen sowas in Richtung: wir laden ne run study runter,
- Checken dass alle Funktionen / Objekte die definiert sind auch hergenommen werden.

- [ ] Upload checken (inklusive evaluations)
      - oml_ids --> upload (wo wird das eigentlich herngnomm)
        hash integrieren um checken ob das verändert wird, das wird von der S3 methode get_oml_id erledigt
      - Aufpassen mit den Versionen beim upload wenn man einen Flow vom rds rekonstruiert:
        ausprobieren was passiert wenn man 2 x den gleichen Flow mit unterschiedlicher binary hochlädt.
      --> Evtl neue Instanz aus der Klasse erstellen und checken ob die gleich sind bevor man uploaded
      - Wird nur die Klasse oder die Instanz hochgeladen
      - Das mit dem hashing fertig machen
    Wenn der Test Server läuft: testen
  fügen unser eigenes resultat hinzu und vergleichen das ganze mal

- Bei den Tests einen eigenen Cache Ordner erstellen und wieder löschen nach den Tests
  Loakle Tests: kein caching
  Github actoins tests: caching

- delete all the invalid mlr3 Objects from OpenML

- Do caching of the description download in the default args desc = download_run_desc

- Ich finde nicht, dass die ignore_attribute und die row_identifier Spalte gelöscht werden sollen
    --> make it an optional argument "discard"

- Check that the predictions for the correct predict set are being uploaded


- Parquet: sparse parquet


Für Michel:

- Maybe change $data from active field to method to allow obtaining the whole dataset and immediately remove
ignore_attribute and row_id_attribute (ask Michel)
FIXME: positive class? <-- Was heißt das
- Checke das alles mit den Namespaces passt (suggest vs import)
(in der download_data). Sie werden ja entsprechend behandelt wenn as_data_backend gecalled wird.
Was hier noch passieren muss: Die primary_key Spalte muss entsprechend gesetzt werden und bei
as_task muss dann as_data_backend gecalled werden und die ignore_attribute features aus den features
entfernt werden. Das mit den ignore_attributes ist wichtig, weil die resampling splits darauf
basierend berechnet werden (am besten checke ich das nochmal selber mit einem Beispiel)
- Aufpassen mit den row identifie

Später:
- [ ] Support parquet (upload and download) Support parquet (upload and download)
- [ ] Add format_list methods (data.table) for OMLRun undso wenn das endlich mal in data.table kommt
- [ ] http test

# TODO File for mlr3oml
Other tasks:
- [ ] Verify existing functions
- [ ] Better console output when downloading collections.
- [ ] Add tags to flows (mlr3), must be done with seperate API Call
- [ ] Extend it to survival tasks and maybe also cluster


<!-- Optional: -->
<!-- - When doing multiuploads that fails, provide the possibility to delete those that were -->
<!-- uploaded. Most important for uploading benchmark results. -->
