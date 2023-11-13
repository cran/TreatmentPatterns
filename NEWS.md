# TreatmentPatterns 2.6.0
----------
* Added `createSunburstPlot2()` as a replacement of `createSunburstPlot()`. `createSunburstPlot2()` will fully replace `createSunburstPlot()` in a future version.
* Moved `DatabaseConnector` and `SqlRender` to Suggests
* Removed obsolete dependencies: `data.table`, `fs`, and `glue`.
* Internal performance updates.
* Internal code style updates.
* Moved basic filtering when fetching cohort table.
* Updated ReadMe with functionalities
* Internal updates to LRFS paths, to allow paths of identical duration.

# TreatmentPatterns 2.5.2
----------
* Resolved issue with finalize method of CDMInterface class.
* Resolved issue with schema references.
* Unified case style.
* Refactored code for `createSunburstPlot()`.

# TreatmentPatterns 2.5.1
----------
* Added checks for user input: `cohorts` and `cohortTableName`.
* Added option to directly return HTML when using `createSankeyDiagram`.
* Added option to directly return HTML when using `createSunburstPlot`.
* Added option for ageWindow to be a vector.
* Added input checking for `export()`.
* Added additional check for frequency for `createSunburstPlot`.
* Resolved issue in `createSunburstPlot`, when converting from data.table to nested JSON.

# TreatmentPatterns 2.5.0
----------
* Updated interface
* Some internal OO usage
* CDM & DatabaseConnector
* Uses Andromeda to be able to handle bigger than RAM data sets.
  * Shift from data.table to dplyr.
* Updated vignettes using new interface
* General code clean up
* Intermediate files are cached and accessable through Andromeda for review.
* Outputted CSV-files re-imagined to be more flexible for use post TreatmentPatterns.
* Sunburst and Sankey plots are now directly usable with treatmentPathways.csv.