# TreatmentPatterns 2.5.1
==========
* Added checks for user input: `cohorts` and `cohortTableName`.
* Added option to directly return HTML when using `createSankeyDiagram`.
* Added option to directly return HTML when using `createSunburstPlot`.
* Added option for ageWindow to be a vector.
* Added input checking for `export()`.
* Added additional check for frequency for `createSunburstPlot`.
* Resolved issue in `createSunburstPlot`, when converting from data.table to nested JSON.

# TreatmentPatterns 2.5.0
==========
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