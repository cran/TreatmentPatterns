# TreatmentPatterns 2.6.3
---------
* Fixed issue where there were duplicate rows in the exported treatmentPathways.csv file.
* Added more comprehensive description for parameter `ageWindow` for `export()`.
* Added additional tests validating the counts per pathway being exported by `export()`.
* Fixed issue with re-occurring treatments in pathways.
* Added dedicated logical tests for various pathways.
* Added dedicated tests for target-event cohort overlaps.
* Fixed spelling error in outputs, and interaction with them in shiny module.

# TreatmentPatterns 2.6.2
---------
* Fixed spelling mistake 'summaryStatsTherapyDuraion' to 'summaryStatsTherapyDuration'.
* Added error when only one level of data is available in the data when computing a sankey diagram.
* Removed `createSankeyDiagram`
* Renamed `createSankeyDiagram2` to `createSankeyDiagram`
* Removed `createSunburstPlot`
* Renamed `createSunburstPlot2` to `createSunburstPlot`
* Fixed issue where target cohorts had to be at least 1 day long to be included when using DatabaseConnector.
* Fixed CRAN error when running examples.
* Renamed `cellCount` to `minCellCount` in export documentation.
* Fixed issue where counts of pathways were not computed correctly.
* Fixed spelling error `summaryStatsTherapyDuraion` to `summaryStatsTherapyDuration`.
* Fixed issue in `export` where merging two tables could fail, if one was empty.


# TreatmentPatterns 2.6.1
---------
* Bumped R version to 4.2.1
* Added additional unit-tests for `createSunburstPlot2()` and `createSankeyDiagram2()`
* Updated to use DATEDIFF to compute difference between dates when fetching data from the database.
* Added functionality to choose how groups below the minimum cell count are censored.
* Made some parameters ambiguous for `createSankeyDiagram2` and `createSunburstPlot2()` for more control over the figures.
* Removed `addNoPath` parameter.
* Renamed `minFreq` parameter to `minCellCount`.
* Added Shiny app with exported module classes.
* Added censoring options of treatment pathways falling below the `minCellCount`.
* Simplified part of `computePathways()`.
* Significantly improved performance of `export()`.


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