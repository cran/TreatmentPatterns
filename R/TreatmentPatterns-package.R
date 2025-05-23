# Copyright 2024 DARWIN EU®
#
# This file is part of TreatmentPatterns
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import checkmate
#' @import stringr
#' @import utils
#' @import dplyr
#' @import Andromeda
#' @import R6
#' @import sunburstR
#' @import ggplot2
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dbplyr window_order
#' @importFrom networkD3 sankeyNetwork
#' @importFrom stats sd median quantile
#' @importFrom htmlwidgets JS
#' @importFrom tidyr pivot_wider
## usethis namespace: end
NULL

# Global Variables
utils::globalVariables(
  c(
    "x",
    "y",
    "targetCohortId",
    "cohortId",
    "rowNumber",
    "pathway",
    "path",
    "personId",
    "freq",
    "duration_q1",
    "duration_q2",
    "duration_min",
    "duration_max",
    "duration_median",
    "event_name",
    "subject_id_origin"
  )
)
