SELECT
  @resultSchema.@cohortTable.cohort_definition_id,
  @resultSchema.@cohortTable.subject_id,
  @resultSchema.@cohortTable.cohort_start_date,
  @resultSchema.@cohortTable.cohort_end_date,
  YEAR(@resultSchema.@cohortTable.cohort_start_date) - @cdmSchema.person.year_of_birth AS age,
  @cdmSchema.concept.concept_name AS sex
FROM
  @resultSchema.@cohortTable
INNER JOIN @cdmSchema.person
  ON @resultSchema.@cohortTable.subject_id = @cdmSchema.person.person_id
INNER JOIN @cdmSchema.concept
  ON @cdmSchema.person.gender_concept_id = @cdmSchema.concept.concept_id
INNER JOIN
  (
    SELECT @resultSchema.@cohortTable.subject_id
    FROM @resultSchema.@cohortTable
    WHERE @resultSchema.@cohortTable.cohort_definition_id IN (@targetCohortId)
  ) AS cross_sec
  ON cross_sec.subject_id = @resultSchema.@cohortTable.subject_id
WHERE
  cohort_definition_id IN (@cohortIds)
  AND cohort_end_date - cohort_start_date > @minEraDuration
