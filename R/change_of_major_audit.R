utils::globalVariables(c(
  "Audit Result", "Campus ID", "Disposition", "ID", "Issue Field",
  "Matched By", "Student ID", "acad.plan", "acceptable_program", "alias",
  "alias_norm", "campus_id", "career", "current_plan", "descr",
  "descr_norm", "first.term", "internal_id", "last.term", "n", "program",
  "program_code", "program_norm", "requested_program", "submission_row",
  "transcript.description", "transcript_norm", "type", "undergraduate"
))

normalize_text <- function(x) {
  x |>
    tidyr::replace_na("") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", " ") |>
    stringr::str_squish()
}

read_change_data <- function(x) {
  if (is.data.frame(x)) {
    return(tidyr::as_tibble(x))
  }

  readr::read_csv(
    x,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    name_repair = "minimal"
  )
}

read_student_data <- function(x) {
  if (is.data.frame(x)) {
    return(tidyr::as_tibble(x))
  }

  # Row 1 is a report title/count; the field names are on row 2.
  readxl::read_excel(x, skip = 1, col_types = "text", .name_repair = "minimal")
}

read_program_names <- function(x) {
  if (is.data.frame(x)) {
    return(tidyr::as_tibble(x))
  }

  # get_program_name_db() returns a named list containing the data frame.
  if (is.list(x) && "program_names_db" %in% names(x)) {
    return(tidyr::as_tibble(x$program_names_db))
  }

  readr::read_csv(
    x,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    name_repair = "minimal"
  )
}

prepare_program_names <- function(program_names) {
  required <- c(
    "acad.plan", "descr", "transcript.description",
    "career", "type", "first.term", "last.term"
  )
  missing <- setdiff(required, names(program_names))
  if (length(missing) > 0) {
    stop("Program mapping is missing: ", paste(missing, collapse = ", "))
  }

  program_names |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ tidyr::replace_na(as.character(.x), "")
      ),
      descr_norm = normalize_text(descr),
      transcript_norm = normalize_text(transcript.description),
      current_plan = last.term == "",
      undergraduate = career == "UGRD"
    )
}

choose_mapping_row <- function(candidates) {
  if (nrow(candidates) == 0) {
    return(candidates)
  }

  candidates |>
    dplyr::arrange(
      dplyr::desc(current_plan),
      dplyr::desc(undergraduate),
      dplyr::desc(suppressWarnings(as.numeric(first.term)))
    ) |>
    dplyr::slice(1)
}

prepare_pre_professional_aliases <- function(alias_lookup, mapping) {
  if (!is.data.frame(alias_lookup)) {
    stop(
      "The pre-professional track alias lookup must be a data frame ",
      "with alias and program columns."
    )
  }

  required <- c("alias", "program")
  missing <- setdiff(required, names(alias_lookup))
  if (length(missing) > 0) {
    stop(
      "Pre-professional track alias lookup is missing: ",
      paste(missing, collapse = ", ")
    )
  }

  aliases <- tidyr::as_tibble(alias_lookup) |>
    dplyr::transmute(
      alias = stringr::str_squish(tidyr::replace_na(as.character(alias), "")),
      program = stringr::str_squish(tidyr::replace_na(as.character(program), "")),
      alias_norm = normalize_text(alias),
      program_norm = normalize_text(program)
    ) |>
    dplyr::filter(alias_norm != "", program_norm != "")

  if (anyDuplicated(aliases$alias_norm)) {
    duplicates <- aliases |>
      dplyr::count(alias_norm) |>
      dplyr::filter(n > 1) |>
      dplyr::pull(alias_norm)
    stop(
      "Duplicate normalized aliases in pre-professional lookup: ",
      paste(duplicates, collapse = ", ")
    )
  }

  resolved <- aliases |>
    dplyr::rowwise() |>
    dplyr::mutate(
      program_code = {
        by_code <- mapping |>
          dplyr::filter(
            undergraduate,
            type == "Pre Prof",
            normalize_text(acad.plan) == program_norm
          ) |>
          choose_mapping_row()

        by_description <- mapping |>
          dplyr::filter(
            undergraduate,
            type == "Pre Prof",
            descr_norm == program_norm
          ) |>
          choose_mapping_row()

        selected <- if (nrow(by_code) > 0) by_code else by_description
        if (nrow(selected) == 0) NA_character_ else selected$acad.plan[[1]]
      }
    ) |>
    dplyr::ungroup()

  unresolved <- resolved |> dplyr::filter(is.na(program_code))
  if (nrow(unresolved) > 0) {
    stop(
      "Alias lookup contains program values that do not resolve to an ",
      "undergraduate Pre Prof plan: ",
      paste(unresolved$program, collapse = ", ")
    )
  }

  resolved |> dplyr::select(alias, program, alias_norm, program_code)
}

prepare_acceptable_program_alternatives <- function(alternatives, mapping) {
  if (!is.data.frame(alternatives)) {
    stop(
      "Acceptable program alternatives must be a data frame with ",
      "requested_program and acceptable_program columns."
    )
  }

  required <- c("requested_program", "acceptable_program")
  missing <- setdiff(required, names(alternatives))
  if (length(missing) > 0) {
    stop(
      "Acceptable program alternatives are missing: ",
      paste(missing, collapse = ", ")
    )
  }

  alternatives <- tidyr::as_tibble(alternatives) |>
    dplyr::transmute(
      requested_program = stringr::str_squish(
        tidyr::replace_na(as.character(requested_program), "")
      ),
      acceptable_program = stringr::str_squish(
        tidyr::replace_na(as.character(acceptable_program), "")
      )
    ) |>
    dplyr::filter(requested_program != "", acceptable_program != "") |>
    dplyr::distinct()

  known_codes <- mapping |>
    dplyr::filter(undergraduate) |>
    dplyr::pull(acad.plan)

  unknown <- unique(c(
    setdiff(alternatives$requested_program, known_codes),
    setdiff(alternatives$acceptable_program, known_codes)
  ))
  if (length(unknown) > 0) {
    stop(
      "Acceptable program alternatives contain unknown undergraduate codes: ",
      paste(unknown, collapse = ", ")
    )
  }

  alternatives
}

major_family_codes <- function(program_code, mapping) {
  source <- mapping |>
    dplyr::filter(undergraduate, acad.plan == program_code) |>
    choose_mapping_row()

  if (nrow(source) == 0) {
    return(character())
  }

  transcript <- source$transcript_norm[[1]]
  code_stem <- stringr::str_remove(program_code, "\\.[^.]+$")

  mapping |>
    dplyr::filter(
      undergraduate,
      type %in% c(
        "Major", "2nd major",
        "Pre-admit to major", "Pre-admit to 2nd major"
      ),
      (transcript != "" & transcript_norm == transcript) |
        stringr::str_remove(acad.plan, "\\.[^.]+$") == code_stem
    ) |>
    dplyr::pull(acad.plan) |>
    unique()
}

expand_acceptable_codes <- function(
  expected_codes,
  alternatives,
  comparison_kind,
  mapping
) {
  if (length(expected_codes) == 0 || nrow(alternatives) == 0) {
    return(expected_codes)
  }

  expanded <- expected_codes

  for (row_number in seq_len(nrow(alternatives))) {
    requested <- alternatives$requested_program[[row_number]]
    acceptable <- alternatives$acceptable_program[[row_number]]

    if (requested %in% expected_codes) {
      expanded <- c(expanded, acceptable)
    }

    if (comparison_kind == "second_major") {
      requested_family <- major_family_codes(requested, mapping)
      if (length(intersect(expected_codes, requested_family)) > 0) {
        expanded <- c(
          expanded,
          major_family_codes(acceptable, mapping)
        )
      }
    }
  }

  unique(expanded)
}

resolve_primary_program <- function(label, mapping, allowed_types = NULL) {
  label_norm <- normalize_text(label)
  if (label_norm == "") {
    return(tidyr::tibble())
  }

  candidates <- mapping |>
    dplyr::filter(undergraduate, descr_norm == label_norm)

  if (!is.null(allowed_types)) {
    candidates <- candidates |> dplyr::filter(type %in% allowed_types)
  }

  choose_mapping_row(candidates)
}

resolve_track_code <- function(label, mapping, alias_lookup) {
  label_norm <- normalize_text(label)
  if (label_norm == "") {
    return(NA_character_)
  }

  alias_match <- alias_lookup |>
    dplyr::filter(alias_norm == label_norm)
  if (nrow(alias_match) == 1) {
    return(alias_match$program_code[[1]])
  }

  candidates <- mapping |>
    dplyr::filter(
      undergraduate,
      type == "Pre Prof",
      descr_norm == label_norm
    )

  selected <- choose_mapping_row(candidates)
  if (nrow(selected) == 0) NA_character_ else selected$acad.plan[[1]]
}

resolve_second_major_codes <- function(label, mapping) {
  primary <- resolve_primary_program(
    label,
    mapping,
    allowed_types = c("Major", "Pre-admit to major")
  )

  if (nrow(primary) == 0) {
    return(character())
  }

  primary_code <- primary$acad.plan[[1]]
  transcript <- primary$transcript_norm[[1]]
  code_stem <- stringr::str_remove(primary_code, "\\.[^.]+$")

  second_candidates <- mapping |>
    dplyr::filter(
      undergraduate,
      type %in% c("2nd major", "Pre-admit to 2nd major"),
      (transcript != "" & transcript_norm == transcript) |
        stringr::str_remove(acad.plan, "\\.[^.]+$") == code_stem
    )

  second <- choose_mapping_row(second_candidates)
  second_code <- if (nrow(second) == 0) character() else second$acad.plan[[1]]

  # Accept both representations. The .2 code is the normal student-record
  # representation; the primary code is retained as an equivalence fallback.
  unique(c(second_code, primary_code))
}

comment_false_positive <- function(field, comments, minor_or_other) {
  comments_norm <- normalize_text(comments)

  field == "Desired Major" &&
    identical(stringr::str_to_lower(tidyr::replace_na(minor_or_other, "")), "true") &&
    stringr::str_detect(comments_norm, "\\bminor\\b") &&
    !stringr::str_detect(
      comments_norm,
      "change .*major|changing .*major|switch .*major|major from|to .*major"
    )
}

collapse_programs <- function(values) {
  values <- stringr::str_squish(tidyr::replace_na(as.character(values), ""))
  values <- unique(values[values != ""])
  paste(values, collapse = " | ")
}

recorded_program_summary <- function(submission) {
  collapse_programs(c(
    submission$`1st Major`[[1]],
    submission$`Plan 2`[[1]],
    submission$`Plan 3`[[1]],
    submission$`Plan 4`[[1]]
  ))
}

desired_program_summary <- function(submission, mapping, alias_lookup) {
  codes <- character()

  primary <- resolve_primary_program(
    submission$`Desired Major`[[1]],
    mapping,
    allowed_types = c("Major", "Pre-admit to major")
  )
  if (nrow(primary) > 0) {
    codes <- c(codes, primary$acad.plan[[1]])
  }

  second_major <- resolve_second_major_codes(
    submission$`Desired 2nd Major`[[1]],
    mapping
  )
  if (length(second_major) > 0) {
    # The student-record second-major representation is returned first.
    codes <- c(codes, second_major[[1]])
  }

  for (field in c(
    "Desired Track/Emphasis/Concentration",
    "Desired 2nd Track/Emphasis/Concentration"
  )) {
    value <- submission[[field]][[1]]
    if (!stringr::str_detect(normalize_text(value), "\\bconcentration\\b")) {
      track_code <- resolve_track_code(value, mapping, alias_lookup)
      if (!is.na(track_code)) {
        codes <- c(codes, track_code)
      }
    }
  }

  for (field in c("Desired Minor", "Desired 2nd Minor")) {
    resolved <- resolve_primary_program(submission[[field]][[1]], mapping)
    if (nrow(resolved) > 0) {
      codes <- c(codes, resolved$acad.plan[[1]])
    }
  }

  collapse_programs(codes)
}

#' Audit change-of-major submissions against student records
#'
#' Compares requested majors, second majors, tracks, concentrations, and minors
#' from change-of-major submissions with the programs currently recorded for
#' each student. Student records are matched first by Campus ID and then by
#' internal ID. Program descriptions are translated to academic-plan codes
#' using the UWL program-name database.
#'
#' @param change_of_major A data frame containing change-of-major submissions,
#'   or a path to a CSV file. The data must include the form columns used by
#'   the audit, including `Reference Number`, `Date Submitted`, `Student ID`,
#'   student name and comment fields, the desired-program fields, and the
#'   minor-or-other-program indicator.
#' @param student_records A data frame containing student records, or a path to
#'   an Excel workbook. Excel input is read with its first row skipped because
#'   the report field names are expected on row 2. Required columns are `ID`,
#'   `Campus ID`, `1st Major`, and `Plan 2` through `Plan 4`.
#' @param program_names A program-name mapping data frame, a CSV path, or the
#'   named list returned by [UWLdbr::get_program_name_db()]. Defaults to the
#'   current UWL program-name database. The mapping must contain `acad.plan`,
#'   `descr`, `transcript.description`, `career`, `type`, `first.term`, and
#'   `last.term`.
#' @param pre_professional_track_alias_lookup A data frame with columns `alias`
#'   and `program`. Each program must resolve to an undergraduate `Pre Prof`
#'   plan. Defaults to the `pre_professional_track_alias` lookup from UWLdbr.
#' @param acceptable_program_alternatives A data frame with columns
#'   `requested_program` and `acceptable_program`. Use it to treat an alternate
#'   undergraduate academic-plan code as satisfying a requested program.
#' @param exception_output Optional path prefix for a dated CSV containing only
#'   exception rows. Do not include the date or `.csv`; both are appended.
#' @param detail_output Optional path prefix for a dated CSV containing all
#'   comparison rows. Do not include the date or `.csv`; both are appended.
#'
#' @return A named list with four tibbles: `summary` (audit counts),
#'   `exceptions` (nonmatching or unresolved comparisons), `detail` (all
#'   comparisons), and `matched_submissions` (submissions joined to student
#'   records).
#'
#' @details Concentrations are treated as embedded in the primary-major code.
#'   Requested second majors may match either the second-major plan code or its
#'   primary-major equivalent. When output prefixes are supplied, files are
#'   written as `<prefix>_YYYY-MM-DD.csv`.
#'
#' @examples
#' \dontrun{
#' alternatives <- tibble::tribble(
#'   ~requested_program, ~acceptable_program,
#'   "RSDMSEV.BS", "PREDMSE.BS",
#'   "NMT.BS", "PRENMT.BS"
#' )
#'
#' result <- audit_change_of_major(
#'   change_of_major = "change_requests.csv",
#'   student_records = "student_records.xlsx",
#'   acceptable_program_alternatives = alternatives,
#'   exception_output = "output/change_of_major_exceptions",
#'   detail_output = "output/change_of_major_audit_detail"
#' )
#'
#' result$summary
#' result$exceptions
#' }
#'
#' @export
audit_change_of_major <- function(
  change_of_major,
  student_records,
  program_names = UWLdbr::get_program_name_db(),
  pre_professional_track_alias_lookup =
    UWLdbr::get_lookup_by_name("pre_professional_track_alias"),
  acceptable_program_alternatives = tibble::tribble(
    ~requested_program, ~acceptable_program
  ),
  exception_output = NULL,
  detail_output = NULL
) {
  changes <- read_change_data(change_of_major)
  students <- read_student_data(student_records)
  mapping <- prepare_program_names(read_program_names(program_names))
  alias_lookup <- prepare_pre_professional_aliases(
    pre_professional_track_alias_lookup,
    mapping
  )
  acceptable_alternatives <- prepare_acceptable_program_alternatives(
    acceptable_program_alternatives,
    mapping
  )

  required_change <- c(
#    "Reference Number", "Date Submitted", "Student ID", "First Name", "Last Name", "Comments",
    "Reference Number", "Date Submitted", "Student ID", "First Name", "Last Name", "Description of Changes",
    "Desired Major", "Desired Track/Emphasis/Concentration",
    "Desired 2nd Major", "Desired 2nd Track/Emphasis/Concentration",
    "Desired Minor", "Desired 2nd Minor",
    "None of the above. I'm changing/adding a minor or other program"
  )
  required_student <- c(
    "ID", "Campus ID", "1st Major",
    "Plan 2", "Plan 3", "Plan 4"
  )

  missing_change <- setdiff(required_change, names(changes))
  missing_student <- setdiff(required_student, names(students))
  if (length(missing_change) > 0) {
    stop("Change data is missing: ", paste(missing_change, collapse = ", "))
  }
  if (length(missing_student) > 0) {
    stop("Student data is missing: ", paste(missing_student, collapse = ", "))
  }

  changes <- changes |>
    dplyr::mutate(
      submission_row = dplyr::row_number(),
      student_id_input = stringr::str_trim(as.character(`Student ID`))
    )

  students <- students |>
    dplyr::mutate(
      internal_id = stringr::str_trim(as.character(ID)),
      campus_id = stringr::str_trim(as.character(`Campus ID`))
    )

  # Student ID on the form may contain either Campus ID or internal ID.
  matched <- changes |>
    dplyr::left_join(
      students |> dplyr::mutate(student_id_input = campus_id, matched_by = "Campus ID"),
      by = "student_id_input",
      suffix = c("", ".student"),
      relationship = "many-to-one"
    )

  still_unmatched <- which(is.na(matched$matched_by))
  if (length(still_unmatched) > 0) {
    internal_matches <- changes[still_unmatched, ] |>
      dplyr::left_join(
        students |> dplyr::mutate(student_id_input = internal_id, matched_by = "ID"),
        by = "student_id_input",
        suffix = c("", ".student"),
        relationship = "many-to-one"
      )

    student_columns <- setdiff(names(internal_matches), names(changes))
    matched[still_unmatched, student_columns] <-
      internal_matches[, student_columns, drop = FALSE]
  }

  comparison_specs <- tidyr::tribble(
    ~field, ~kind, ~student_slots,
    "Desired Major", "primary_major", list(c("1st Major")),
    "Desired 2nd Major", "second_major", list(c("Plan 2", "Plan 3", "Plan 4")),
    "Desired Track/Emphasis/Concentration", "track", list(c("Plan 2", "Plan 3", "Plan 4")),
    "Desired 2nd Track/Emphasis/Concentration", "track", list(c("Plan 2", "Plan 3", "Plan 4")),
    "Desired Minor", "minor", list(c("Plan 2", "Plan 3", "Plan 4")),
    "Desired 2nd Minor", "minor", list(c("Plan 2", "Plan 3", "Plan 4"))
  )

  detail_rows <- vector("list", 0)

  for (i in seq_len(nrow(matched))) {
    submission <- matched[i, ]
    record_found <- !is.na(submission$matched_by[[1]])
    desired_programs <- desired_program_summary(
      submission,
      mapping,
      alias_lookup
    )
    recorded_programs <- if (record_found) {
      recorded_program_summary(submission)
    } else {
      ""
    }

    if (!record_found) {
      detail_rows[[length(detail_rows) + 1]] <- tibble::tibble(
        submission_row = submission$submission_row,
        `Reference Number` = submission$`Reference Number`,
        `Date Submitted` = submission$`Date Submitted`,
        `Student ID Submitted` = submission$student_id_input,
        `Student Name` = stringr::str_squish(paste(submission$`First Name`, submission$`Last Name`)),
        `Matched By` = NA_character_,
        `Issue Field` = "Student record",
        `Desired Value` = NA_character_,
        `Desired Code(s)` = NA_character_,
        `Student Code(s)` = NA_character_,
        `Audit Result` = "Student not found",
        `Disposition` = "Action required",
        `Desired Programs` = desired_programs,
        `Recorded Programs` = recorded_programs,
        `Comments` = submission$`Description of Changes`
      )
      next
    }

    for (j in seq_len(nrow(comparison_specs))) {
      spec <- comparison_specs[j, ]
      field <- spec$field[[1]]
      kind <- spec$kind[[1]]
      desired_value <- stringr::str_trim(tidyr::replace_na(submission[[field]][[1]], ""))

      if (desired_value == "") {
        next
      }

      expected_codes <- character()
      resolution_note <- ""
      embedded_in_major <- FALSE

      if (kind == "primary_major") {
        resolved <- resolve_primary_program(
          desired_value,
          mapping,
          allowed_types = c("Major", "Pre-admit to major")
        )
        if (nrow(resolved) > 0) {
          expected_codes <- resolved$acad.plan[[1]]
        }
      } else if (kind == "second_major") {
        expected_codes <- resolve_second_major_codes(desired_value, mapping)
      } else if (kind == "track") {
        # Concentrations are normally encoded in the major itself, not as
        # a separate student plan.
        if (stringr::str_detect(normalize_text(desired_value), "\\bconcentration\\b")) {
          primary <- resolve_primary_program(
            submission$`Desired Major`[[1]],
            mapping,
            allowed_types = c("Major", "Pre-admit to major")
          )
          primary_code <- if (nrow(primary) == 0) NA_character_ else primary$acad.plan[[1]]
          if (!is.na(primary_code) && identical(primary_code, submission$`1st Major`[[1]])) {
            expected_codes <- primary_code
            resolution_note <- "Concentration is embedded in the major code"
            embedded_in_major <- TRUE
          }
        } else {
          expected_codes <- resolve_track_code(
            desired_value,
            mapping,
            alias_lookup
          )
        }
      } else if (kind == "minor") {
        resolved <- resolve_primary_program(
          desired_value,
          mapping
        )
        if (nrow(resolved) > 0) {
          expected_codes <- resolved$acad.plan[[1]]
        }
      }

      expected_codes <- expand_acceptable_codes(
        expected_codes,
        acceptable_alternatives,
        kind,
        mapping
      )

      slots <- unlist(spec$student_slots[[1]])
      if (embedded_in_major) {
        slots <- "1st Major"
      }
      recorded_codes <- unname(unlist(submission[slots], use.names = FALSE))
      recorded_codes <- unique(recorded_codes[!is.na(recorded_codes) & recorded_codes != ""])

      result <- if (length(expected_codes) == 0) {
        "Mapping not found"
      } else if (any(expected_codes %in% recorded_codes)) {
        "Match"
      } else {
        "Program mismatch"
      }

      false_positive <- result == "Program mismatch" &&
        comment_false_positive(
          field,
          submission$`Description of Changes`[[1]],
          submission[["None of the above. I'm changing/adding a minor or other program"]][[1]]
        )

      disposition <- dplyr::case_when(
        result == "Match" ~ "No action",
        false_positive ~ "False positive - comments indicate different scope",
        result == "Mapping not found" ~ "Mapping review required",
        submission$`Description of Changes`[[1]] != "" ~ "Review comments",
        TRUE ~ "Action required"
      )

      detail_rows[[length(detail_rows) + 1]] <- tibble::tibble(
        submission_row = submission$submission_row,
        `Reference Number` = submission$`Reference Number`,
        `Date Submitted` = submission$`Date Submitted`,
        `Student ID Submitted` = submission$student_id_input,
        `Student Name` = stringr::str_squish(paste(submission$`First Name`, submission$`Last Name`)),
        `Matched By` = submission$matched_by,
        `Issue Field` = field,
        `Desired Value` = desired_value,
        `Desired Code(s)` = paste(expected_codes, collapse = " | "),
        `Student Code(s)` = paste(recorded_codes, collapse = " | "),
        `Audit Result` = result,
        `Disposition` = disposition,
        `Desired Programs` = desired_programs,
        `Recorded Programs` = recorded_programs,
        `Comments` = submission$`Description of Changes`,
        `Resolution Note` = resolution_note
      )
    }
  }

  detail <- dplyr::bind_rows(detail_rows)
  exceptions <- detail |>
    dplyr::filter(`Audit Result` != "Match") |>
    dplyr::arrange(
      stringr::str_to_lower(`Issue Field`) == "student record",
      submission_row,
      `Issue Field`
    )

  summary <- tibble::tibble(
    metric = c(
      "Submissions",
      "Students found",
      "Students not found",
      "Exception rows",
      "False-positive rows",
      "Mapping-review rows",
      "Other review/action rows"
    ),
    value = c(
      nrow(changes),
      sum(!is.na(matched$matched_by)),
      sum(is.na(matched$matched_by)),
      nrow(exceptions),
      sum(stringr::str_detect(exceptions$Disposition, "^False positive")),
      sum(exceptions$`Audit Result` == "Mapping not found"),
      sum(
        !stringr::str_detect(exceptions$Disposition, "^False positive") &
          exceptions$`Audit Result` != "Mapping not found"
      )
    )
  )

  if (!is.null(exception_output)) {
    readr::write_csv(
      exceptions |> dplyr::select(-submission_row, -`Matched By`, -Disposition, -`Audit Result`),
      paste0(exception_output, "_", lubridate::today(), ".csv"),
      na = ""
    )
  }
  if (!is.null(detail_output)) {
    readr::write_csv(detail, paste0(detail_output, "_", lubridate::today(), ".csv"), na = "")
  }

  list(
    summary = summary,
    exceptions = exceptions,
    detail = detail,
    matched_submissions = matched
  )
}

# Example:

# remotes::install_github("rallen-uwl/UWLcshr", upgrade = "never")

#script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
#from_script <- function(...) file.path(script_dir, ...)

#acceptable_alternatives <- tibble::tribble(
#  ~requested_program, ~acceptable_program,
#  "RSDMSEV.BS", "PREDMSE.BS",
#  "RSDMSGV.BS", "PREDMSG.BS",
#  "NMT.BS", "PRENMT.BS",
#  "RSRAD.BS", "PRERAD.BS",
#  "RSRT.BS", "PRERT.BS",
#  "BIOAGPT.BS", "BIODDPT.BS",
  #"CLIAGCM.BS", "CLIDDCM.BS",
  #"CSAGSE.BS", "CSDDSE.BS",
  #"CSYAGSE.BS", "CSYDDSE.BS",
  #"PHYAGPT.BS", "PHYDDPY.BS",
#  "RMCMAGR.BS", "RMCMDDR.BS",
#  "RMEVAGR.BS", "RMEVDDR.BS",
#  "RMGEAGR.BS", "RMGEDDR.BS",
#  "RMORAGR.BS", "RMORDDR.BS",
#  "RTHAGRT.BS", "RTHDDRT.BS",
  #"STATAGS.BS", "STATDDS.BS",
#)

#audit <-
#  audit_change_of_major(
#    change_of_major = list.files(from_script("input"), pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE),
#    student_records = list.files(from_script("input"), pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE),
#    acceptable_program_alternatives = acceptable_alternatives,
#    exception_output = from_script("output/change_of_major_exceptions"),
#    detail_output = from_script("output/change_of_major_audit_detail")
#  )
