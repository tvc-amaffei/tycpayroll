#!/usr/bin/env Rscript
# Standalone payroll runner adapted from `tyc-instructor-payroll-2.qmd`
# Usage: Rscript run_payroll.R --month=10 --year=2025 --google_user_email=you@example.com

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
    params <- list(
        month = NULL,
        year = NULL,
        google_user_email = NULL,
        # !/usr/bin/env Rscript
        payroll_folder_url = NULL,
        use_local = FALSE
    )
    for (a in args) {
        if (grepl("^--", a)) {
            kv <- strsplit(sub("^--", "", a), "=")[[1]]
            key <- kv[1]
            val <- if (length(kv) > 1) kv[2] else TRUE
            if (key %in% names(params)) params[[key]] <- val
        }
    }
    params
}

params <- parse_args(args)

# --- Setup logging -----------------------------------------------------
log_dir <- "logs"
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
logfile <- file.path(log_dir, paste0("payroll-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".log"))
log_con <- file(logfile, open = "a")
log_write <- function(level, msg) {
    line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [", level, "] ", msg)
    # write to logfile
    writeLines(line, con = log_con)
    flush(log_con)
    # also echo to console
    cat(line, "\n")
}
log_info <- function(msg) log_write("INFO", msg)
log_warn <- function(msg) log_write("WARN", msg)
log_error <- function(msg) log_write("ERROR", msg)

# helper for friendly errors that also logs
fail <- function(msg, hint = NULL) {
    log_error(msg)
    if (!is.null(hint)) log_info(paste0("HINT: ", hint))
    close(log_con)
    quit(status = 1)
}

# --- Structured CLI parsing (optparse if available) -------------------
dry_run <- FALSE
verbose <- FALSE
output_dir <- "instructor_pdfs"
if (requireNamespace("optparse", quietly = TRUE)) {
    optparse <- asNamespace("optparse")
    option_list <- list(
        optparse$make_option(c("--month"), type = "character", default = NULL, help = "Month (numeric, e.g. 10)"),
        optparse$make_option(c("--year"), type = "character", default = NULL, help = "Year (4-digit, e.g. 2025)"),
        optparse$make_option(c("--google_user_email"), type = "character", default = NULL, help = "Google email for Drive/Sheets auth"),
        optparse$make_option(c("--use_local"), action = "store_true", default = FALSE, help = "Use local CSVs instead of Drive"),
        optparse$make_option(c("--dry-run"), action = "store_true", default = FALSE, help = "Validate inputs and exit without rendering"),
        optparse$make_option(c("--output-dir"), type = "character", default = "instructor_pdfs", help = "Output directory for PDFs"),
        optparse$make_option(c("--verbose"), action = "store_true", default = FALSE, help = "Verbose logging to console")
    )
    parser <- optparse$OptionParser(option_list = option_list)
    opt <- tryCatch(optparse$parse_args(parser, args = args), error = function(e) NULL)
    if (!is.null(opt)) {
        if (!is.null(opt$month)) params$month <- opt$month
        if (!is.null(opt$year)) params$year <- opt$year
        if (!is.null(opt$google_user_email)) params$google_user_email <- opt$google_user_email
        if (isTRUE(opt$use_local)) params$use_local <- TRUE
        dry_run <- isTRUE(opt$`dry-run`)
        verbose <- isTRUE(opt$verbose)
        output_dir <- opt$`output-dir`
    }
} else {
    log_info("optparse not available; using simple CLI parsing and interactive prompts")
}

log_info(paste0("Starting payroll run. Log file: ", logfile))

## Simple dependency checker
required_pkgs <- c("tidyverse", "googledrive", "googlesheets4", "jsonlite", "quarto", "readr")
missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
    stop(
        "Missing required packages: ", paste(missing, collapse = ", "),
        "\nInstall them in R with: install.packages(c(\"", paste(missing, collapse = "\", \""), "\"))"
    )
}
# Interactive prompts: if running interactively and any key params are missing,
# ask the user. This makes it friendly for non-RStudio CLI usage.
if (interactive()) {
    if (is.null(params$title) || params$title == "") {
        v <- readline("Enter report title (or press Enter to accept default): ")
        if (nzchar(v)) params$title <- v
    }
    if (is.null(params$month) || params$month == "") {
        v <- readline("Enter month (numeric, e.g. 10): ")
        if (nzchar(v)) params$month <- sprintf("%02s", v)
    }
    if (is.null(params$year) || params$year == "") {
        v <- readline("Enter year (4-digit, e.g. 2025): ")
        if (nzchar(v)) params$year <- v
    }
    if (is.null(params$google_user_email) || params$google_user_email == "") {
        v <- readline("Enter Google account email for Drive/Sheets (leave blank to run local mode): ")
        if (nzchar(v)) params$google_user_email <- v
    }
}

library(tidyverse)
library(googledrive)
library(googlesheets4)
library(jsonlite)
library(quarto)

#-- Defaults (taken from original qmd)
if (is.null(params$google_user_email)) params$google_user_email <- "andymaffei@gmail.com"
if (is.null(params$month)) params$month <- format(Sys.Date(), "%m")
if (is.null(params$year)) params$year <- format(Sys.Date(), "%Y")
if (is.null(params$payroll_folder_url)) params$payroll_folder_url <- "https://drive.google.com/drive/folders/1si6xal0w6JPyy-m7RVm3zJ9q7n0tf2tm"

payrate_file_name <- "tyc-payrates"
instructor_file_name <- "tyc-instructors"
payroll_csvs_folder_name <- "payroll_csvs"

# Create local folders if missing
dir.create("csvs", showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)

# Remove any existing PDF files under instructor_pdfs
existing <- list.files("instructor_pdfs", full.names = TRUE)
if (length(existing)) file.remove(existing)

use_local <- identical(tolower(as.character(params$use_local)), "true") || identical(params$use_local, TRUE)
log_info(paste0("Mode: ", ifelse(use_local, "local", "drive"), "; month=", params$month, ", year=", params$year))

if (!use_local) {
    cat("Authenticating with Google for user:", params$google_user_email, "\n")
    googlesheets4::gs4_auth(email = params$google_user_email)
    googledrive::drive_auth(email = params$google_user_email)

    # Extract folder id
    payroll_folder_id <- sub(".*/folders/(.*)$", "\\1", params$payroll_folder_url)
    payroll_folder_dirlist <- drive_ls(path = as_id(payroll_folder_id))

    # Payrate and instructor files
    payrate_file <- payroll_folder_dirlist |> filter(name == payrate_file_name)
    instructor_file <- payroll_folder_dirlist |> filter(name == instructor_file_name)
    payrollcsvs_folder <- payroll_folder_dirlist |> filter(name == payroll_csvs_folder_name)

    if (nrow(payrate_file) == 0) fail(paste0("Could not find payrate file named: ", payrate_file_name), "Verify that the Drive folder contains the spreadsheet named exactly as 'tyc-payrates'.")
    if (nrow(instructor_file) == 0) fail(paste0("Could not find instructor file named: ", instructor_file_name), "Verify that the Drive folder contains the spreadsheet named exactly as 'tyc-instructors'.")
    if (nrow(payrollcsvs_folder) == 0) fail("Could not find payroll_csvs folder in drive folder", "Make sure the top-level payroll folder contains a subfolder named 'payroll_csvs'.")

    cat("Reading payrate and instructor sheets from Google Sheets...\n")
    payrate_tbl_raw <- read_sheet(payrate_file$id)
    # ensure expected columns exist before renaming
    needed_payrate_cols <- c("WL Hourly Payrate", "PP7", "Cap", "Base 0", "Base 1 Plus")
    missing_pr <- setdiff(needed_payrate_cols, names(payrate_tbl_raw))
    if (length(missing_pr) > 0) {
        fail(
            paste0("payrate sheet is missing expected columns: ", paste(missing_pr, collapse = ", ")),
            paste0("Found columns: ", paste(names(payrate_tbl_raw), collapse = ", "))
        )
    }
    payrate_tbl <- payrate_tbl_raw |>
        rename(
            hourly_pay_rate = `WL Hourly Payrate`,
            pp7 = PP7,
            cap = Cap,
            base0 = `Base 0`,
            base1 = `Base 1 Plus`
        )

    instructor_tbl_raw <- read_sheet(instructor_file$id)
    needed_instr_cols <- c("Name", "Hourly Pay Rate")
    missing_instr <- setdiff(needed_instr_cols, names(instructor_tbl_raw))
    if (length(missing_instr) > 0) {
        fail(
            paste0("instructor sheet is missing expected columns: ", paste(missing_instr, collapse = ", ")),
            paste0("Found columns: ", paste(names(instructor_tbl_raw), collapse = ", "))
        )
    }
    instructor_tbl <- instructor_tbl_raw |>
        rename(
            name = Name,
            instructor_pay_rate = `Hourly Pay Rate`,
            anniversary_date = `Anniversary Date`
        )

    # Find and download this month's payroll csv from the payroll_csvs folder
    payrollcsvs_folder_dirlist <- drive_ls(path = as_id(payrollcsvs_folder$id))
    payroll_file_name <- paste0("tyc_wlpayroll-", params$year, params$month, ".csv")
    payrollcsv_file <- payrollcsvs_folder_dirlist |> filter(name == payroll_file_name)
    if (nrow(payrollcsv_file) == 0) fail(paste0("Could not find payroll csv: ", payroll_file_name), "Confirm the CSV exists in the 'payroll_csvs' Drive folder and is named exactly 'tyc_wlpayroll-<YEAR><MONTH>.csv'.")
    drive_download(payrollcsv_file$id, overwrite = TRUE)
    # read the CSV; tolerate variable column types but require essential headers
    services_held <- read_csv(payroll_file_name, guess_max = 5000, show_col_types = FALSE)
    required_cols <- c("Staff", "Date", "Time", "Details", "Service Type", "Pay", "Booked", "Attended", "Late Cancels", "Pay Rate")
    missing_cols <- setdiff(required_cols, names(services_held))
    if (length(missing_cols) > 0) {
        log_error(paste0("Downloaded payroll CSV is missing required columns: ", paste(missing_cols, collapse = ", ")))
        log_info(paste0("CSV columns found: ", paste(names(services_held), collapse = ", ")))
        fail("Payroll CSV schema mismatch.", "Confirm the payroll CSV has not changed. If the vendor changed headers, update the script or provide a local CSV in the expected format.")
    }
    # move csv into csvs/
    file.rename(payroll_file_name, file.path("csvs", payroll_file_name))
    payroll_local_csv <- file.path("csvs", payroll_file_name)
} else {
    # Local mode: expect the payroll CSV to be present in csvs/
    payroll_file_name <- paste0("tyc_wlpayroll-", params$year, params$month, ".csv")
    payroll_local_csv <- file.path("csvs", payroll_file_name)
    if (!file.exists(payroll_local_csv)) fail(paste0("Local payroll CSV not found: ", payroll_local_csv), "Place the file under csvs/ or run without --use_local=false to fetch from Google Drive.")

    # Try to find payrate and instructors CSVs under extdata/ or project root
    payrate_candidates <- c("extdata/tyc-payrates.csv", "extdata/payrate.csv", "extdata/payrates.csv")
    instructor_candidates <- c("extdata/tyc-instructors.csv", "extdata/instructors.csv")
    pfile <- instructor_file <- NULL
    for (c in payrate_candidates) {
        if (file.exists(c)) {
            pfile <- c
            break
        }
    }
    for (c in instructor_candidates) {
        if (file.exists(c)) {
            instructor_file <- c
            break
        }
    }
    if (is.null(pfile) || is.null(instructor_file)) fail("Local payrate or instructor CSVs not found in extdata/; either provide them or run in Google mode", "Expected files such as extdata/tyc-payrates.csv and extdata/tyc-instructors.csv")
    payrate_tbl <- read_csv(pfile, show_col_types = FALSE)
    instructor_tbl <- read_csv(instructor_file, show_col_types = FALSE)
    services_held <- read_csv(payroll_local_csv, guess_max = 5000, show_col_types = FALSE)
    # Validate local files have expected headers
    if (!all(c("Name", "Hourly Pay Rate") %in% names(instructor_tbl))) {
        fail(
            "Local instructor CSV missing expected columns (Name, Hourly Pay Rate)",
            paste0("Found columns: ", paste(names(instructor_tbl), collapse = ", "))
        )
    }
    if (!any(grepl("WL Hourly Payrate|Hourly Payrate|hourly_pay_rate", names(payrate_tbl), ignore.case = TRUE))) {
        fail(
            "Local payrate CSV does not contain an hourly pay rate column.",
            paste0("Found columns: ", paste(names(payrate_tbl), collapse = ", "))
        )
    }
}

# Now transform the payroll data to match the qmd processing pipeline
services_held <- services_held |>
    mutate(wl_calc_pay = readr::parse_number(Pay)) |>
    mutate(short_service = ifelse(
        grepl("^ *[IiEe][NnXx][ Tt][SsEe][TtRr][UuNn][DdAa][IiLl]", Details),
        gsub("^ *[IiEe][NnXx][ Tt][SsEe][TtRr][UuNn][DdAa][IiLl]O* *", "", Details),
        gsub("^ *Virtual via Zoom *", "", Details)
    )) |>
    rename(
        instructor = Staff,
        date = Date,
        time = Time,
        service_name = Details,
        service_type = `Service Type`,
        bk = Booked,
        at = Attended,
        lc = `Late Cancels`,
        hourly_pay_rate = `Pay Rate`
    )

# Merge studio and virtual
s_services <- services_held |> filter(grepl("[IiEe][NnXx][ Tt][SsEe][TtRr][UuNn][DdAa][IiLl]", service_name))
v_services <- services_held |> filter(grepl("Virtual via Zoom", service_name))

combined_sv <- s_services |>
    left_join(v_services, by = c("instructor", "date", "time", "service_type"), relationship = "one-to-one", suffix = c(".s", ".v"))

v_services_not_in_s_services <- v_services |>
    anti_join(s_services, by = c("instructor", "date", "time", "service_type", "short_service")) |>
    rename(
        service_name.v = service_name,
        bk.v = bk,
        at.v = at,
        lc.v = lc,
        hourly_pay_rate.v = hourly_pay_rate,
        wl_calc_pay.v = wl_calc_pay,
        short_service.v = short_service
    )

combined_sv <- bind_rows(combined_sv, v_services_not_in_s_services)

# Change NAs to zeros
num_cols <- c("bk.s", "at.s", "lc.s", "bk.v", "at.v", "lc.v", "wl_calc_pay.s", "wl_calc_pay.v")
for (c in num_cols) if (c %in% names(combined_sv)) combined_sv[[c]][is.na(combined_sv[[c]])] <- 0
combined_sv <- combined_sv |>
    mutate(total_attendees = ifelse(is.na(at.s), 0, at.s) + ifelse(is.na(lc.s), 0, lc.s) + ifelse(is.na(at.v), 0, at.v))

# Add payrate columns by matching hourly_pay_rate.s to payrate_tbl$hourly_pay_rate
if ("hourly_pay_rate.s" %in% names(combined_sv)) {
    combined_sv_w_pay <- combined_sv |>
        left_join(payrate_tbl, by = c("hourly_pay_rate.s" = "hourly_pay_rate"))
} else {
    combined_sv_w_pay <- combined_sv |>
        left_join(payrate_tbl, by = c("hourly_pay_rate" = "hourly_pay_rate"))
}

# Calculate base_pay, bonus_pay, final_pay
combined_sv_w_pay <- combined_sv_w_pay |>
    mutate(
        base_pay = ifelse(service_type == "Class" & total_attendees == 0, base0, base1),
        bonus_pay = ifelse(service_type == "Class" & (total_attendees - 6) > 0, (total_attendees - 6) * pp7, 0),
        final_pay = ifelse(service_type == "Class", base_pay + bonus_pay, 0)
    ) |>
    mutate(final_pay = ifelse(service_type == "Class" & final_pay > cap, cap, final_pay))

# For Event types, use WL's calculation
combined_sv_w_pay <- combined_sv_w_pay |>
    mutate(final_pay = ifelse(service_type == "Event", (coalesce(wl_calc_pay.s, 0) + coalesce(wl_calc_pay.v, 0)) * 1.0, final_pay))

## Build a clean table matching the template's expected column names
# Determine which short_service and hourly_pay_rate columns exist
short_col <- intersect(c("short_service.s", "short_service", "short_service.v"), names(combined_sv_w_pay))[1]
hourly_col <- intersect(c("hourly_pay_rate.s", "hourly_pay_rate", "hourly_pay_rate.v"), names(combined_sv_w_pay))[1]
if (is.na(short_col)) short_col <- "short_service"
if (is.na(hourly_col)) hourly_col <- "hourly_pay_rate"

services_data_all <- combined_sv_w_pay |>
    mutate(total = coalesce(total_attendees, 0)) |>
    transmute(
        instructor = instructor,
        date = date,
        time = time,
        short_service = .data[[short_col]],
        stype = service_type,
        `at.s` = coalesce(.data[["at.s"]], 0),
        `lc.s` = coalesce(.data[["lc.s"]], 0),
        `at.v` = coalesce(.data[["at.v"]], 0),
        total = total,
        hourly_pay_rate = .data[[hourly_col]],
        final_pay = coalesce(final_pay, 0)
    )

# Create summary (use `total` column)
services_data_summary <- services_data_all |>
    group_by(instructor) |>
    summarize(
        classes = sum(stype == "Class", na.rm = TRUE),
        participants = sum(total, na.rm = TRUE),
        pay = sum(final_pay, na.rm = TRUE)
    )

log_info(paste0("Found ", nrow(services_data_summary), " instructors to render PDFs for."))

if (isTRUE(dry_run)) {
    log_info("Dry-run requested: skipping PDF rendering and reporting checks only.")
    # Summarize issues or counts
    log_info(paste0("Instructors found: ", nrow(services_data_summary)))
    log_info(paste0("Total pay amount across instructors: $", format(sum(services_data_summary$pay, na.rm = TRUE), big.mark = ",")))
    close(log_con)
    quit(status = 0)
}

# Basic sanity checks
if (nrow(services_data_summary) == 0) {
    fail(
        "No instructor payroll rows found after processing the CSV.",
        "Check the CSV for expected rows and headers, or run with --use_local=true to test local files."
    )
}

# Ensure template exists
template_path <- file.path("quarto", "template2.qmd")
if (!file.exists(template_path)) {
    fail(
        paste0("Quarto template not found at: ", template_path),
        "Make sure `quarto/template2.qmd` is present and readable."
    )
}

unique_instructors <- unique(combined_sv_w_pay$instructor)
for (i in seq_along(unique_instructors)) {
    instructor_name <- unique_instructors[i]
    log_info(paste0("Processing: ", instructor_name))
    instructor_payrate <- instructor_tbl |>
        filter(name == instructor_name) |>
        select(instructor_pay_rate)
    if (nrow(instructor_payrate) == 0 || is.na(instructor_payrate$instructor_pay_rate) || instructor_payrate$instructor_pay_rate == "") {
        log_warn(paste0("No pay rate found for instructor '", instructor_name, "' in instructors sheet."))
        log_info("Suggestion: check `extdata/instructors.csv` or the Google `tyc-instructors` sheet for a matching Name and Hourly Pay Rate.")
        payrate_data <- tibble(hourly_pay_rate = NA_character_, pp7 = NA_real_, cap = NA_real_, base0 = NA_real_, base1 = NA_real_)
    } else {
        payrate_data <- payrate_tbl |>
            filter(hourly_pay_rate == instructor_payrate$instructor_pay_rate) |>
            select(hourly_pay_rate, pp7, cap, base0, base1)
        if (nrow(payrate_data) == 0) {
            log_warn(paste0("No matching payrate row for pay rate '", instructor_payrate$instructor_pay_rate, "'."))
            log_info("Suggestion: check `tyc-payrates` sheet for a row where WL Hourly Payrate matches this value.")
            payrate_data <- tibble(hourly_pay_rate = instructor_payrate$instructor_pay_rate, pp7 = NA_real_, cap = NA_real_, base0 = NA_real_, base1 = NA_real_)
        }
    }
    service_data <- services_data_all |> filter(instructor == instructor_name)
    paydatajson <- jsonlite::toJSON(payrate_data)
    serdatajson <- jsonlite::toJSON(service_data)

    outname <- paste0("payroll-", gsub("[[:space:]]+", "_", instructor_name), ".pdf")
    # Use the Quarto CLI to render into the instructor_pdfs dir and pass params via a small YAML file.
    # Try using quarto::quarto_render with quarto args to set the output directory
    tryCatch(
        {
            quarto::quarto_render(
                input = template_path,
                output_file = outname,
                execute_params = list(
                    iname = instructor_name,
                    pdata = paydatajson,
                    cdata = serdatajson
                ),
                # set output-dir relative to the template folder
                quarto_args = c("--output-dir", file.path("..", output_dir))
            )
        },
        error = function(e) {
            log_error(paste0("Error rendering PDF for ", instructor_name, ": ", conditionMessage(e)))
            log_info("Suggestion: try rendering `quarto/template2.qmd` manually to see Quarto errors, or check template variables.")
        }
    )
}
log_info(paste0("All done. PDFs are in ", output_dir, "/ and csv moved into csvs/ (if downloaded)."))
## keep the log connection open until all rendering (including combined log) completes
## close(log_con) was here previously which caused attempts to write to a closed
## connection later when rendering the combined payroll log. We'll close once
## the combined-log rendering finishes below.

# Combined payroll-log rendering removed by request; rely on logs instead.
## Write a lightweight CSV summary and render a small PDF table from it.
summary_csv <- file.path("csvs", paste0("payroll-summary-", params$year, params$month, ".csv"))
tryCatch(
    {
        write.csv(services_data_summary, summary_csv, row.names = FALSE)
        log_info(paste0("Wrote summary CSV: ", summary_csv))
        # Use absolute path for Quarto rendering so the template (run from its
        # folder) can locate the CSV reliably.
        summary_csv_abs <- normalizePath(summary_csv, winslash = "/", mustWork = TRUE)
        # Render a simple PDF summary using the Quarto template
        summary_pdf <- paste0("payroll-summary-", params$year, params$month, ".pdf")
        tryCatch(
            {
                quarto::quarto_render(
                    input = file.path("quarto", "payroll_summary.qmd"),
                    output_file = summary_pdf,
                    execute_params = list(
                        summary_file = summary_csv_abs,
                        month = params$month,
                        year = params$year
                    ),
                    quarto_args = c("--output-dir", file.path("..", output_dir))
                )
                log_info(paste0("Wrote summary PDF: ", file.path(output_dir, summary_pdf)))
            },
            error = function(e) {
                log_warn(paste0("Failed to render summary PDF: ", conditionMessage(e)))
                log_info("You can render `quarto/payroll_summary.qmd` manually if desired.")
            }
        )
    },
    error = function(e) {
        log_warn(paste0("Failed to write summary CSV: ", conditionMessage(e)))
    }
)

## All done: close the log connection now that all messages have been written
close(log_con)
