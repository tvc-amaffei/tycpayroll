# TYC Payroll Runner

This repository contains a lightweight CLI to reproduce the outputs from `tyc-instructor-payroll-2.qmd` (per-instructor PDFs and a payroll summary).

Quick overview
- `run_payroll.R`: main R script that fetches the payroll CSV and supporting sheets from Google Drive (or runs from local CSV), computes pay, and renders per-instructor PDFs using `quarto/template2.qmd`.
- `run_payroll.sh`: simple shell wrapper that calls the R script with `MONTH YEAR EMAIL` arguments.
- `run_payroll.command`: double-clickable macOS wrapper (runs the R script interactively).
- `instructor_pdfs/`: output directory containing generated PDFs.

Prerequisites
- R (compatible version, e.g. 4.2+)
- Quarto CLI installed: https://quarto.org
- Required R packages: `tidyverse`, `googledrive`, `googlesheets4`, `jsonlite`, `quarto`, `readr`.

Install R packages (one-time):
```r
install.packages(c("tidyverse","googledrive","googlesheets4","jsonlite","readr"))
install.packages("quarto") # optional; Quarto CLI is required
```

Usage

1) Interactive (recommended for non-technical users)
 - Double-click `run_payroll.command` in Finder (first make it executable with `chmod +x run_payroll.command`).
 - The script will prompt for month, year, and Google account email (used for `googledrive` / `googlesheets4` auth). Follow the prompts and sign into Google when your browser opens.

2) Wrapper script
```bash
./run_payroll.sh 10 2025 your_google_email@example.com
```

3) Direct R invocation
```bash
Rscript run_payroll.R --month=10 --year=2025 --google_user_email=you@example.com
```

Local mode
- If you want to run without Google Drive access, place the payroll CSV at `csvs/tyc_wlpayroll-<YEAR><MONTH>.csv` and supporting CSVs under `extdata/` (see `run_payroll.R` for fallback filenames) and run with `--use_local=true`.

Cleaning artifacts
- The script will write final PDFs to `instructor_pdfs/`. If you see older artifacts under `quarto/`, it's safe to remove them:
```bash
rm -f quarto/*.pdf
rm -rf quarto/instructor_pdfs
```

Next steps
- I can create a packaged macOS App (Automator or Script Editor compiled app) that runs the `run_payroll.command` for a one-click experience. Ask me to scaffold that if you want.

If anything fails during a run, paste the terminal output (or open an issue) and I'll help debug.
