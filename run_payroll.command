#!/bin/zsh
# Double-clickable command wrapper for macOS — runs interactively.
DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$DIR"
# Run the R script interactively so prompts appear
Rscript run_payroll.R
