#!/bin/zsh
# Simple wrapper: ./run_payroll.sh MONTH YEAR GOOGLE_EMAIL
if [ "$#" -lt 3 ]; then
  echo "Usage: $0 MONTH YEAR GOOGLE_EMAIL"
  echo "Example: $0 10 2025 andymaffei@gmail.com"
  exit 1
fi

MONTH=$1
YEAR=$2
EMAIL=$3

Rscript run_payroll.R --month=${MONTH} --year=${YEAR} --google_user_email=${EMAIL}
