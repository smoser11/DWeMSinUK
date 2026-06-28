#!/usr/bin/env bash
# 06c-run-all.sh
# Run the 96-combination MA.estimates sensitivity loop via session restarts.
# Created 2026-06-11.
#
# Each combination runs in a fresh Rscript process - R exits between calls
# so memory is reclaimed by the OS. Safe to interrupt and resume; existing
# .RData files are skipped.
#
# Usage:
#   bash R/analysis/06c-run-all.sh                     # all 96 combinations
#   bash R/analysis/06c-run-all.sh document_withholding_rds   # one indicator (all popsizes/seeds)
#
# Run from the repo root.

set -u
# Find repo root (the directory containing this file's R/analysis/ ancestor)
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$( cd "$SCRIPT_DIR/../.." && pwd )"
cd "$REPO_ROOT"

# All combinations: 8 indicators x 4 population sizes x 3 seed selection methods = 96
# sum_categories was dropped 2026-06-17 (duplicate of composite_risk in raw CSV).
INDICATORS=(
  document_withholding_rds
  pay_issues_rds
  threats_abuse_rds
  excessive_hours_rds
  access_to_help_rds
  whether_exploitation
  composite_risk
)
POPSIZES=(50000 100000 980000 1740000)
SEEDS=(degree random sample)

# Optional first-arg filter: if given, only run combinations for that indicator
FILTER_INDICATOR="${1:-}"

LOG_DIR="$REPO_ROOT/output/logs"
mkdir -p "$LOG_DIR"
RUN_LOG="$LOG_DIR/06c-run-all_$(date +%Y%m%d_%H%M%S).log"
echo "Logging to: $RUN_LOG"

TOTAL=0; SKIPPED=0; SUCCEEDED=0; FAILED=0
START_TIME=$(date +%s)

for ind in "${INDICATORS[@]}"; do
  if [ -n "$FILTER_INDICATOR" ] && [ "$ind" != "$FILTER_INDICATOR" ]; then
    continue
  fi
  for pop in "${POPSIZES[@]}"; do
    for seed in "${SEEDS[@]}"; do
      TOTAL=$((TOTAL + 1))
      OUTFILE="output/ma_sensitivity_${ind}_${pop}_${seed}.RData"

      if [ -f "$OUTFILE" ]; then
        echo "[$(date +%H:%M:%S)] SKIP $ind / N=$pop / $seed (exists)" | tee -a "$RUN_LOG"
        SKIPPED=$((SKIPPED + 1))
        continue
      fi

      echo "[$(date +%H:%M:%S)] RUN  $ind / N=$pop / $seed" | tee -a "$RUN_LOG"
      if Rscript R/analysis/06c-run-single.R "$ind" "$pop" "$seed" >> "$RUN_LOG" 2>&1; then
        SUCCEEDED=$((SUCCEEDED + 1))
        echo "[$(date +%H:%M:%S)]   ok" | tee -a "$RUN_LOG"
      else
        FAILED=$((FAILED + 1))
        echo "[$(date +%H:%M:%S)]   FAILED - see log" | tee -a "$RUN_LOG"
      fi
    done
  done
done

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))
ELAPSED_MIN=$((ELAPSED / 60))

echo ""
echo "=========================================="
echo "Summary"
echo "=========================================="
echo "Total considered: $TOTAL"
echo "Skipped (already done): $SKIPPED"
echo "Succeeded: $SUCCEEDED"
echo "Failed: $FAILED"
echo "Elapsed: ${ELAPSED_MIN} minutes"
echo "Log: $RUN_LOG"
echo ""
echo "Next step: source('R/analysis/06d-consolidate_existing_results.R') in R"
echo "to consolidate the individual .RData files into the summary tables."
