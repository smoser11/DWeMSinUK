#!/usr/bin/env bash
# 06e-run-all.sh
# Minimal appendix-only Bayesian MA run.
# Created 2026-06-11 as a successor to 06c-run-all.sh.
#
# Rationale: the full 96-combination sensitivity loop in 06c- is
# computationally infeasible (single MA call at N=980k = days; at
# N=100k = overnight). Per the analysis in correspondence with
# Caroline, MA prevalence estimates are functionally invariant to N
# for n/N <<1, so we run MA only at N=50,000 and scale for count
# reporting. Seed-selection sensitivity at the converged setting
# shows the model is insensitive to seed.selection (different chain
# bytes, identical extracted estimates), so we run the headline
# results with seed.selection="degree" only and include a small
# seed-check (one indicator across 3 methods) as supporting evidence.
#
# Total: 8 main calls + 2 extra seed-check calls = 10 MA.estimates
# invocations. At ~2 hours per call at N=50k, runs overnight.
#
# Each call delegates to 06c-run-single.R (which exits between calls
# so memory is reclaimed by the OS). Resumable - skips .RData files
# already in output/.
#
# Usage:
#   bash R/analysis/06e-run-all.sh
#
# Old script kept at R/analysis/06c-run-all.sh for reference.

set -u
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$( cd "$SCRIPT_DIR/../.." && pwd )"
cd "$REPO_ROOT"

# Fixed population size for all MA calls (justified in appendix methods).
POPSIZE=50000

# Headline run: all 8 indicators with seed.selection="degree"
# sum_categories was dropped 2026-06-17 (duplicate of composite_risk in raw CSV).
HEADLINE_INDICATORS=(
  document_withholding_rds
  pay_issues_rds
  threats_abuse_rds
  excessive_hours_rds
  access_to_help_rds
  whether_exploitation
  composite_risk
)

# Seed-selection sensitivity: only for one representative indicator.
SEED_CHECK_INDICATOR=document_withholding_rds
SEED_CHECK_METHODS=(random sample)   # "degree" is already covered in headline run

LOG_DIR="$REPO_ROOT/output/logs"
mkdir -p "$LOG_DIR"
RUN_LOG="$LOG_DIR/06e-run-all_$(date +%Y%m%d_%H%M%S).log"
echo "Logging to: $RUN_LOG"

TOTAL=0; SKIPPED=0; SUCCEEDED=0; FAILED=0
START_TIME=$(date +%s)

run_one() {
  local ind="$1" pop="$2" seed="$3"
  TOTAL=$((TOTAL + 1))
  OUTFILE="output/ma_sensitivity_${ind}_${pop}_${seed}.RData"
  if [ -f "$OUTFILE" ]; then
    echo "[$(date +%H:%M:%S)] SKIP $ind / N=$pop / $seed (exists)" | tee -a "$RUN_LOG"
    SKIPPED=$((SKIPPED + 1))
    return
  fi
  echo "[$(date +%H:%M:%S)] RUN  $ind / N=$pop / $seed" | tee -a "$RUN_LOG"
  if Rscript R/analysis/06-run-single.R "$ind" "$pop" "$seed" >> "$RUN_LOG" 2>&1; then
    SUCCEEDED=$((SUCCEEDED + 1))
    echo "[$(date +%H:%M:%S)]   ok" | tee -a "$RUN_LOG"
  else
    FAILED=$((FAILED + 1))
    echo "[$(date +%H:%M:%S)]   FAILED - see log" | tee -a "$RUN_LOG"
  fi
}

# Headline: 8 indicators @ N=50k, seed=degree
echo "=== Headline run (8 indicators, N=$POPSIZE, seed=degree) ==="
for ind in "${HEADLINE_INDICATORS[@]}"; do
  run_one "$ind" "$POPSIZE" "degree"
done

# Seed-selection check: 1 indicator @ N=50k, seed in (random, sample)
echo ""
echo "=== Seed-selection check (indicator=$SEED_CHECK_INDICATOR) ==="
for seed in "${SEED_CHECK_METHODS[@]}"; do
  run_one "$SEED_CHECK_INDICATOR" "$POPSIZE" "$seed"
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
echo "Next step: source('R/analysis/06e-bayesian_appendix.R') in R"
echo "to consolidate the individual .RData files into the appendix summary."
