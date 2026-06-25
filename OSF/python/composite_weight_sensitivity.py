"""Composite-risk weight-perturbation sensitivity analysis used in main paper
Section 5.5. Vary each of the 11 sub-indicator weights by uniform [-50%, +50%]
random factor; recompute composite per respondent; compute Spearman rho
between perturbed and baseline rankings. Repeat B = 5,000 times.

Inputs:  data_full_anon.csv  (in ../data/)
Outputs: composite_risk_weight_sensitivity.json  (in ../output/tables/)
"""
import pandas as pd, numpy as np, json, os
from scipy.stats import spearmanr

np.random.seed(20260622)
df = pd.read_csv("../data/processed/data_full.csv")
cats = [f'category_{i}' for i in range(1, 12)]  # composite uses 1..11
baseline = df['composite_risk'].values
vals = df[cats].values
n, K = vals.shape
baseline_q75 = np.quantile(baseline, 0.75)
baseline_top = baseline >= baseline_q75

def perturb_and_score(scale):
    spearman, preserved = np.zeros(5000), np.zeros(5000)
    for b in range(5000):
        p = np.random.uniform(1-scale, 1+scale, size=K)
        s = (vals * p).sum(axis=1)
        spearman[b], _ = spearmanr(baseline, s)
        pq75 = np.quantile(s, 0.75)
        preserved[b] = (baseline_top & (s >= pq75)).sum() / baseline_top.sum()
    return spearman, preserved

sp50, pr50 = perturb_and_score(0.50)
sp75, pr75 = perturb_and_score(0.75)
summary = {
    "n_perturbations": 5000,
    "perturbation_pm50": {
        "spearman_rho_mean": float(sp50.mean()),
        "spearman_rho_ci": [float(np.quantile(sp50, 0.025)), float(np.quantile(sp50, 0.975))],
        "spearman_rho_min": float(sp50.min()),
        "top_quartile_preservation_mean_pct": float(pr50.mean()*100),
    },
    "perturbation_pm75": {
        "spearman_rho_mean": float(sp75.mean()),
        "spearman_rho_min": float(sp75.min()),
        "top_quartile_preservation_mean_pct": float(pr75.mean()*100),
    },
}
os.makedirs("../output/tables", exist_ok=True)
with open("../output/tables/composite_risk_weight_sensitivity.json", "w") as f:
    json.dump(summary, f, indent=2)
print(f"Spearman rho (mean / worst @ +/-50%): {sp50.mean():.4f} / {sp50.min():.4f}")
print(f"Saved: ../output/tables/composite_risk_weight_sensitivity.json")
