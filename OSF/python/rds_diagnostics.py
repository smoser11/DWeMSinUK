"""Generate the RDS sampling diagnostics figure used in main paper Section 4.1.1.

Inputs:  data_nonzero_anon.csv  (in ../data/)
Outputs: rds_diagnostics.png    (in ../output/figures/)

Reproduces the 3-panel diagnostic figure: wave-depth distribution, recruitment
chain length distribution, and degree distribution. Also computes the
diagnostic numbers reported in Table 4 (n_seeds, n_recruits, active chains,
max chain length, max wave, etc.).
"""
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import os

df = pd.read_csv("../data/data_nonzero_anon.csv")
print(f"Loaded {len(df)} respondents")

# Build parent map and compute wave + chain root
id_to_recruiter = dict(zip(df['id'], df['recruiter.id']))
def compute_wave(rid):
    current = id_to_recruiter.get(rid, -1); wave = 0; visited = set()
    while current != -1 and current not in visited:
        visited.add(current); wave += 1
        current = id_to_recruiter.get(current, -1)
    return wave
def find_root(rid):
    current = rid; visited = set()
    while current != -1 and current in id_to_recruiter and current not in visited:
        visited.add(current)
        if id_to_recruiter[current] == -1: return current
        current = id_to_recruiter[current]
    return current
df['wave'] = df['id'].apply(compute_wave)
df['root'] = df['id'].apply(find_root)
chains = df.groupby('root').size().values

# Print diagnostics (matches main paper Table 4)
print(f"Seeds: {(df['recruiter.id'] == -1).sum()}")
print(f"Recruits: {(df['recruiter.id'] != -1).sum()}")
print(f"Active chains (length >= 2): {sum(c >= 2 for c in chains)}")
print(f"Singleton seeds: {sum(c == 1 for c in chains)}")
print(f"Max chain length: {max(chains)}")
print(f"Max wave: {df['wave'].max()}")
print(f"Wave >= 3 respondents: {(df['wave'] >= 3).sum()}")
print(f"Median q13 (network size): {df['q13'].median()}")
print(f"Mean q13: {df['q13'].mean():.1f}")

# Build the 3-panel figure
fig, axes = plt.subplots(1, 3, figsize=(13, 4))
wc = df['wave'].value_counts().sort_index()
axes[0].bar(wc.index, wc.values, color='#4682B4', edgecolor='black')
axes[0].set_xlabel('Recruitment wave (0 = seed)'); axes[0].set_ylabel('Number of respondents')
axes[0].set_title('Wave depth distribution')
for x, y in zip(wc.index, wc.values): axes[0].text(x, y + 1, str(y), ha='center')

cc = pd.Series(chains).value_counts().sort_index()
axes[1].bar(cc.index, cc.values, color='#CD5C5C', edgecolor='black')
axes[1].set_xlabel('Chain length (respondents per root seed)'); axes[1].set_ylabel('Number of chains')
axes[1].set_title('Recruitment chain length distribution')
for x, y in zip(cc.index, cc.values): axes[1].text(x, y + 0.5, str(y), ha='center')

axes[2].hist(df['q13'], bins=20, color='#9370DB', edgecolor='black')
axes[2].set_xlabel('Reported network size (q13)'); axes[2].set_ylabel('Number of respondents')
axes[2].set_title(f"Degree distribution (mean={df['q13'].mean():.1f}, median={df['q13'].median():.0f})")
axes[2].set_xlim(0, df['q13'].quantile(0.99))

plt.tight_layout()
os.makedirs("../output/figures", exist_ok=True)
plt.savefig("../output/figures/rds_diagnostics.png", dpi=150, bbox_inches='tight')
print("Saved: ../output/figures/rds_diagnostics.png")
