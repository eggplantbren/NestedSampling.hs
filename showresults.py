# A small Python script to load the text files
# output by the Haskell code, and compute log(Z) and H,
# as well as create some plots.

# Imports
import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng
import pandas as pd

# Logsumexp function
def logsumexp(x):
    biggest = x.max()
    y = x - biggest
    tot = np.sum(np.exp(y))
    return np.log(tot) + biggest

# Load the files
sample      = pd.read_csv("nested_sampling_parameters.csv", header=None)
sample_info = pd.read_csv("nested_sampling_info.csv")

# In case one is shorter than the other, truncate.
L = min(sample.shape[0], sample_info.shape[0])
sample      = sample.iloc[0:L, :]
sample_info = sample_info.iloc[0:L, :]

# Normalise prior weights
logw = sample_info["ln_prior_weight"]
logw = logw - logsumexp(logw)

# Calculate logZ, posterior weights, and H
logZ = logsumexp(logw + sample_info["ln_l"])
logW = logw + sample_info["ln_l"] - logZ
W = np.exp(logW - logsumexp(logW))

# Save posterior weights
np.savetxt("posterior_weights.txt", W)

# Create posterior samples
W /= W.max()
Wnormed = W/W.sum()
ESS = int(np.exp(-np.sum(Wnormed*np.log(Wnormed + 1E-300))))
print("Effective sample size = {ESS}".format(ESS=ESS))
posterior_sample = np.empty((ESS, sample.shape[1]))
for i in range(0, ESS):
    while True:
        k = rng.randint(sample.shape[0])
        if rng.rand() <= W[k]:
            break
    posterior_sample[i, :] = sample.iloc[k, :]

np.savetxt("posterior_sample.txt", posterior_sample)
    

# Make the standard NS plots
plt.figure(figsize=(10, 6))
plt.subplot(2,1,1)
plt.plot(sample_info["ln_x"], sample_info["ln_l"], "-", markersize=3)
plt.xlabel("$\\ln(X)$")
plt.ylabel("$\\ln(L)$")
plt.title("Likelihood curve")

# Set y lower limit by excluding bottom 5%
ln_l_sorted = np.sort(sample_info["ln_l"])
lower_limit = ln_l_sorted[int(0.05*len(ln_l_sorted))]
plt.ylim(lower_limit)

plt.subplot(2,1,2)
plt.plot(sample_info["ln_x"], W, "-", markersize=3)
plt.xlabel("$\\ln(X)$")
plt.ylabel("$W$")
plt.title("Posterior weights")

# Format and display figures
plt.tight_layout(h_pad=1.0)
plt.show()

