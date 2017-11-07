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

def loadtxt_rows(filename, rows, single_precision=False):
    """
    Load only certain rows
    """
    # Open the file
    f = open(filename, "r")

    # Storage
    results = {}

    # Row number
    i = 0

    # Number of columns
    ncol = None

    while(True):
        # Read the line and split by commas
        line = f.readline()
        cells = line.split(",")

        # Quit when you see a different number of columns
        if ncol is not None and len(cells) != ncol:
            break

        # Non-comment lines
        if cells[0] != "#":
            # If it's the first one, get the number of columns
            if ncol is None:
                ncol = len(cells)

            # Otherwise, include in results
            if i in rows:
                if single_precision:
                    results[i] = np.array([float(cell) for cell in cells],\
                                                              dtype="float32")
                else:
                    results[i] = np.array([float(cell) for cell in cells])
            i += 1

    results["ncol"] = ncol
    return results

def postprocess(single_precision=False, temperature=1.0):

    # Load the files
    #sample      = pd.read_csv("nested_sampling_parameters.csv", header=None)
    sample_info = pd.read_csv("nested_sampling_info.csv")

    # In case one is shorter than the other, truncate.
    #L = min(sample.shape[0], sample_info.shape[0])
    #sample      = sample.iloc[0:L, :]
    #sample_info = sample_info.iloc[0:L, :]

    # Normalise prior weights
    logw = sample_info["ln_prior_weight"]
    logw = logw - logsumexp(logw)

    # Calculate logZ, posterior weights, and H
    logZ = logsumexp(logw + sample_info["ln_l"])
    logW = logw + sample_info["ln_l"]/temperature - logZ
    W = np.exp(logW - logsumexp(logW))

    # Save posterior weights
    np.savetxt("posterior_weights.txt", W)

    # Create posterior samples
    W /= W.max()
    Wnormed = W/W.sum()
    ESS = int(np.exp(-np.sum(Wnormed*np.log(Wnormed + 1E-300))))
    print("Effective sample size = {ESS}".format(ESS=ESS))

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


    rows = np.empty(ESS, dtype="int64")
    for i in range(0, ESS):
        while True:
            which = np.random.randint(sample_info.shape[0])
            if np.random.rand() <= W[which]:
                break
        rows[i] = which



    sample = loadtxt_rows("nested_sampling_parameters.csv",
                          set(rows), single_precision)
    posterior_sample = None
    if single_precision:
        posterior_sample = np.empty((ESS, sample["ncol"]), dtype="float32")
    else:
        posterior_sample = np.empty((ESS, sample["ncol"]))

    for i in range(0, ESS):
        posterior_sample[i, :] = sample[rows[i]]


    np.savetxt("posterior_weights.txt", W)
    if single_precision:
        np.savetxt("posterior_sample.csv",
                   posterior_sample, delimiter=",", fmt="%.7e")
    else:
        np.savetxt("posterior_sample.csv", posterior_sample, delimiter=",")


if __name__ == "__main__":
    postprocess()

