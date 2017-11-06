import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

n = np.array([1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000])
m = np.array([100000, 50000, 20000, 10000, 5000, 2000, 1000, 500, 200, 100, 50, 20])*5

rms_error = []
for i in range(0, len(n)):
    try:
        filename = "outputs/" + str(n[i]) + "_" + str(m[i]) + ".csv"
        x = pd.read_csv(filename)
        rms_error.append(np.sqrt(np.mean((x.iloc[:,0] + 286)**2)))
    except:
        pass

n = n[0:len(rms_error)]
m = m[0:len(rms_error)]

plt.loglog(n, rms_error, "o-", label="Observed")
plt.loglog(n, np.sqrt(260.0/n), "o-", alpha=0.25, label="Theoretical")
plt.xlabel("Number of particles")
plt.ylabel("RMS $\\log(Z)$ Error")
plt.legend(loc="upper right")
plt.show()

