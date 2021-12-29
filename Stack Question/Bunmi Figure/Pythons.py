import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

p=sns.swarmplot(x="Class", y="DS", data=dfx, size=9, palette="summer_r", hue="Conc")
sns.boxplot(showmeans=True,
            meanline=True,
            meanprops={'color': 'k', 'ls': '-', 'lw': 2},
            medianprops={'visible': False},
            whiskerprops={'visible': False},
            zorder=10,
            x="Class",
            y="DS",
            data=dfx,
            showfliers=False,
            showbox=False,
            showcaps=False,
            ax=p)
plt.show()
