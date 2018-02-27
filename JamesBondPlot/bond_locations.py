from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import pandas as pd

fig = plt.figure(figsize=(11.69, 8.27))

m = Basemap(projection='merc',llcrnrlat=-60,urcrnrlat=80, llcrnrlon=-180,urcrnrlon=180,lat_ts=20,resolution='c')
m.fillcontinents(color='#d9d9d9', zorder=0)

df = pd.read_excel(io=r"C:\Users\Richard\Documents\Blog\Data_Team_Blog_Posts\JamesBondPlot\Bond_draft.xlsx", sheetname="Country Count")

x1, y1 = m(df['Lon'].values, df['Lat'].values)
m.scatter(x1, y1, s=df['NumFilms'] * 20, c="b", marker="o", alpha=0.5)

labels = df['Label'].values
xyt = {0: (-20, 20), 1: (-20, 20), 2: (-15, -30), 3: (20, 20), 4: (20, 20),
       5: (20, 20), 6: (40, 20), 7: (-40, 10), 8: (25, 22), 9: (40, 10),
      10: (20, -20), 11: (-45, -7), 12: (-50, 20), 13: (-35, 12), 14: (15, 30),
      15: (30, 25), 16: (30, 0), 17: (-20, 20), 18: (-40, 0), 19: (0, -55),
      20: (15, -40), 21: (20, -20), 22: (-20, -20), 23: (20, -20), 24: (23, 5),
      25: (20, 20), 26: (-20, 20), 27: (20, 20), 28: (20, -20), 29: (-20, -20),
      30: (-20, -20), 31: (30, 0), 32: (15, 10), 33: (20, -20), 34: (20, -20),
      35: (30, -10), 36: (-20, -20), 37: (40, 15), 38: (-22, -50), 39: (-20, 20),
      40: (-40, -7), 41: (0, 40), 42: (-25, -42)}

for i, (label, x, y, pos) in enumerate(zip(labels, x1, y1, xyt)):
    plt.annotate(
        label, 
        xy = (x, y), xytext = xyt[i], size=6, textcoords = 'offset points', ha = 'center', va = 'center',
        arrowprops = dict(arrowstyle = '-', connectionstyle = 'arc3,rad=0'))
plt.title("Countries by Number of James Bond Film Appearances")
plt.savefig(r"C:\Users\Richard\Documents\Blog\Data_Team_Blog_Posts\JamesBondPlot\locations.jpeg", bbox_inches='tight')
