#!/usr/bin/env python3

import sqlalchemy as SQL
import numpy as NP
import pandas as PD
import atexit as AXT

host = SQL.engine.URL.create(
      drivername = "postgresql"
    , username = "falk"
    , host = "143.169.13.164"
    , database = "watinatje"
    , port = "2408"
)

watinatje_engine = SQL.create_engine(host)

def Disconnect():
    watinatje_engine.dispose()

AXT.register(Disconnect)


# with watinatje_engine.connect() as watinatje_conn:
#     data =


well_pairs = PD.read_sql_table(
    "well_pairs",
    con = watinatje_engine
)

print(well_pairs.head(3).T)


import matplotlib as MP
import matplotlib.pyplot as PLT

# data- and regression plot: distance
dpi = 300
cm = 1/2.54
PLT.rcParams.update({"text.usetex": True})
fig = PLT.figure(figsize = (24*cm, 16*cm), dpi = dpi)
fig.subplots_adjust(top = 0.96 , right = 0.98, bottom = 0.08, left = 0.08 )

ax = fig.add_subplot(1, 1, 1)

x = well_pairs["distance_m"].values
y = well_pairs["corr"].values
s = NP.sqrt(well_pairs["n"].values)
ax.scatter(x, y, s = s, marker = "o", facecolor = "w", edgecolor = "k", alpha = 0.2, linewidth = 0.5 \
           , label = "observations (\(size \sim \sqrt(N)\))" \
           )

dx = NP.linspace(0, 1000, 501, endpoint = True)
shift = 10 # NP.mean(NP.diff(dx))
dy = [NP.nanmean(y[NP.logical_and(x>=px, x<(px+shift))]) for px in dx]
dy[-2] += dy[-1]
dy = dy[:-1]
dx = dx[:-1]+shift/2

# print(list(zip(dx, dy)))
ax.plot(dx, dy, ls = "-", lw = 1, color = "k" \
        , label = "binned rolling average" \
        )
ax.set_xlim(0,1000)
ax.set_ylim(-1.,1.)
ax.set_xlabel("distance (m)")
ax.set_ylabel("correlation coefficient (\(R^2\))")
# ax.spines[["top", "right"]].set_visible(False)

ax.legend(loc = 4)
ax.set_title("Pairwise site correlation of water level measurements: effect of distance")

fig.savefig("figures/PeilMeting_correlation_distance.pdf")
PLT.show()


# data- and regression plot: ftd
dpi = 300
cm = 1/2.54
PLT.rcParams.update({"text.usetex": True})
fig = PLT.figure(figsize = (24*cm, 16*cm), dpi = dpi)
fig.subplots_adjust(top = 0.96 , right = 0.98, bottom = 0.08, left = 0.08 )

ax = fig.add_subplot(1, 1, 1)

x = well_pairs["d_ftd"].values
y = well_pairs["corr"].values
s = NP.sqrt(well_pairs["n"].values)
ax.scatter(x, y, s = s, marker = "o", facecolor = "w", edgecolor = "k", alpha = 0.2, linewidth = 0.5 \
           , label = "observations (\(size \sim \sqrt(N)\))" \
           )

dx = NP.linspace(0, 3, 65, endpoint = True)
shift = NP.mean(NP.diff(dx))
dy = [NP.nanmean(y[NP.logical_and(x>=px, x<(px+shift))]) for px in dx]
dy[-2] += dy[-1]
dy = dy[:-1]
dx = dx[:-1]+shift/2

ax.plot(dx, dy, ls = "-", lw = 1, color = "k" \
        , label = "binned average" \
        )
ax.set_xlim(0, 4)
ax.set_ylim(-1., 1.)
ax.set_xlabel("difference in filter top depth (m)")
ax.set_ylabel("correlation coefficient (\(R^2\))")
# ax.spines[["top", "right"]].set_visible(False)

ax.legend(loc = 4)
ax.set_title("Pairwise site correlation of water level measurements: effect of \(ftd\) difference")

fig.savefig("figures/PeilMeting_correlation_filtertopdepth.pdf")
PLT.show()
