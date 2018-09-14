import pandas
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
import seaborn as sns

def plot_on_pdf (filename, res, transf):
    res_1 = res[(res["transf"] == transf)]
    with PdfPages(filename) as pdf:
        for depth in res_1["depth"].unique():
            res_2 = res_1[res_1["depth"] == depth]
            for fvc in res_2["fvc"].unique():
                res_3 = res_2[res_2["fvc"] == fvc]
                for slow_prob in res_3["slow_prob"].unique():
                    res_4 = res_3[res_3["slow_prob"] == slow_prob]
                    fig, ax = plt.subplots()
                    add_res_orig = res_4[res_4["name"] == "orig"].drop(["slow_prob", "transf", "name", "depth", "fvc"], axis=1).groupby(["inv_depth"]).mean()
                    add_res_inc = res_4[res_4["name"] == "inc"].drop(["slow_prob", "transf", "name", "depth", "fvc"], axis=1).groupby(["inv_depth"]).mean()

                    plt.title("slow_prob = {}; transf = {}; depth = {}; fv_c = {}".format(slow_prob, transf, depth, fvc))
                
                    add_res_orig.plot(y="rate", ax=ax, label="orig", marker='+', color='blue', linewidth=2, linestyle='dashed') # BLUE
                    add_res_inc.plot(y="rate", ax=ax, label="inc", marker='o', color='orange', linewidth=2) # ORANGE
                    pdf.savefig()
                    plt.close()

res = pandas.read_csv("results.csv", sep=", ").drop(["repeat", "time"], axis=1)

# Just plot for "big" trees
res = res[res["depth"] >= 12]

# Plot results for transf=id; connect by name; with x axis=depth; y axis=rate; 
with PdfPages('res_id.pdf') as pdf:
    for fvc in res['fvc'].unique():
        for slow_prob in res["slow_prob"].unique():
            fig, ax = plt.subplots()
            id_res_orig = res[(res["slow_prob"] == slow_prob) & (res["transf"] == "id") & (res["name"] == "orig") & (res["fvc"] == fvc)].drop(["slow_prob", "transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean()
            id_res_inc = res[(res["slow_prob"] == slow_prob) & (res["transf"] == "id") & (res["name"] == "inc") & (res["fvc"] == fvc)].drop(["slow_prob", "transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean()
            id_res_einc = res[(res["slow_prob"] == slow_prob) & (res["transf"] == "id") & (res["name"] == "einc") & (res["fvc"] == fvc)].drop(["slow_prob", "transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean()
            plt.title("slow_prob = {}; transf = id; fv_c = {}".format(slow_prob, fvc))
        
            op = id_res_orig.plot(y="rate", ax=ax, label="orig", marker='+', color='blue', linewidth=2, linestyle='dashed') # BLUE
            ep = id_res_einc.plot(y="rate", ax=ax, label="einc", marker='o', color='orange', linewidth=2) # ORANGE
            pdf.savefig()
            plt.close()

plot_on_pdf("res_add.pdf", res, "add")
plot_on_pdf("res_move.pdf", res, "move")
plot_on_pdf("res_elim.pdf", res, "elim")
