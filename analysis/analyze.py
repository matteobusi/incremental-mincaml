import pandas
import matplotlib.pyplot as plt
import numpy as np
plt.style.use('seaborn-whitegrid')
from matplotlib.backends.backend_pdf import PdfPages


def plot_on_pdf (filename, res, transf):
    res_1 = res[(res["transf"] == transf)]
    with PdfPages(filename) as pdf:
        for fvc in res_1['fvc'].unique():
            res_2 = res_1[res_1["fvc"] == fvc]
            for inv_depth in res_2["inv_depth"].unique():
                res_3 = res_2[res_2["inv_depth"] == inv_depth]
                fig, ax = plt.subplots()
                add_res_orig = res_3[res_3["name"] == "orig"].drop(["transf", "name", "fvc", "inv_depth"], axis=1).groupby(["depth"]).mean()
                add_res_inc = res_3[res_3["name"] == "inc"].drop(["transf", "name", "fvc", "inv_depth"], axis=1).groupby(["depth"]).mean()

                plt.title("transf = {}; fv_c = {}; inv_depth = {}".format(transf, fvc, inv_depth))
            
                add_res_orig.plot(y="rate", ax=ax, label="orig", xticks=add_res_orig.index)
                add_res_inc.plot(y="rate", ax=ax, label="inc")
                pdf.savefig()
                plt.close()

res = pandas.read_csv("tmp.csv", sep=", ").drop(["repeat", "time"], axis=1)

# Just plot for "big" trees
res = res[res["depth"] >= 12]

# Plot results for transf=id; connect by name; with x axis=depth; y axis=rate; 
with PdfPages('res_id.pdf') as pdf:
    for fvc in res['fvc'].unique():
        fig, ax = plt.subplots()
        id_res_orig = res[(res["transf"] == "id") & (res["name"] == "orig") & (res["fvc"] == fvc)].drop(["transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean()
        id_res_inc = res[(res["transf"] == "id") & (res["name"] == "inc") & (res["fvc"] == fvc)].drop(["transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean()
        id_res_einc = res[(res["transf"] == "id") & (res["name"] == "einc") & (res["fvc"] == fvc)].drop(["transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean()
        plt.title("transf = id; fv_c = {}".format(fvc))
    
        op = id_res_orig.plot(y="rate", ax=ax, label="orig", xticks=id_res_orig.index) # BLUE
        # ip = id_res_inc.plot(y="rate", ax=ax, label="inc")
        ep = id_res_einc.plot(y="rate", ax=ax, label="einc") # ORANGE
        pdf.savefig()
        plt.close()

plot_on_pdf("res_add.pdf", res, "add")
plot_on_pdf("res_move.pdf", res, "move")
plot_on_pdf("res_elim.pdf", res, "elim")
