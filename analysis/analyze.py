import pandas
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
import seaborn as sns
import sys
import os

def plot_on_pdf (filename, res, transf):
    res_1 = res[(res["transf"] == transf)]
    for depth in res_1["depth"].unique():
        res_2 = res_1[res_1["depth"] == depth]
        for fvc in res_2["fvc"].unique():
            res_3 = res_2[res_2["fvc"] == fvc]
            with PdfPages(filename.format(transf, depth, fvc)) as pdf:
                fig, ax = plt.subplots()
                add_res_orig = res_3[res_3["name"] == "orig"].drop(["transf", "name", "depth", "fvc"], axis=1).groupby(["inv_depth"]).mean().reset_index()
                add_res_inc  = res_3[res_3["name"] == "inc" ].drop(["transf", "name", "depth", "fvc"], axis=1).groupby(["inv_depth"]).mean().reset_index()

                add_res_orig["diff_sz"] = depth - add_res_orig["inv_depth"]
                add_res_inc["diff_sz"] = depth - add_res_inc["inv_depth"]

                add_res_orig = add_res_orig.drop(["inv_depth"], axis=1)
                add_res_inc = add_res_inc.drop(["inv_depth"], axis=1)

                # plt.title("transf = {}; depth = {}; fv_c = {}".format(transf, depth, fvc))
                plt.xticks(np.arange(min(add_res_orig["diff_sz"]) - 1, max(add_res_orig["diff_sz"]) + 1, 1.0))

                add_res_orig.plot(x="diff_sz", y="rate", ax=ax, label="std", marker='+', color='blue', linewidth=2, linestyle='dashed') # BLUE
                add_res_inc.plot(x="diff_sz", y="rate", ax=ax, label="inc", marker='o', color='orange', linewidth=2) # ORANGE
                
                pdf.savefig()
                plt.close()


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(("Usage: {} input_file.csv dest_path\n").format(sys.argv[0]))
    else:
        res = pandas.read_csv(sys.argv[1], sep=", ").drop(["repeat", "time"], axis=1)

        interesting = [(16, 1), (8, 128), (10, 512), (12, 2048), (14, 8192), (16, 32768)]
        # Just plot for "big" enough trees
        res = res[res["depth"] >= 8]

        # Create the directory
        os.makedirs("{}".format(sys.argv[2]), exist_ok=True)
        # Plot results for transf=id; connect by name; with x axis=depth; y axis=rate; 
        for fvc in res['fvc'].unique():
            with PdfPages('{}/res_id_{}.pdf'.format(sys.argv[2], fvc)) as pdf:
                fig, ax = plt.subplots()
                id_res_orig = res[(res["transf"] == "id") & (res["name"] == "orig") & (res["fvc"] == fvc)].drop(["transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean().reset_index()
                id_res_inc = res[(res["transf"] == "id") & (res["name"] == "inc") & (res["fvc"] == fvc)].drop(["transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean().reset_index()
                id_res_einc = res[(res["transf"] == "id") & (res["name"] == "einc") & (res["fvc"] == fvc)].drop(["transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean().reset_index()
                plt.title("transf = id; fv_c = {}".format(fvc))

                plt.xticks(np.arange(min(id_res_orig["depth"]) - 1, max(id_res_orig["depth"]) + 1, 1.0))
                #print(np.arange(min(id_res_orig["depth"]) - 1, max(id_res_orig["depth"]) + 1, 1.0))
                op = id_res_orig.plot(x="depth", y="rate", ax=ax, label="std", marker='+', color='blue', linewidth=2, linestyle='dashed') # BLUE
                ep = id_res_einc.plot(x="depth", y="rate", ax=ax, label="einc", marker='o', color='orange', linewidth=2) # ORANGE
                pdf.savefig()
                plt.close()
        
        plot_on_pdf("{}/".format(sys.argv[2]) + "res_{}_{}_{}.pdf", res, "mod")
        #plot_on_pdf("plot/{}/res_move.pdf".format(sys.argv[2]), res, "move")
        #plot_on_pdf("plot/{}/res_elim.pdf".format(sys.argv[2]), res, "elim")
