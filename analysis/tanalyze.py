import pandas
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib import rcParams
import seaborn as sns
import sys
import os

## Names of the experiments, taken from ocaml source
orig_n = "orig"
einc_n = "einc"
finc_n = "finc"
setupinc_n = "setup+inc"
setup_n = "setup"
inc_n = "inc"
##

font = {'family' : 'serif',
        'weight' : 'bold',
        'size'   : 15}

plt.rc('text', usetex=True)
plt.rc('font', **font)
rcParams.update({'figure.autolayout': True})

def tabulate(res, interesting_pairs):
    for (depth, fvc) in interesting_pairs:
        id_res_orig = res[(res["name"].str.startswith(orig_n)) & (res["depth"] == depth) & (res["fvc"] == fvc)].drop(["inv_depth", "name", "fvc"], axis=1).groupby("depth").mean().reset_index()
        id_res_einc = res[(res["name"].str.startswith(einc_n)) & (res["depth"] == depth) & (res["fvc"] == fvc)].drop(["inv_depth", "name", "fvc"], axis=1).groupby("depth").mean().reset_index()

        if len(id_res_orig.index) == 1:
            orig_r, einc_r = id_res_orig.iloc[0]["rate"], id_res_einc.iloc[0]["rate"]
            ratio = einc_r / orig_r
            if fvc.bit_length() == 1:
                print("${}$ & $1$ & ${:.2f}$ & ${:.2f}$ & ${:.2f}$\\\\".format(depth, orig_r, einc_r, ratio))
            else:
                print("${}$ & $2^{{{}}}$ & ${:.2f}$ & ${:.2f}$ & ${:.2f}$\\\\".format(depth, fvc.bit_length() - 1, orig_r, einc_r, ratio))

def plot_on_pdf (filename, res):
    for depth in res["depth"].unique():
        res_2 = res[res["depth"] == depth]
        for fvc in res_2["fvc"].unique():
            res_3 = res_2[res_2["fvc"] == fvc]
            with PdfPages(filename.format(depth, fvc)) as pdf:
                fig, ax = plt.subplots()
                add_res_orig = res_3[res_3["name"].str.startswith(orig_n)].drop(["name", "depth", "fvc"], axis=1).groupby(["inv_depth"]).mean().reset_index()
                add_res_inc  = res_3[res_3["name"].str.startswith(inc_n)].drop(["name", "depth", "fvc"], axis=1).groupby(["inv_depth"]).mean().reset_index()

                add_res_orig["diffsz"] = depth - add_res_orig["inv_depth"]
                add_res_inc["diffsz"] = depth - add_res_inc["inv_depth"]

                add_res_orig = add_res_orig.drop(["inv_depth"], axis=1)
                add_res_inc = add_res_inc.drop(["inv_depth"], axis=1)

                # Repeat the row
                # rate = add_res_orig["rate"].values[0]
                # add_res_orig = pandas.DataFrame()
                # for dsz in add_res_inc.drop(["rate"], axis=1).values:
                #     add_res_orig = add_res_orig.append({"diffsz":int(dsz[0]), "rate":rate}, ignore_index=True)

                # plt.title("transf = {}; depth = {}; fv_c = {}".format(transf, depth, fvc))
                add_res_orig.plot(x="diffsz", y="rate", ax=ax, label=orig_n, marker='+', color='blue', linewidth=2, linestyle='dashed') # BLUE
                add_res_inc.plot(x="diffsz", y="rate", ax=ax, label=inc_n, marker='o', color='orange', linewidth=2) # ORANGE

                plt.xticks(np.arange(min(add_res_orig["diffsz"]), max(add_res_orig["diffsz"]) + 1, 1.0))
                # if depth == 12 and (fvc==512 or fvc==2048 or fvc==1024):
                #     plt.yticks(np.arange(0, 8000, 1000))
                plt.xlabel("$\log_2 (\mathit{\# nodes\_diff} + 1)$")
                plt.ylabel("re-typings/$s$")

                pdf.savefig()
                plt.close()


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(("Usage: {} input_file.csv dest_path\n").format(sys.argv[0]))
    else:
        res = pandas.read_csv(sys.argv[1], sep=", ", engine="python", dtype={'fvc':int, 'depth':int, 'inv_depth':int, 'rate':float})
        #.drop(["repeat", "time"], axis=1)

        # Just plot for "big" enough trees
        res = res[res["depth"] >= 8]

        # Create the directory
        os.makedirs("{}".format(sys.argv[2]), exist_ok=True)
        # # Plot results for transf=id; connect by name; with x axis=depth; y axis=rate;
        # for fvc in res['fvc'].unique():
        #     with PdfPages('{}/res_id_{}.pdf'.format(sys.argv[2], fvc)) as pdf:
        #         fig, ax = plt.subplots()
        #         id_res_orig = res[(res["transf"] == "id") & (res["name"] == "orig") & (res["fvc"] == fvc)].drop(["transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean().reset_index()
        #         id_res_inc = res[(res["transf"] == "id") & (res["name"] == "inc") & (res["fvc"] == fvc)].drop(["transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean().reset_index()
        #         id_res_einc = res[(res["transf"] == "id") & (res["name"] == "einc") & (res["fvc"] == fvc)].drop(["transf", "inv_depth", "name", "fvc"], axis=1).groupby("depth").mean().reset_index()
        #         #plt.title("transf = id; fv_c = {}".format(fvc))

        #         plt.xticks(np.arange(min(id_res_orig["depth"]) - 1, max(id_res_orig["depth"]) + 1, 1.0))
        #         #print(np.arange(min(id_res_orig["depth"]) - 1, max(id_res_orig["depth"]) + 1, 1.0))
        #         op = id_res_orig.plot(x="depth", y="rate", ax=ax, label="std", marker='+', color='blue', linewidth=2, linestyle='dashed') # BLUE
        #         ep = id_res_einc.plot(x="depth", y="rate", ax=ax, label="einc", marker='o', color='orange', linewidth=2) # ORANGE
        #         pdf.savefig()
        #         plt.close()

        tabulate(res, [(16,1), (16, 2**7), (16, 2**9), (16, 2**11), (16, 2**13), (16, 2**15)])
        tabulate(res, [(14,1), (14, 2**7), (14, 2**9), (14, 2**11), (14, 2**13)])
        tabulate(res, [(12,1), (12, 2**7), (12, 2**9), (12, 2**11)])

        plot_on_pdf("{}/".format(sys.argv[2]) + "tres_{}_{}.pdf", res)
