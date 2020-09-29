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
        id_res_orig = res[(res["name"].str.startswith(orig_n)) & (res["nodecount"] == 2**depth - 1) & (res["fvc"] == fvc)].drop(["invalidation_parameter", "name", "fvc"], axis=1).groupby("nodecount").mean().reset_index()
        id_res_einc = res[(res["name"].str.startswith(einc_n)) & (res["nodecount"] == 2**depth - 1) & (res["fvc"] == fvc)].drop(["invalidation_parameter", "name", "fvc"], axis=1).groupby("nodecount").mean().reset_index()

        if len(id_res_orig.index) == 1:
            orig_r, einc_r = id_res_orig.iloc[0]["rate"], id_res_einc.iloc[0]["rate"]
            ratio = einc_r / orig_r
            if fvc.bit_length() == 1:
                print("${}$ & $1$ & ${:.2f}$ & ${:.2f}$ & ${:.2f}$\\\\".format(depth, orig_r, einc_r, ratio))
            else:
                print("${}$ & $2^{{{}}}$ & ${:.2f}$ & ${:.2f}$ & ${:.2f}$\\\\".format(depth, fvc.bit_length() - 1, orig_r, einc_r, ratio))

def plot_on_pdf (filename, res):
    for nodecount in res["nodecount"].unique():
        res_2 = res[res["nodecount"] == nodecount]
        for fvc in res_2["fvc"].unique():
            res_3 = res_2[res_2["fvc"] == fvc]
            with PdfPages(filename.format(nodecount, fvc)) as pdf:
                fig, ax = plt.subplots()
                add_res_orig = res_3[res_3["name"].str.startswith(orig_n)].drop(["name", "nodecount", "fvc"], axis=1).groupby(["invalidation_parameter"]).mean().reset_index()
                add_res_inc  = res_3[res_3["name"].str.startswith(inc_n)].drop(["name", "nodecount", "fvc"], axis=1).groupby(["invalidation_parameter"]).mean().reset_index()

                # add_res_orig = add_res_orig.drop(["invalidation_parameter"], axis=1)
                # add_res_inc = add_res_inc.drop(["invalidation_parameter"], axis=1)

                add_res_orig["invalidation_parameter"] = np.log2(nodecount+1) - add_res_orig["invalidation_parameter"]
                add_res_inc["invalidation_parameter"] = np.log2(nodecount+1) - add_res_inc["invalidation_parameter"]


                add_res_orig.plot(x="invalidation_parameter", y="rate", ax=ax, label=orig_n, marker='*', color='blue', linewidth=1, linestyle='dashed') # BLUE
                add_res_inc.plot(x="invalidation_parameter", y="rate", ax=ax, label=inc_n, marker='d', color='orange', linewidth=1) # ORANGE

                ymax = max (
                    [
                        max(add_res_orig["rate"]),
                        max(add_res_inc["rate"]),
                    ]
                )

                ymin = max ([ 0, min (
                    [
                        min(add_res_orig["rate"]),
                        min(add_res_inc["rate"]),
                    ]
                ) - 1])

                plt.yticks(np.arange(ymin, ymax + 1, (ymax-ymin+1)/10))
                plt.xlabel("$\log ($ size of the diff sub-tree $ + 1)$")
                plt.ylabel("throughput (re-typings/$s$)")

                pdf.savefig()
                plt.close()


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(("Usage: {} input_file.csv dest_path\n").format(sys.argv[0]))
    else:
        res = pandas.read_csv(sys.argv[1], sep=", ", engine="python", dtype={'fvc':int, 'invalidation_parameter':int, 'nodecount':int, 'diffsz' : int, 'rate':float})

        # Just plot for "big" enough trees
        res = res[res["nodecount"] >= 2**8]

        # Create the directory
        os.makedirs("{}".format(sys.argv[2]), exist_ok=True)

        tabulate(res, [(16,1), (16, 2**7), (16, 2**9), (16, 2**11), (16, 2**13), (16, 2**15)])
        tabulate(res, [(14,1), (14, 2**7), (14, 2**9), (14, 2**11), (14, 2**13)])
        tabulate(res, [(12,1), (12, 2**7), (12, 2**9), (12, 2**11)])

        plot_on_pdf("{}/".format(sys.argv[2]) + "tres_{}_{}.pdf", res)
