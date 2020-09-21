import pandas
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib import rcParams
import seaborn as sns
import sys
import os

## Names of the experiments, taken from ocaml source
orig_n = "orig:"
einc_n = "einc"
finc_n = "finc"
setupinc_n = "setup+inc:"
setup_n = "setup:"
inc_n = "inc:"
##

font = {'family' : 'serif',
        'weight' : 'bold',
        'size'   : 15}

plt.rc('text', usetex=True)
plt.rc('font', **font)
rcParams.update({'figure.autolayout': True})

def tabulate(res, interesting_pairs):
    for (num_fact, xi_invalidated) in interesting_pairs:
        id_res_orig = res[(res["name"].str.startswith(orig_n)) & (res["nodecount"] == num_fact) & (res["invalidation_parameter"] == xi_invalidated)].drop(["invalidation_parameter", "name"], axis=1).groupby("nodecount").mean().reset_index()
        id_res_einc = res[(res["name"].str.startswith(einc_n)) & (res["nodecount"] == num_fact) & (res["invalidation_parameter"] == xi_invalidated)].drop(["invalidation_parameter", "name"], axis=1).groupby("nodecount").mean().reset_index()

        if len(id_res_orig.index) == 1:
            orig_r, einc_r = id_res_orig.iloc[0]["rate"], id_res_einc.iloc[0]["rate"]
            ratio = einc_r / orig_r
            print("${}$ & ${:.2f}$ & ${:.2f}$ & ${:.2f}$\\\\".format(num_fact, orig_r, einc_r, ratio))

def plot_on_pdf (filename, res):
    for num_fact in res["nodecount"].unique():
        res_2 = res[res["nodecount"] == num_fact]
        with PdfPages(filename.format(num_fact)) as pdf:
            fig, ax = plt.subplots()
            add_res_orig = res_2[res_2["name"].str.startswith(orig_n)].drop(["name", "nodecount", "invalidation_parameter"], axis=1).groupby(["diffsz"]).mean().reset_index()
            add_res_inc  = res_2[res_2["name"].str.startswith(inc_n)].drop(["name", "nodecount", "invalidation_parameter"], axis=1).groupby(["diffsz"]).mean().reset_index()
            # add_res_setupinc = res_2[res_2["name"].str.startswith(setupinc_n)].drop(["name", "nodecount", "invalidation_parameter"], axis=1).groupby(["diffsz"]).mean().reset_index()
            #add_res_setup = res_2[res_2["name"].str.startswith(setup_n)].drop(["name", "nodecount", "invalidation_parameter"], axis=1).groupby(["diffsz"]).mean().reset_index()

            # add_res_inc_comp = add_res_inc.drop(["rate"], axis=1)
            # add_res_inc_comp["rate"] = 1.0/(1.0/add_res_setupinc["rate"] - 1.0/add_res_setup["rate"])

            # add_res_orig["diffsz"] = num_fact - add_res_orig["invalidation_parameter"]
            # add_res_inc["diffsz"] = num_fact - add_res_inc["invalidation_parameter"]

            add_res_orig.plot(x="diffsz", y="rate", ax=ax, label=orig_n, marker='*', color='blue', linewidth=2, linestyle='dashed') # BLUE
            add_res_inc.plot(x="diffsz", y="rate", ax=ax, label=inc_n, marker='d', color='orange', linewidth=2) # ORANGE
            # add_res_setupinc.plot(x="diffsz", y="rate", ax=ax, label=setupinc_n, marker='*', color='red', linewidth=2) # RED
            #add_res_setup.plot(x="diffsz", y="rate", ax=ax, label=setup_n, marker='o', color='green', linewidth=2) # GREEN
            # add_res_inc_comp.plot(x="diffsz", y="rate", ax=ax, label="inc\_comp", marker='X', color='violet', linewidth=2) # YELLOW

            # plt.xticks(np.arange(min(add_res_orig["diffsz"]), max(add_res_orig["diffsz"]) + 1, 1.0))
            ymax = max (
                [
                max(add_res_orig["rate"]),
                max(add_res_inc["rate"]),
                # max(add_res_setupinc["rate"]),
                #max(add_res_setup["rate"]),
                # max(add_res_inc_comp["rate"])
                ]
            )

            ymin = max ([ 0, min (
                [
                min(add_res_orig["rate"]),
                min(add_res_inc["rate"]),
                # min(add_res_setupinc["rate"]),
                #min(add_res_setup["rate"]),
                # min(add_res_inc_comp["rate"])
                ]
            ) - 1])

            plt.yticks(np.arange(ymin, ymax + 1, (ymax-ymin)/10 + 1))
            plt.xlabel("$\mathit{\# nodes\_diff}$")
            plt.ylabel("re-typings/$s$")

            pdf.savefig()
            plt.close()


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(("Usage: {} input_file.csv dest_path\n").format(sys.argv[0]))
    else:
        res = pandas.read_csv(sys.argv[1], sep=", ", engine="python", dtype={'num_fact':int, 'xi_invalidated':int, 'rate':float})

        # Create the directory
        os.makedirs("{}".format(sys.argv[2]), exist_ok=True)

        # tabulate(res, [(16,1), (16, 2**7), (16, 2**9), (16, 2**11), (16, 2**13), (16, 2**15)])
        # tabulate(res, [(14,1), (14, 2**7), (14, 2**9), (14, 2**11), (14, 2**13)])
        # tabulate(res, [(12,1), (12, 2**7), (12, 2**9), (12, 2**11)])

        plot_on_pdf("{}/".format(sys.argv[2]) + "dres_{}.pdf", res)
