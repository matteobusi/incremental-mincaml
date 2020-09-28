import pandas
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib import rcParams
from cycler import cycler
import seaborn as sns
import sys
import os

## Names of the experiments, taken from ocaml source
orig_n = "orig"
einc_n = "einc"
finc_n = "finc"
setupinc_n = "setup+inc"
setup_n = "setup:"
inc_n = "inc"
##

font = {'family' : 'serif',
        'weight' : 'bold',
        'size'   : 15}

plt.rc('text', usetex=True)
plt.rc('font', **font)
rcParams.update({'figure.autolayout': True})

def plot_on_pdf (filename, res):
    for num_fact in res["nodecount"].unique():
        res_2 = res[res["nodecount"] == num_fact]
        with PdfPages(filename.format(num_fact)) as pdf:
            fig, ax = plt.subplots()
            from matplotlib.cm import get_cmap

            name = "Accent"
            cmap = get_cmap(name)  # type: matplotlib.colors.ListedColormap
            colors = cmap.colors  # type: list
            ax.set_prop_cycle(color=colors)

            add_res_orig = res_2[res_2["name"].str.startswith(orig_n)].drop(["name", "nodecount", "invalidation_parameter","threshold"], axis=1).groupby(["diffsz"]).mean().reset_index()
            add_res_inc  = res_2[res_2["name"].str.startswith(inc_n)].drop(["name", "nodecount", "invalidation_parameter"], axis=1).groupby(["diffsz","threshold"]).mean().reset_index()

            add_res_orig["diffsz"] = np.log2(add_res_orig["diffsz"])
            add_res_inc["diffsz"] = np.log2(add_res_inc["diffsz"])

            for t in add_res_inc["threshold"].unique():
                if t != 4611686018427387903:
                    add_res_inc[add_res_inc["threshold"] == t].drop(["threshold"], axis=1).plot(x="diffsz", y="rate", ax=ax, marker='+', label=inc_n + " (" + str(t) + ")", linewidth=1) #

            add_res_orig.plot(x="diffsz", y="rate", ax=ax, marker='*', linewidth=1, color="blue", label=orig_n, linestyle='dashed') # BLUE
            add_res_inc[add_res_inc["threshold"] == 4611686018427387903].drop(["threshold"], axis=1).plot(x="diffsz", y="rate", ax=ax, marker='d', label=inc_n, color="red", linewidth=1) # ORANGE


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
            plt.xlabel("$\log (\mathit{\# nodes\_diff})$")
            plt.ylabel("re-typings/$s$")

            # plt.legend().remove()
            plt.legend(bbox_to_anchor=(0., 1.02, 1., .102), loc=3, ncol=3, mode="expand", borderaxespad=0.)


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
