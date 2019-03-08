import pandas
import numpy as np
import sys
import os


def tabulate(res, interesting_pairs):
    print("Depth & Vars & Standard (MB) & Incremental (MB) & Ratio & Diff\\\\")
    for (depth, fvc) in interesting_pairs:
        id_res_orig = res[res["name"] == (
            "orig_" + str(depth) + "_" + str(fvc))]
        id_res_inc = res[res["name"] == (
            "inc_" + str(depth) + "_" + str(fvc))]

        if len(id_res_orig.index) == 1:
            orig_sz_bytes, inc_sz_bytes = int(id_res_orig.iloc[0]["allocated_bytes"]), int(
                id_res_inc.iloc[0]["allocated_bytes"])

            orig_sz, inc_sz = float(orig_sz_bytes) / 1000000, \
                float(inc_sz_bytes) / 1000000

            diff = inc_sz - orig_sz
            ratio = orig_sz_bytes/float(inc_sz_bytes)
            print("${}$ & $2^{{{}}}$ & ${:.2f}$ & ${:.2f}$ & ${:.4f}$ & ${:.4f}$ \\\\".format(
                depth, fvc.bit_length() - 1, orig_sz, inc_sz, ratio, diff))


def tabulate_cache(res, interesting_pairs):
    print("Depth & Vars & Cache (MB)\\\\")
    for (depth, fvc) in interesting_pairs:
        cache_mem = res[res["name"] == (
            "cache_" + str(depth) + "_" + str(fvc))]
        if len(cache_mem.index) == 1:
            cache_mem_bytes = int(cache_mem.iloc[0]["allocated_bytes"])
            cache_mem = float(cache_mem_bytes) / 1000000
            print("${}$ & $2^{{{}}}$ & ${:.4f}$\\\\".format(
                depth, fvc.bit_length() - 1, cache_mem))


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(("Usage: {} cache_file.json mem_file.json\n").format(
            sys.argv[0]))
    else:
        res_cache = pandas.io.json.json_normalize(pandas.read_json(
            sys.argv[1])["nodes"])[["name", "allocated_bytes"]]
        res_mem = pandas.io.json.json_normalize(pandas.read_json(sys.argv[2])["nodes"])[
            ["name", "allocated_bytes"]]

        tabulate_cache(
            res_cache, [(10, 512), (12, 2048), (14, 8192), (16, 32768)])
        tabulate(res_mem, [(10, 512), (12, 2048), (14, 8192), (16, 32768)])
