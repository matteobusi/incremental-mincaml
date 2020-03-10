#! /bin/sh

make clean
make tbench

taskset -c 7 ./tbenchmark.native 5 3 10 10 true > results-5-3-10-10.csv &
taskset -c 6 ./tbenchmark.native 5 3 12 12 true > results-5-3-12-12.csv &
taskset -c 5 ./tbenchmark.native 5 3 14 14 true > results-5-3-14-14.csv &
taskset -c 4 ./tbenchmark.native 5 3 16 16 true > results-5-3-16-16.csv 

python analysis/analyze.py results-5-3-10-10.csv plots
python analysis/analyze.py results-5-3-12-12.csv plots
python analysis/analyze.py results-5-3-14-14.csv plots
python analysis/analyze.py results-5-3-16-16.csv plots
