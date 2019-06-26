# !/bin/bash
#
# 03_computational_modeling_analysis_cluster_run_script.sh

# Any task
pids="1 6 9 11 13 14 15 17 19 20 21 23 24 25 26 27 32 33 35 36 37 38 41 42 43 45 46 47 49 50 51 52 55 56 58 62 64 65 67 68 69 70 74 75 77 79 81 82 83 84 85 86 87 88 89 90 91 92 93";

for pid in `echo $pids`
  do
    echo `module unload R; module load R/3.5.1; source ~/.bashrc; cd /project/3017051.01; sh analysis/03_computational_modeling_analysis_cluster.sh $pid`
  done
