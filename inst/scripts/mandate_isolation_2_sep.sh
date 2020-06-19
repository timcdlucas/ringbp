#!/bin/bash

#$ -cwd
#$ -N mandate_isolation_2

# Choose the project and the queue. Test queue for < 1min tests.
# The c is for the type of node. i.e. a normal size node.

#$ -P hollingsworth.prjc -q short.qc
#$ -t 1-3:1


module  load R/3.6.2-foss-2019b

echo "******************************************"
echo "SGE job ID: "$JOB_ID
echo "SGE task ID: "$SGE_TASK_ID
echo "Run on host: "`hostname`
echo "Operating system: "`uname -s`
echo "Username :" `whoami`
echo "Started at: "`date`
echo "******************************************"

Rscript mandate_isolation_2_sep.R ${SGE_TASK_ID}


"******************************************"
echo "Finished at: "`date`
echo "******************************************"
exit 0
