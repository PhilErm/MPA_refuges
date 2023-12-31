#!/bin/bash
#! Name of the job:
#SBATCH -J SSS_custom_log_mig_adam_ovr02_fishfav_sens_m01
#! Name of log:
#SBATCH -o ../logs/HPC_logs/SSS_custom_log_mig_adam_ovr02_fishfav_sens_m01.log
#! How many whole nodes should be allocated?
#SBATCH --nodes=1
#! How many (MPI) tasks will there be in total? (<= nodes*32)
#! The icelake/icelake-himem nodes have 76 CPUs (cores) each.
#SBATCH --ntasks=50
#! How much wallclock time will be required?
#SBATCH --time=10:00:00
#! What types of email messages do you wish to receive?
#SBATCH --mail-type=NONE
#! Uncomment this to prevent the job from being requeued (e.g. if
#! interrupted by node failure or system downtime):
#SBATCH --no-requeue
#! SBATCH --qos=intr

#! For 3380 MiB per CPU, set "-p icelake"; for 6760 MiB per CPU, set "-p icelake-himem": 
#SBATCH -p icelake

#! sbatch directives end here (put any additional directives above this line)

#! Notes:
#! Charging is determined by core number*walltime.
#! The --ntasks value refers to the number of tasks to be launched by SLURM only. This
#! usually equates to the number of MPI tasks launched. Reduce this from nodes*32 if
#! demanded by memory requirements, or if OMP_NUM_THREADS>1.
#! Each task is allocated 1 core by m01, and each core is allocated 5980MB (skylake)
#! and 12030MB (skylake-himem). If this is insufficient, also specify
#! --cpus-per-task and/or --mem (the latter specifies MB per node).

#! Number of nodes and tasks per node allocated by SLURM (do not change):
numnodes=$SLURM_JOB_NUM_NODES
numtasks=$SLURM_NTASKS
mpi_tasks_per_node=$(echo "$SLURM_TASKS_PER_NODE" | sed -e  's/^\([0-9][0-9]*\).*$/\1/')
#! ############################################################
#! Modify the settings below to specify the application's environment, location 
#! and launch method:

#! Optionally modify the environment seen by the application
#! (note that SLURM reproduces the environment at submission irrespective of ~/.bashrc):
. /etc/profile.d/modules.sh                # Leave this line (enables the module command)
#! module purge                               # Removes all modules still loaded
module load rhel7/m01-peta4            # REQUIRED - loads the basic environment

#! Insert additional module load commands after this line if needed:

#! Full path to application executable: 
application=""

#! Run options for the application:
options=""

#! Work directory (i.e. where the job will run):
workdir="$SLURM_SUBMIT_DIR"  # The value of SLURM_SUBMIT_DIR sets workdir to the directory
                             # in which sbatch is run.

###############################################################
### MY OWN CODE ####
###############################################################

#! Phil Erm's script
module load R/4.2.0-icelake
Rscript facil_log_mig_adam_ovr02_fishfav_sens_m01.R

###############################################################
### You should not have to change anything below this line ####
###############################################################

cd $workdir
echo -e "Changed directory to `pwd`.\n"

JOBID=$SLURM_JOB_ID

echo -e "JobID: $JOBID\n======"
echo "Time: `date`"
echo "Running on master node: `hostname`"
echo "Current directory: `pwd`"

#!if [ "$SLURM_JOB_NODELIST" ]; then
#!        #! Create a machine file:
#!        export NODEFILE=`generate_pbs_nodefile`
#!        cat $NODEFILE | uniq > machine.file.$JOBID
#!        echo -e "\nNodes allocated:\n================"
#!        echo `cat machine.file.$JOBID | sed -e 's/\..*$//g'`
#!fi

echo -e "\nnumtasks=$numtasks, numnodes=$numnodes, mpi_tasks_per_node=$mpi_tasks_per_node (OMP_NUM_THREADS=$OMP_NUM_THREADS)"

echo -e "\nExecuting command:\n==================\n$CMD\n"

eval $CMD 