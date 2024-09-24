#! /bin/bash
#SBATCH -N 1
#SBATCH --ntasks-per-node=48
#SBATCH --error=job.%A_%a.err
#SBATCH --output=job.%A_%a.out
#SBATCH --time=01:00:00
#SBATCH --partition=standard

ml MSCC/litesoph

cd /home/mscc/liteosph_test/octopus/spec_test/workflow_1/octopus;
## DO NOT REMOVE LINE BELOW
touch Start_fb0d8434-38fd-4437-aa0b-32bd583249a7
/opt/ohpc/pub/compiler/intel/2020/compilers_and_libraries_2020.4.304/linux/mpi/intel64/bin/mpirun -np 10 /home/apps/MSCC/lite/octopus/bin/octopus &> TaskTypes.RT_TDDFT/td.log
