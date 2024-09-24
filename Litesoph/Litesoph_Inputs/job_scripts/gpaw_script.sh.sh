#! /bin/bash
#SBATCH -N 1
#SBATCH --ntasks-per-node=48
#SBATCH --error=job.%A_%a.err
#SBATCH --output=job.%A_%a.out
#SBATCH --time=02:00:00
#SBATCH --partition=standard

ml MSCC/litesoph

export PYTHONPATH=$PYTHONPATH:/home/apps/MSCC/lite/litesoph/lib/python3.10/site-packages  

cd /home/mscc/liteosph_test/gpaw/masking_4.26/workflow_1/gpaw/TaskTypes.RT_TDDFT;
## DO NOT REMOVE LINE BELOW
touch Start_853c2269-5c3d-445f-8c51-fd5a25410b5e
/opt/ohpc/pub/compiler/intel/2020/compilers_and_libraries_2020.4.304/linux/mpi/intel64/bin/mpirun -np 48 /home/apps/MSCC/lite/gpaw/g1/bin/python td.py
