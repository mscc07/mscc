# Guide to Run/Execute AMDKIIT Software on NSM HPC Systems

---

##  Step 1: Login to the NSM HPC System
- If you have a user account, please use your existing credentials to log in.
- If you do not have access to any NSM HPC system and wish to create an account, please contact [mscc-support@cdac.in](mailto:mscc-support@cdac.in)


```
#Command to log in into NSM HPC system via linux or windows OS
ssh -p 4422 username@hostname
```
![ssh](https://github.com/user-attachments/assets/fadebec6-8d52-4a81-b03e-a40bfaa96378)

> ⚠️ **Important:** Do not run any MSCC application on login nodes!


##  Step 2: Prepare input and bond Order files
To run a job using **AMDKIIT**, the user must provide the following three types of input files:
- **Input Coordinate File**
- **Pseudopotential Files**
- **YAML Input File**
  
Download the sample input files available in https://github.com/mscc07/mscc/tree/main/AMDKIIT/Test_Inputs

OR

Prepare input files by referring to the detailed instructions in the GitHub repository  https://github.com/mscc07/mscc/blob/main/AMDKIIT/AMDKIIT_Manual_New_v1_0_1.pdf


##  Step 3: Run the application 
The application can be run in two modes: **Interactive Mode** and **Non-Interactive Mode**

### A) Interactive Mode
In this mode, you manually allocate a node and run the application on that node.
- To allocate and log in to the node, execute the following commands in order:

```bash
# Command 1: Allocate a node
salloc -N 1

# Command 2: Check the assigned node
squeue --me

# Command 3: SSH into the assigned node
ssh <node-name>

```
![1](https://github.com/user-attachments/assets/3931a180-7bb7-420b-bbc6-1d5318bb5542)

- After logging into the node, you need to load the AMDKIIT application module by using the following commands
```bash
# Command 4: List all MSCC applications
module avail | grep -i mscc

# Command 5: Load the AMDKIIT application
module load MSCC/amdkiit

# Command 6: Check the loaded modules
module list
```
![2](https://github.com/user-attachments/assets/cbeeb0b7-dda7-4803-a1d3-1fa2518c558a)

- Once the module is loaded, navigate to the directory containing the input files and run the application using the following command:
```bash
mpirun -n <N_core> amdkiit.x input.yaml > amdkiit.out                                
```
![5](https://github.com/user-attachments/assets/8c727fe1-2ac2-4855-8f50-ddd025ae9aac)


### B) Non-Interactive Mode
You can also run the application using a job submission script.
- `submit_p.sh` is a sample script used to submit a job to a computing cluster. It automates the environment setup, runs the application, and efficiently handles input and output operations.

```bash
#!/bin/bash
#SBATCH --job-name=sys10       # Set the job name
#SBATCH --nodes=1              # Request one compute node
#SBATCH --partition=standard   # Specify the partition/queue
#SBATCH --time=48:00:00        # Set maximum execution time
#SBATCH --output=%j.out        # File to save standard output
#SBATCH --error=%j.err         # File to save error messages

module load MSCC/amdkiit                                         # Load the AMDKIIT module (verify module name if needed)
mpirun -n <N_core> amdkiit.x input.yaml > amdkiit.out            # Run the AMDKIIT application 
```

- To run the application, submit the job script using the following command:

```bash
sbatch job.sh   # Submit the batch job to the scheduler
```
![7](https://github.com/user-attachments/assets/23537964-c4dd-4cff-b2a6-359022f1d503)


## Step 4: Output Files
After successful execution, output files are generated. The general out file **amdkiit.out** contains all the information of the job being performed. Apart from that, different 
calculations AMDKIIT generates different output files for different type of calculations as mentioned here.

Depending on the type of calculation performed, the following output files will be generated:

### 4.1 Wavefunction Optimization

```
SP_ENERGY.dat    #Contains Kohn-Sham (KS) energy (in Hartree) at each SCF step.  
```
### 4.2 Geometry Optimization

```
atom_coord.xyz    # Appends all ionic coordinates (in Å) during the optimization process in XYZ format.  
atom_force.dat    # Stores atomic forces (in XYZ format) computed during geometry optimization.  
```

### 4.3 Molecular Dynamics
```
MD_ENERGY.dat     # Logs instantaneous temperature (K), KS energy (Hartree), total energy (Hartree), and CPU time (seconds) at every MD step.
MD_TRAJECTORY.xyz # Records the system geometry (in Å) at each MD step in XYZ format (append mode).  
```
## Screentshot of the output files for wavefunction optimization
![7](https://github.com/user-attachments/assets/2c8e745f-5870-46a3-9f2a-04cb3c47b7c6)
## Video Tutotial
- Please refer to the tutorial video for more detailed running instructions: https://www.youtube.com/watch?v=x69wl20Hq5M&t=2273s 
---


