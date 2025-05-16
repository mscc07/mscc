# Guide to Run/Execute ANN-CI Software on NSM HPC Systems

---

##  Step 1: Login to the NSM HPC System
- If you have a user account, please use your existing credentials to log in.
- If you do not have access to any NSM HPC system and wish to create an account, please contact [mscc-support@cdac.in](mailto:mscc-support@cdac.in)


```
# Command to log in into NSM HPC system via linux or windows OS
ssh -p 4422 username@hostname
```
![ssh](https://github.com/user-attachments/assets/fadebec6-8d52-4a81-b03e-a40bfaa96378)

> ⚠️ **Important:** Do not run any MSCC application on login nodes!

## Step 2: Create a directory and prepare input files

Create a working directory and navigate into it, this directory will contain the input files.

```bash
# Command 1: Create a directory
mkdir annci_test001

# Command 2: Navigate into the directory
cd annci_test001
```

- Download the sample input files available in [Sample input files](https://github.com/rahul-dethe/running_instructions/tree/master/ann_wk2/inputs)

OR

- Prepare input and bond order files by referring to the detailed instructions in the GitHub repository  [AL-MCCI](https://github.com/dghoshlab/AL-MCCI)


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
![salloc](https://github.com/user-attachments/assets/c4982c71-6ed3-4f35-b2d5-37e8aa325cb7)

- After logging into the node, navigate to the directory that contains the input and bond order files and load the ANN-CI application module using the following commands
```bash
# Command 4: List all MSCC applications
module avail | grep -i mscc

# Command 5: Load the ANN-CI application
module load MSCC/ann-ci

# Command 6: Check the loaded modules
module list
```
![module list](https://github.com/user-attachments/assets/37181545-902c-46c0-af11-9a6f9078d809)


Make sure to **choose the correct version of ann-ci** from the available modules, if multiple versions are listed.

- Running the Application
Once the module is loaded, run the application using the following command:
```bash
exe.py **<your_input_file>**                      # Replace <your_input_file> with the actual file name
```
![running the application](https://github.com/user-attachments/assets/9a37669c-4eb3-4067-a080-f1886e5dddac)

### B) Non-Interactive Mode

You can also run the application using the job submission script.

- Create a directory and create the input and bond Order files and navigate into it (if already have, navigate into it)
  
- Create a file _job.sh_ with the following content:
This is a script used to submit a job to a computing cluster. It automates the job setup, runs your application, and handles input/output.

```bash
#!/bin/bash
#SBATCH --job-name=sys10       # Set the job name
#SBATCH --nodes=1              # Request one compute node
#SBATCH --partition=standard   # Specify the partition/queue
#SBATCH --time=48:00:00        # Set maximum execution time
#SBATCH --output=%j.out        # File to save standard output
#SBATCH --error=%j.err         # File to save error messages

module load MSCC/ann-ci        # Load the ANN-CI module (verify module name if needed)
cd $SLURM_SUBMIT_DIR           # Navigate to the job submission directory
exe.py <input_file>            # Run the ANN-CI application (replace <input_file> accordingly)
```

- To run the application, submit the above job submission script using the below command 
Once your _job.sh_ file is ready, run the script using the command below:

```bash
sbatch job.sh             # Submit the batch job
```
![sbatch job sh](https://github.com/user-attachments/assets/6e1a50e3-64d6-4816-b042-fb2a06b195c4)

## Step 4: Output Files

After successful execution, 10 output files are generated. The key files include:

```
1)  input_file.in.out                        # Main output file containing information on subspace size and calculated energy.
2)  input_file.in.out.basis                  # Configurations of final sub-Hilbert space
3)  input_file.in.out.ci                     # CI coefficient corresponding to configurations
4)  input_file.in.out.model.pth              # Final optimized ANN model
5)  input_file.in.out.error.dat              # Training and testing errors at each Active Learning iteration.
6)  input_file.in.out.TrainData_subSpace.csv #Training dataset generated during the calculation.
```
These files are essential for analyzing the system and model performance. In addition, there are four more files, the following are scratch files created during the computation process:
```
7)  input_file.in.out.predictData.csv
8)  input_file.in.out.accVsPreTest.dat
9)  input_file.in.out.accVsPreTrain.dat
10) input_file.in.out.enrich.csv
```
It is recommended to delete these files to keep the directory clean.

## Screentshot of the output files
![output files](https://github.com/user-attachments/assets/17d9ee71-dbcd-4023-b4b9-251bf66585f8)

## Video Tutorial
- Please refer to the tutorial video for more detailed running instructions: https://youtu.be/R2KRXly2RBo?si=Wd9UUfS0lKd2Ayfy
---
