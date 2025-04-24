# Guide to Run/Execute LITESOPH Software on NSM HPC Systems

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


##  Step 2: Download the sample input files

- Download the sample input files  available in https://github.com/mscc07/mscc-testinput.git

## Step 3: Run the Application

The application is designed to run in **Graphical User Interface (GUI) Mode** .  
Simply launch the application to access its user-friendly graphical interface, where all functionalities can be performed through interactive menus and buttons.

## Step 3: Run the Application

To run the **LITESOPH** application, load the corresponding module and launch it using the GUI mode.

```bash
module load MSCC/litesoph     # Load the LITESOPH module
litesoph gui                  # Launch the GUI mode
```

![Image](https://github.com/user-attachments/assets/61048e65-ea16-4606-968b-2014d33d0fd1)

Gui appears 

![Image](https://github.com/user-attachments/assets/50a9c41e-4258-4108-b8a3-de9aa909b5ad)

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
- Please refer to the tutorial video for more detailed running instructions: [LITESOPH Video Playlist](https://youtube.com/playlist?list=PL4_IG4UezHjDSP1blp4bK1dUvcyXi_0L1&si=j-xj592fIeUyTFi9)
---

