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

- Download the sample input files  available in https://github.com/mscc07/mscc-testinput/tree/main/Litesoph_Inputs

## Step 3: Run the Application

The application is designed to run in **Graphical User Interface (GUI) Mode** .  
Simply launch the application to access its user-friendly graphical interface, where all functionalities can be performed through interactive menus and buttons.


To run the **LITESOPH** application, load the corresponding module and launch it using the GUI mode.

```bash

# Command 1: List all MSCC applications
module avail | grep -i mscc

# Command 2: Load the LITESOPH module
module load MSCC/litesoph     

# Command 3: Launch the GUI mode
litesoph gui

```

![Image](https://github.com/user-attachments/assets/61048e65-ea16-4606-968b-2014d33d0fd1)

Gui appears 

![Image](https://github.com/user-attachments/assets/144b941a-9637-43c7-b0e0-e5973becf244)

 ⚠️ To perform calculations  [Litesoph_how to run.pdf](https://github.com/user-attachments/files/19955967/Litesoph_how.to.run.pdf)




## Step 4: Output Files

After successful execution, output files are generated. The key files include:

## Screenshot of the output files

## Output in GUI

![Image](https://github.com/user-attachments/assets/e3b03d8e-741d-4ed0-af83-26783d0d9ceb)

## Output on the cluster 

![Image](https://github.com/user-attachments/assets/5b7671ee-6bb7-401f-8d68-feb9fffd5add)

![Image](https://github.com/user-attachments/assets/afa6dc3e-8125-47af-b264-d96b104f9e05)

![Image](https://github.com/user-attachments/assets/6a9425ab-09e1-4d6b-97ad-107d7e523f59)

## Video Tutorial
- Please refer to the tutorial video for more detailed running instructions: [LITESOPH Video Playlist](https://youtube.com/playlist?list=PL4_IG4UezHjDSP1blp4bK1dUvcyXi_0L1&si=j-xj592fIeUyTFi9)
---

