# Guide to Run/Execute μ2mech Software on NSM HPC Systems

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
mkdir mu2mech_test001

# Command 2: Navigate into the directory
cd mu2mech_test001
```


##  Step 3: Run the application 
The application is designed to run in **Graphical User Interface (GUI) Mode.**

Simply launch the application to access its user-friendly graphical interface, where all functionalities can be performed through interactive menus and buttons.

•	To run the **µ2mech** application, load the corresponding module and launch it using the GUI mode.

```bash
# Command 1: List all MSCC applications
module avail | grep -i mscc

# Command 2: Load the LITESOPH module
module load MSCC/mu2mech
```

![image](https://github.com/user-attachments/assets/8920ff98-517f-4927-be7a-79abd8ef395d)

•	Open the µ2mech GUI with the below command

```bash
# Command 3: Launch the GUI mode
mu2mech
```
- GUI appears

![image](https://github.com/user-attachments/assets/3c8ac5d9-1d54-436c-b552-34a196a6959c)

- ⚠️ To perform calculations refer [µ2mech(pdf)](https://github.com/mscc07/mscc/blob/main/%CE%BC2mech/Mu2Mech.pdf) 

## Video Tutorial
- Please refer to the tutorial video for more detailed running instructions: https://youtu.be/U91WctyAsoQ?si=hDoVVqZ5YQL44svG
---
