import os
import socket
from getpass import *

# .......................................
# Setting Paths. Please note variables PLUGIN_ROOT and OUTPUT_PREFIX should end with /  
# .......................................
#***************************************************************************************
PLUGIN_ROOT = "/home/sahu/mtaplugin/"
OUTPUT_PREFIX = "/home/sahu/jobs/"
GAUSSIANCOMAND =  "g16"
#***************************************************************************************

# remote gamserver root
REMOTE_GAMSERVER_ROOT = PLUGIN_ROOT + "gamserver"

# output prefix

CODE = 'GAUSSIAN'
# GAUSSIAN COMMAND
# Please make sure that the path for gaussian directory is correctly specified in gauss.sh
#GAUSS_CMD = GAUSSIANCOMAND + "  "
GAUSS_CMD = '/bin/sh ' + PLUGIN_ROOT + 'gauss.sh '
# grep atoms command
GAUSS_GREP_ATOMS_CMD = "grep --binary-files=text \"NAtoms\" " 
# grep constactions command
GAUSS_GREP_CONTRACTIONS_CMD = "grep --binary-files=text \"NBasis\" " 
# a "gamess job over" and "gaussian job over" indication strings 
GAUSS_JOB_OVER = "$GAUSS_JOB_OVER\n"
# a "gamess job killed" and "gaussian job killed" indication strings 
GAUSS_JOB_KILLED = "$GAUSS_JOB_KILLED\n"
# YES
GAUSS_YES = "YES"
GAMESS_YES = GAUSS_YES
# NO
GAUSS_NO  = "NO"
GAMESS_NO = GAUSS_NO

# For Gaussian-PlugIn dictionary of Atomic Numbers and Atom Symbols
AtmSymbol = {1:'H', 2:'He', 3:'Li', 4:'Be', 5:'B', 6:'C', 7:'N', 8:'O', 9:'F', 10:'Ne', 11:'Na', 12:'Mg', 13:'Al', 14:'Si', 15:'P', 16:'S', 17:'Cl', 18:'Ar', 19:'K', 20:'Ca', 21:'Sc', 22:'V', 23:'Tr', 24:'Cr', 25:'Mn', 26:'Fe', 27:'Co', 28:'Ni', 29:'Cu', 30:'Zn', 31:'Ga', 34:'Se', 46:'Pd',47:'Ag',48:'Cd'}

AtmNumber = ['H', 'He', 'Li', 'Be', 'B', 'C', 'N', 'O', 'F', 'Ne', 'Na', 'Mg', 'Al', 'Si', 'P', 'S', 'Cl', 'Ar', 'K', 'Ca', 'Sc', 'V', 'Tr', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', 'Ga', 'Se', 'Pd','Ag','Cd']

# Dictionary of Atomic Masses
AtmMass = {1:1.0079, 2:4.0026, 3:6.9410, 4:9.0122, 5:10.8110, 6:12.0107, 7:14.007, 8:15.999, 9:18.998, 10:18.998, 11:22.989, 12:24.305, 13:26.982, 14:26.982, 15:31.97, 16:32.066, 17:35.453, 18:39.948, 19:39.098, 20:40.078, 21:44.956, 22:47.867, 23:50.942, 24:51.996, 25:54.938, 26:55.845, 27:58.933, 28:58.693, 29:62.546, 30:65.409, 31:69.723, 34:78.96, 46:106.42, 47:107.8682 ,48:112.411}
# .......................................
# Setting strings, integers, booleans
# .......................................

# gamess version
PLUGIN_VERSION = "1.0.0"
# the gamess local host
PLUGIN_HOST = ''
# the gamess server keep alive message
PLUGIN_SERVER_KEEPALIVE_MSG = "ALIVE"
# the gamess server keep alive message
PLUGIN_SERVER_KILL_MSG = "KILL"
# a "data over" indication string
DATA_OVER = "$DATA_OVER\n"
# the redirection operator(s)!
REDIRECTION_OPERATOR = " > "
REDIRECTION_ERR_OPERATOR = " >& "
# use the "keep alive" thread?
USE_KEEPALIVE_THREAD = 1
# the buffer size
BUFFER_SIZE = 1024
# host sufix
HOST_SUFIX = socket.gethostname()
# fragment number width
FRAG_NUMBERS = 4
# sleep time for heart beat query ( 30 sec)
HEART_BEAT_SPAN = 30
# server sleep time ... waiting for a processor
SERVER_SLEEP_SPAN = 100
# the total number of reserver ports
RESERVED_PORTS = 1024
# the number of process per node
# it is not used in the current version, but may be very important in near future
NUMBER_OF_PROCESS_PER_NODE = 1
# Do we use NFS? this is important as the clustering of nodes
# needs NFS, or else the host files need to be copied.
# By default we assume it is available! If NFS is not available
# then we will need to "copy" the host files if node
# clustering is needed.
NFS_AVAILABLE = 0 
# Are you running on a numa enabled setup, like an
# SGI Altix. If so, then the gamboot process will
# attempt to use some performace optimizations such
# as cpu set locking and controlling memory placement.
# Default is set to false (0).
NUMA_ENABLED = 0
# .......................................
# Setting Commands 
# .......................................
# command used to execute remote shell
#REMOTE_CMD = "/usr/bin/rsh -n "
REMOTE_CMD = "/usr/bin/ssh -n "
# remote copy command
REMOTE_COPY_CMD = "/usr/bin/scp "

# the pbsdsh execution process
PBSDSH_CMD = "/opt/pbs/bin/pbsdsh -N -v "

# the dplace command, to lock processes in
# a cpu set in round robin fashion on
# NUMA machines like SGI Altix.
DPLACE_CMD = "/usr/bin/dplace "

# the numactl command, to control memory
# placemnet near the allocated processor
# on NUMA machines like SGI Altix.
NUMACTL_CMD = "/usr/bin/numactl "

# query job command
QUERY_JOB = "$QUERY_JOB$\n"

# job complete command
JOB_COMPLETE = GAUSS_JOB_OVER

# job killed command
JOB_KILLED = GAUSS_JOB_KILLED

# file delete command
FILE_DEL_CMD = "rm -f " + " "

# python command
PYTHON_CMD = "/usr/bin/python " + " "

# Sync time for booting remote hosts
REMOTE_SYNC_TIME = 2

AUTO_BOOT_GAMSERVER = 1

PLUGIN_ANY_PORT = 0


PLUGIN_SERVER_PORT = 50007

# the default notification port
PLUGIN_NOTIFY_SERVER_PORT = 50008

# the default update port
PLUGIN_UPDATE_SERVER_PORT = 50009

# the default "keep-alive" port
PLUGIN_KEEPALIVE_PORT = 50010

# the port of @Home server
# as the roles (client/server) are reversed, so is the port number ;)
AT_HOME_SERVER_PORT = 70005

# the default notification server
PLUGIN_NOTIFY_SERVER = "192.9.200.1"

# @ home server address; the information for the clients
AT_HOME_SERVER_ADDRESS = "192.9.200.211"


# .......................................
# important object settings
# related to distributed mode
# .......................................

import gamnodeallocater
import gamboot
import gamexecuter
import gamscheduler

# How the node list is generated?
# Default is manual specification of the nodes. For instace:
# gamNodeList = ManualGamNodeList(["platinum", "gold", "silver"])
# Allocates three nodes named "platinum", "gold" and "silver".
#
# If you are using a queuing system, like PBS then using a manual
# node allocation might not be a good option. You may try something
# like:
# gamNodeList = PBSGamNodeList()
#
# See gamnodeallocater.py for available implementations.
gamNodeList = gamnodeallocater.ManualFileGamNodeList("/home/sskadam/SPPU/MTASpecParallel/Jobs/node.list")

# How the nodes are allocated? 
# Default setting in this case is good enough for most cases,
# unless ofcourse you need some weired settings.
#
# See gamnodeallocater.py for available implementations.
gamNodeAllocater = gamnodeallocater.GroupClusteredNodeGamNodeAllocater(gamNodeList,[1])

# The gamBoot instance. Defines how the
# booting process of the gamexecuter is
# performed on remote machines.
#
# See gamboot.py for available implementations.
gamBoot = gamboot.RSHGamBoot()

# The gamExecuter instance. Defines how the 
# gamess process execution will be handled on
# individual nodes.
#
# See gamexecuter.py for available implementations.
gamExecuter = gamexecuter.PlugInGAUSSIANGamExecuter()

# The gamScheduler instance. Defines how the
# gamess fragment job scheduling is handled.
#
# See gamscheduler.py for available implementations.

gamScheduler = gamscheduler.PlugInGAUSSIANGamScheduler()

# A global dictionary of fragment enegies for previous and current optimization cycles, which is required in the case of Conjugate Gradient Optimizer
OPT_FRAGENES = {}
