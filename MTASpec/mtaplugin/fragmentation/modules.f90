!!
!! modules.f90
!!
!! All the modules pertaining to fragv2 are declared here.
!!
!! @author V.Ganesh
!! @version 2.0
!!

!!
!! Keeps all info. pertaining to atom properties, like covalent radii, etc.
module m_atomInfo
use m_datatypes

implicit none

integer(kind=SP) :: NUMBER_OF_ELELEMTS
parameter (NUMBER_OF_ELELEMTS = 48)

integer(kind=SP) , dimension(NUMBER_OF_ELELEMTS) :: atomicNumbers
integer(kind=SP) , dimension(NUMBER_OF_ELELEMTS) :: defaultValencies
character(len=CH), dimension(NUMBER_OF_ELELEMTS) :: atomicSymbols
real(kind=DP)    , dimension(NUMBER_OF_ELELEMTS) :: covalentRadii
real(kind=DP)    , dimension(NUMBER_OF_ELELEMTS) :: vdWRadii
real(kind=DP)    , dimension(NUMBER_OF_ELELEMTS) :: dblOverlap
real(kind=DP)    , dimension(NUMBER_OF_ELELEMTS) :: weakBondAngles
real(kind=DP)    , dimension(NUMBER_OF_ELELEMTS) :: atomicMass

contains 
   !!
   !! initilises all the atom infos. 
   subroutine initAtomInfo()
     implicit none

     ! init the atom numbers
     atomicNumbers(1)  = 1;  atomicNumbers(2)  = 2; 
     atomicNumbers(3)  = 3;  atomicNumbers(4)  = 4; 
     atomicNumbers(5)  = 5;  atomicNumbers(6)  = 6; 
     atomicNumbers(7)  = 7;  atomicNumbers(8)  = 8; 
     atomicNumbers(9)  = 9;  atomicNumbers(10) = 10; 
     atomicNumbers(11) = 11; atomicNumbers(12) = 12; 
     atomicNumbers(13) = 13; atomicNumbers(14) = 14; 
     atomicNumbers(15) = 15; atomicNumbers(16) = 16; 
     atomicNumbers(17) = 17; atomicNumbers(18) = 18; 
     atomicNumbers(19) = 19; atomicNumbers(20) = 20; 
     atomicNumbers(21) = 21; atomicNumbers(22) = 22; 
     atomicNumbers(23) = 23; atomicNumbers(24) = 24; 
     atomicNumbers(25) = 25; atomicNumbers(26) = 26; 
     atomicNumbers(27) = 27; atomicNumbers(28) = 28; 
     atomicNumbers(29) = 29; atomicNumbers(30) = 30; 
     atomicNumbers(31) = 31; atomicNumbers(32) = 32; 
     atomicNumbers(33) = 33; atomicNumbers(34) = 34; 
     atomicNumbers(35) = 35; atomicNumbers(36) = 36; 
     atomicNumbers(37) = 37; atomicNumbers(38) = 38; 
     atomicNumbers(39) = 39; atomicNumbers(40) = 40; 
     atomicNumbers(41) = 41; atomicNumbers(42) = 42; 
     atomicNumbers(43) = 43; atomicNumbers(44) = 44; 
     atomicNumbers(45) = 45; atomicNumbers(46) = 46; 
     atomicNumbers(47) = 47; atomicNumbers(48) = 48;

     ! init the atomic mass
     atomicMass(1)  = 1.0079;  atomicMass(2)  = 4.0026; 
     atomicMass(3)  = 6.941;   atomicMass(4)  = 9.0122; 
     atomicMass(5)  = 10.81;   atomicMass(6)  = 12.011; 
     atomicMass(7)  = 14.007;  atomicMass(8)  = 15.999; 
     atomicMass(9)  = 18.998;  atomicMass(10) = 18.998; 
     atomicMass(11) = 22.989;  atomicMass(12) = 24.305; 
     atomicMass(13) = 26.982;  atomicMass(14) = 26.982; 
     atomicMass(15) = 31.97;   atomicMass(16) = 32.066; 
     atomicMass(17) = 35.453;  atomicMass(18) = 39.948; 
     atomicMass(19) = 39.098;  atomicMass(20) = 40.078; 
     atomicMass(21) = 44.956;  atomicMass(22) = 47.867; 
     atomicMass(23) = 50.942;  atomicMass(24) = 51.996; 
     atomicMass(25) = 54.938;  atomicMass(26) = 55.845; 
     atomicMass(27) = 58.933;  atomicMass(28) = 58.693; 
     atomicMass(29) = 62.546;  atomicMass(30) = 65.409; 
     atomicMass(31) = 69.723;  atomicMass(32) = 102.42;
     atomicMass(33) = 33.100;  atomicMass(34) = 78.960; 
     atomicMass(35) = 35.100;  atomicMass(36) = 36.100; 
     atomicMass(37) = 37.100;  atomicMass(38) = 38.100; 
     atomicMass(39) = 39.100;  atomicMass(40) = 40.100; 
     atomicMass(41) = 41.100;  atomicMass(42) = 42.100; 
     atomicMass(43) = 43.100;  atomicMass(44) = 44.100; 
     atomicMass(45) = 43.100;  atomicMass(46) = 102.42; 
     atomicMass(47) = 107.868;   atomicMass(48) = 112.411;

     ! init default valencies
     ! for elements where multiple valencies may occur,
     ! these are set to zero
     defaultValencies(1) = 1; defaultValencies(2) = 0; 
     defaultValencies(3) = 1; defaultValencies(4) = 2; 
     defaultValencies(5) = 3; defaultValencies(6) = 4; 
     defaultValencies(7) = 3; defaultValencies(8) = 2; 
     defaultValencies(9) = 1; defaultValencies(10) = 0; 
     defaultValencies(11) = 0; defaultValencies(12) = 0; 
     defaultValencies(13) = 3; defaultValencies(14) = 0; 
     defaultValencies(15) = 5; defaultValencies(16) = 2; 
     defaultValencies(17) = 1; defaultValencies(18) = 0; 
     defaultValencies(19) = 0; defaultValencies(20) = 2; 
     defaultValencies(21) = 0; defaultValencies(22) = 0; 
     defaultValencies(23) = 0; defaultValencies(24) = 0; 
     defaultValencies(25) = 0; defaultValencies(26) = 0; 
     defaultValencies(27) = 0; defaultValencies(28) = 0; 
     defaultValencies(29) = 0; defaultValencies(30) = 0; 
     defaultValencies(31) = 3; defaultValencies(32) = 0;
     defaultValencies(33) = 0; defaultValencies(34) = 0;
     defaultValencies(35) = 0; defaultValencies(36) = 0;
     defaultValencies(37) = 0; defaultValencies(38) = 0;
     defaultValencies(39) = 0; defaultValencies(40) = 0;
     defaultValencies(41) = 0; defaultValencies(42) = 0;
     defaultValencies(43) = 0; defaultValencies(44) = 0;
     defaultValencies(45) = 0; defaultValencies(46) = 0;
     defaultValencies(47) = 0; defaultValencies(48) = 0;

     ! init the atomic symbols
     atomicSymbols(1)  = "H"; atomicSymbols(2)   = "He"; 
     atomicSymbols(3)  = "Li"; atomicSymbols(4)  = "Be"; 
     atomicSymbols(5)  = "B"; atomicSymbols(6)   = "C"; 
     atomicSymbols(7)  = "N"; atomicSymbols(8)   = "O"; 
     atomicSymbols(9)  = "F"; atomicSymbols(10)  = "Ne"; 
     atomicSymbols(11) = "Na"; atomicSymbols(12) = "Mg"; 
     atomicSymbols(13) = "Al"; atomicSymbols(14) = "Si"; 
     atomicSymbols(15) = "P"; atomicSymbols(16)  = "S"; 
     atomicSymbols(17) = "Cl"; atomicSymbols(18) = "Ar"; 
     atomicSymbols(19) = "K"; atomicSymbols(20)  = "Ca"; 
     atomicSymbols(21) = "Sc"; atomicSymbols(22) = "V"; 
     atomicSymbols(23) = "Ti"; atomicSymbols(24) = "Cr"; 
     atomicSymbols(25) = "Mn"; atomicSymbols(26) = "Fe"; 
     atomicSymbols(27) = "Co"; atomicSymbols(28) = "Ni"; 
     atomicSymbols(29) = "Cu"; atomicSymbols(30) = "Zn"; 
     atomicSymbols(31) = "Ga"; atomicSymbols(32) = "X";
     atomicSymbols(33) = "X"; atomicSymbols(34) = "Se";
     atomicSymbols(35) = "X"; atomicSymbols(36) = "X";
     atomicSymbols(37) = "X"; atomicSymbols(38) = "X";
     atomicSymbols(39) = "X"; atomicSymbols(40) = "X";
     atomicSymbols(41) = "X"; atomicSymbols(42) = "X";
     atomicSymbols(43) = "X"; atomicSymbols(44) = "X";
     atomicSymbols(45) = "X"; atomicSymbols(46) = "Pd";
     atomicSymbols(47) = "Ag"; atomicSymbols(48) = "Cd"; 

     ! init the covalent radii ( in angstrom )
     covalentRadii(1)  = 0.23; covalentRadii(2)  = 0.32; 
     covalentRadii(3)  = 0.32; covalentRadii(4)  = 0.90; 
     covalentRadii(5)  = 0.82; covalentRadii(6)  = 0.77;
     covalentRadii(7)  = 0.75; covalentRadii(8)  = 0.70; 
     covalentRadii(9)  = 0.71; covalentRadii(10) = 0.71; 
     covalentRadii(11) = 1.54; covalentRadii(12) = 1.30;
     covalentRadii(13) = 1.18; covalentRadii(14) = 1.11; 
     covalentRadii(15) = 1.06; covalentRadii(16) = 1.02; 
     covalentRadii(17) = 0.99; covalentRadii(18) = 0.97;
     covalentRadii(19) = 1.96; covalentRadii(20) = 1.74; 
     covalentRadii(21) = 1.44; covalentRadii(22) = 1.36;
     covalentRadii(23) = 1.25; covalentRadii(24) = 1.27; 
     covalentRadii(25) = 1.39; covalentRadii(26) = 1.25; 
     covalentRadii(27) = 1.26; covalentRadii(28) = 1.21;
     covalentRadii(29) = 1.38; covalentRadii(30) = 1.31; 
     covalentRadii(31) = 1.26; covalentRadii(32) = 1.31;
     covalentRadii(33) = 1.26; covalentRadii(34) = 1.20;
     covalentRadii(35) = 1.26; covalentRadii(36) = 1.31;
     covalentRadii(37) = 1.26; covalentRadii(38) = 1.31;
     covalentRadii(39) = 1.26; covalentRadii(40) = 1.31;
     covalentRadii(41) = 1.26; covalentRadii(42) = 1.31;
     covalentRadii(43) = 1.26; covalentRadii(44) = 1.31;
     covalentRadii(45) = 1.26; covalentRadii(46) = 1.31;
     covalentRadii(47) = 1.20; covalentRadii(48) = 1.48;

     ! init the vdW radii ( in angstrom )
     vdWRadii(1)  = 1.20; vdWRadii(2)  = 1.40; 
     vdWRadii(3)  = 1.40; vdWRadii(4)  = 1.37; 
     vdWRadii(5)  = 1.00; vdWRadii(6)  = 1.70; 
     vdWRadii(7)  = 1.55; vdWRadii(8)  = 1.52; 
     vdWRadii(9)  = 1.47; vdWRadii(10) = 1.47; 
     vdWRadii(11) = 2.27; vdWRadii(12) = 1.73; 
     vdWRadii(13) = 1.70; vdWRadii(14) = 2.10; 
     vdWRadii(15) = 1.80; vdWRadii(16) = 1.80; 
     vdWRadii(17) = 1.75; vdWRadii(18) = 1.88; 
     vdWRadii(19) = 2.75; vdWRadii(20) = 2.45; 
     vdWRadii(21) = 1.37; vdWRadii(22) = 1.37; 
     vdWRadii(23) = 1.37; vdWRadii(24) = 1.37; 
     vdWRadii(25) = 1.37; vdWRadii(26) = 1.46; 
     vdWRadii(27) = 0.88; vdWRadii(28) = 1.63; 
     vdWRadii(29) = 1.39; vdWRadii(30) = 1.39; 
     vdWRadii(31) = 1.87; vdWRadii(32) = 1.63;
     vdWRadii(33) = 1.87; vdWRadii(34) = 1.90;
     vdWRadii(35) = 1.87; vdWRadii(36) = 1.63;
     vdWRadii(37) = 1.87; vdWRadii(38) = 1.63;
     vdWRadii(39) = 1.87; vdWRadii(40) = 1.63;
     vdWRadii(41) = 1.87; vdWRadii(42) = 1.63;
     vdWRadii(43) = 1.87; vdWRadii(44) = 1.63;
     vdWRadii(45) = 1.87; vdWRadii(46) = 1.63;
     vdWRadii(47) = 1.90; vdWRadii(48) = 1.60;

     ! init the double bond overlap  ( in percentage )
     dblOverlap(1)  = 0.0; dblOverlap(2)  = 0.0; 
     dblOverlap(3)  = 0.0; dblOverlap(4)  = 0.0; 
     dblOverlap(5)  = 0.0; dblOverlap(6)  = 0.0; 
     dblOverlap(7)  = 0.8; dblOverlap(8)  = 0.87; 
     dblOverlap(9)  = 0.0; dblOverlap(10) = 0.0; 
     dblOverlap(11) = 0.0; dblOverlap(12) = 0.0; 
     dblOverlap(13) = 0.0; dblOverlap(14) = 0.8; 
     dblOverlap(15) = 0.0; dblOverlap(16) = 0.0; 
     dblOverlap(17) = 0.0; dblOverlap(18) = 0.0; 
     dblOverlap(19) = 0.0; dblOverlap(20) = 0.0; 
     dblOverlap(21) = 0.0; dblOverlap(22) = 0.0; 
     dblOverlap(23) = 0.0; dblOverlap(24) = 0.0; 
     dblOverlap(25) = 0.0; dblOverlap(26) = 0.0; 
     dblOverlap(27) = 0.0; dblOverlap(28) = 0.0; 
     dblOverlap(29) = 0.0; dblOverlap(30) = 0.0; 
     dblOverlap(31) = 0.0; dblOverlap(32) = 0.0;
     dblOverlap(33) = 0.0; dblOverlap(34) = 0.0;
     dblOverlap(35) = 0.0; dblOverlap(36) = 0.0;
     dblOverlap(37) = 0.0; dblOverlap(38) = 0.0;
     dblOverlap(39) = 0.0; dblOverlap(40) = 0.0;
     dblOverlap(41) = 0.0; dblOverlap(42) = 0.0;
     dblOverlap(43) = 0.0; dblOverlap(44) = 0.0;
     dblOverlap(45) = 0.0; dblOverlap(46) = 0.0;
     dblOverlap(47) = 0.0; dblOverlap(48) = 0.0;

     ! init weak bond angles ( in radians )
     weakBondAngles(1)  = 0.6981317;  weakBondAngles(2)  = 0.00; 
     weakBondAngles(3)  = 0.00;       weakBondAngles(4)  = 0.00; 
     weakBondAngles(5)  = 0.00;       weakBondAngles(6)  = 0.00; 
     weakBondAngles(7)  = 0.6981317;  weakBondAngles(8)  = 1.221730; 
     weakBondAngles(9)  = 1.04719755; weakBondAngles(10) = 0.00; 
     weakBondAngles(11) = 0.00;       weakBondAngles(12) = 0.00; 
     weakBondAngles(13) = 0.00;       weakBondAngles(14) = 0.00; 
     weakBondAngles(15) = 0.00;       weakBondAngles(16) = 1.221730; 
     weakBondAngles(17) = 1.04719755; weakBondAngles(18) = 0.00; 
     weakBondAngles(19) = 0.00;       weakBondAngles(20) = 0.00; 
     weakBondAngles(21) = 0.00;       weakBondAngles(22) = 0.00; 
     weakBondAngles(23) = 0.00;       weakBondAngles(24) = 0.00; 
     weakBondAngles(25) = 0.00;       weakBondAngles(26) = 0.00; 
     weakBondAngles(27) = 0.00;       weakBondAngles(28) = 0.00; 
     weakBondAngles(29) = 0.00;       weakBondAngles(30) = 0.00; 
     weakBondAngles(31) = 0.00;       weakBondAngles(32) = 0.00;
     weakBondAngles(33) = 0.00;       weakBondAngles(34) = 0.00;
     weakBondAngles(35) = 0.00;       weakBondAngles(36) = 0.00;
     weakBondAngles(37) = 0.00;       weakBondAngles(38) = 0.00;
     weakBondAngles(39) = 0.00;       weakBondAngles(40) = 0.00;
     weakBondAngles(41) = 0.00;       weakBondAngles(42) = 0.00;
     weakBondAngles(43) = 0.00;       weakBondAngles(44) = 0.00;
     weakBondAngles(45) = 0.00;       weakBondAngles(46) = 0.00;
     weakBondAngles(47) = 0.00;       weakBondAngles(48) = 0.00;
   end subroutine initAtomInfo

   !! 
   !! get the atom number for the given symbol
   subroutine getAtomicNumber(symbol, atomicNumber)
      use m_datatypes

      implicit none
      
      integer(kind=SP)  :: i, atomicNumber
      character(len=CH) :: symbol

      do i = 1, NUMBER_OF_ELELEMTS
         if (symbol .eq. atomicSymbols(i)) then
            atomicNumber = i
            return
         endif
      enddo

   end subroutine getAtomicNumber

end module m_atomInfo


!! 
!! Keeps track of all the input data 
module m_input
use m_datatypes
implicit none

! type of bonds
integer(kind=SP) :: SINGLE_BOND, MULTIPLE_BOND, WEAK_BOND, NO_BOND
parameter(SINGLE_BOND = 1, MULTIPLE_BOND = 2, WEAK_BOND = -1, NO_BOND = 0)

! maximum number of bonds we can have
integer(kind=SP) :: MAXIMUM_NUMBER_OF_BONDS
parameter(MAXIMUM_NUMBER_OF_BONDS = 500)

!! the point type
type t_point3d
   real(kind=DP)     :: x, y, z      ! the coordinates
endtype t_point3d

!! the atom type
type t_atom
   integer(kind=SP)  :: index        ! index of this atom
   character(len=CH) :: symbol       ! atom symbol 
   integer(kind=SP)  :: atomicNumber ! atomic number of this atom
   real(kind=DP)     :: x, y, z      ! and the atom centers
   logical(kind=SP)  :: isDummy      ! is it a dummy atom ?

   ! bonded to which atoms
   integer(kind=SP), dimension(MAXIMUM_NUMBER_OF_BONDS) :: bondedAtoms
   integer(kind=SP), dimension(MAXIMUM_NUMBER_OF_BONDS) :: bondTypes

   integer(kind=SP) :: noOfBonds
   integer(kind=SP) :: noOfWeakBonds
endtype t_atom

! ring constants
integer(kind=SP) :: MAXIMUM_ATOMS_IN_RING
parameter(MAXIMUM_ATOMS_IN_RING = 500)

!! the ring type
type t_ring
   ! stores atom indices indicating a ring
   integer(kind=SP), dimension(MAXIMUM_ATOMS_IN_RING) :: ringAtoms
   
   integer(kind=SP) :: noOfRingAtoms
  
   logical(kind=SP) :: isPlanar
endtype t_ring

!! the molecule ... as a list of atoms, and list of rings...
type(t_atom), allocatable, dimension(:) :: atoms
integer(kind=SP), allocatable, dimension(:) :: sortedAtomIndices  ! on nearness to the center of mass
integer(kind=SP) :: numberOfAtoms ! the number of atoms in above list
type(t_ring), allocatable, dimension(:) :: rings
integer(kind=SP) :: numberOfRings ! number of rings in the atom list

integer(kind=SP) :: MAX_ATOM_TYPES
parameter(MAX_ATOM_TYPES = 48)

!! the formula type
type t_formula
   character(len=CH) :: symbol       ! atom symbol 
   integer(kind=SP)  :: items        ! number of these symbols
endtype t_formula
type(t_formula), dimension(MAX_ATOM_TYPES) :: molecularFormula, cardinalMolecularFormula

!! number of unique atoms & bonds
integer(kind=SP) :: numberOfUniqueAtoms, numberOfUniqueBonds

!! the bond stats type
type t_bondStats
   character(len=CH) :: symbol1, symbol2       ! atom symbol 
   integer(kind=SP)  :: items                  ! number of these symbols
endtype t_bondStats
type(t_bondStats), dimension(MAX_ATOM_TYPES) :: molecularBondFormula, cardinalMolecularBondFormula

!! the max contraction map
integer(kind=SP), allocatable, dimension(:) :: contractionMap

!! the number of dummy contractions
integer(kind=SP) :: dummyContraction

!! the charges on fragments
integer(kind=SP), allocatable, dimension(:) :: fragmentCharges, fragmentMultiplicity
end module m_input

!!
!! keeps all the graph related info.
!! .. like temporary arrays needed by graph algorithms
module m_graph
use m_datatypes
implicit none

   integer(kind=SP), allocatable, dimension(:) :: color, parent
   logical(kind=SP), allocatable, dimension(:) :: visited
end module m_graph

!!
!! Keeps track of unit numbers
module m_units
use m_datatypes
implicit none

integer(kind=SP) :: u_inp, u_out, u_xml, u_xyz, u_frg, u_lst, u_key, u_car, u_mainout
end module m_units


!! 
!! Keeps track of standard string constants
module m_strings
use m_datatypes
implicit none

character(len=8)   :: inputFileName    = "frag.inp"
character(len=8)   :: chargeFileName   = "charges"
character(len=8)   :: keyFileName      = "frag.key"
character(len=9)   :: fragListFileName = "frag.list"

character(len=8)   :: tabString     = "        "

character(len=80)  :: xmlHeader     = '<?xml version="1.0" standalone="yes" ?>'
character(len=80)  :: xmlComment    = &
& '<!-- Automatically generated for: MeTA Studio v2.0 (c) V.Ganesh, 2004 -->'

character(len=255) :: prefix      ! the input prefix string
integer(kind=SP)   :: iPrefixSize ! size of the above string

character(len=255) :: outfile      ! the main output file
integer(kind=SP)   :: outfilesize ! size of the above string

end module m_strings

!!
!! Keeps track of other constants
module m_constants
use m_datatypes
implicit none

! initlilizers
integer(kind=SP) :: zero_int    = 0
real(kind=SP)    :: zero_float  = 0.0d0
real(kind=DP)    :: zero_double = 0.0d0

! bonding constants
real(kind=DP)    :: COVALENT_BOND_TOLERANCE
parameter(COVALENT_BOND_TOLERANCE = 0.4) ! angstroms

real(kind=DP)    :: DOUBLE_BOND_OVERLAP_PERCENTAGE
parameter(DOUBLE_BOND_OVERLAP_PERCENTAGE = 0.92) ! 92 percentage

real(kind=DP)    :: WEAK_BOND_TOLERANCE_LOWER
parameter(WEAK_BOND_TOLERANCE_LOWER = 0.1) ! angstroms

real(kind=DP)    :: WEAK_BOND_TOLERANCE_UPPER
parameter(WEAK_BOND_TOLERANCE_UPPER = 1.05) ! angstroms

real(kind=DP)    :: WEAK_BOND_AXIS_REJECTION_FACTOR
parameter(WEAK_BOND_AXIS_REJECTION_FACTOR = 0.1)

! constants used in graph algorithms, like for cycle detection
integer(kind=SP) :: WHITE, GRAY, BLACK, RED
parameter(WHITE = 0, GRAY = 1, BLACK = 2, RED = 3)

! for checking ring planarity
real(kind=DP) :: TORSSIAN_ANGLE_TOLERANCE
parameter(TORSSIAN_ANGLE_TOLERANCE = 0.087266462599716474 ) ! 5 degrees

end module m_constants


!!
!! the fragment module, stores info. pertaining to fragments,
!! fragmentation
module m_fragment
use m_datatypes
use m_input, only : t_point3d, t_atom, t_formula, t_bondStats, MAX_ATOM_TYPES

implicit none

! some fragment constants
integer(kind=SP) :: MAX_ATOMS_IN_FRAGMENT
integer(kind=SP) :: MAX_DUMMY_ATOMS
parameter(MAX_ATOMS_IN_FRAGMENT = 200)
parameter(MAX_DUMMY_ATOMS = 200)

! for representing a fragment
type t_fragment
   ! indexes into the actual molecule, which form the fragment
   integer(kind=SP), dimension(MAX_ATOMS_IN_FRAGMENT) :: atomIndices
   type(t_atom), dimension(MAX_DUMMY_ATOMS)           :: dummyAtoms
   type(t_formula), dimension(MAX_ATOM_TYPES)         :: molecularFormula
   type(t_bondStats), dimension(MAX_ATOM_TYPES)       :: molecularBondFormula
   type(t_point3d)  :: center, maxBound, minBound
   real(kind=DP)    :: radius
   integer(kind=SP) :: numberOfTotalAtoms ! total atoms (only actual)
   integer(kind=SP) :: numberOfDummyAtoms ! number of dummy atoms
   integer(kind=SP) :: numberOfBoundaryAtoms ! number of boundary atoms
   integer(kind=SP) :: centeredOn ! centered on which atom?
   integer(kind=SP) :: numberOfContractions ! number of contractions here
   integer(kind=SP) :: numberOfFreezedAtom  ! number of atoms not intended to be moved :)
   integer(kind=SP) :: numberOfUniqueAtoms, numberOfUniqueBonds ! atoms, bonds
   integer(kind=SP) :: signOfEnergy ! sign of energy, could be > 1 or < -1
endtype t_fragment

! and the list of fragments
type(t_fragment), allocatable, dimension(:) :: fragments

! the fragment atom map, an atom is picked from which fragment?
integer(kind=SP), allocatable, dimension(:) :: fragmentAtomMap

! the fragment atom pair map, an atom pair is picked from which fragment?
integer(kind=SP), allocatable, dimension(:,:) :: fragmentAtomPairMap

! this is used to maintain the goodness of an atom from the fragment
! which it is picked, on a temporary basis as the algorithm proceeds
real(kind=DP), allocatable, dimension(:) :: atomGoodnessMap

! this is used to keep track of current fragment positions
! used while sorting 
integer(kind=SP), allocatable, dimension(:) :: currentFragmentPositions

! the number of fragments
integer(kind=SP) :: numberOfFragments, numberOfOverlapFragments

! the goodness stuff
integer(kind=SP) :: minGoodnessIndex, minNonPendantIndex
real(kind=DP)    :: minGoodness, minNonPendantGoodness
logical(kind=SP) :: readCharge
end module m_fragment

!!
!! keep track of all the options
module m_options
use m_datatypes
implicit none

! some control flags
logical(kind=SP) :: isOptimizationRun, usePrevDM, directFragSCF, useMultiThreading, &
&                   useDDIMemory, fuzzyConv, armyGrid, verbose, isPointCharge
logical(kind=SP) :: readExternally, wheelWithInWheel, isDifferentFrag, doAddDummyAtoms, &
&                   freezAtoms, isDFTRun, doSelectedGradients
logical(kind=SP) :: helFeyCorrection, setBasedEnergy, setBasedGradients, isHessianRun, &
&                   hydrogenAtOrigin, doFragDIIS, setBasedDM, setBasedCharges, cutRing
logical(kind=SP) :: isSelgrad 

logical(kind=SP) :: isFinalFrags   ! final set of fragments 

integer(kind=SP) :: optimizationStep ! which optimization step

integer(kind=SP) :: minFragmentSize, maxFragmentSize ! fragment size control

integer(kind=SP) :: mpLevel ! level of Moller Plesset

real(kind=DP)    :: radiusCutoff  ! the radius cut off of the fragments

! the basis set of the job
character(len=80) :: basisSet

! if dft run, its type
character(len=80) :: dftType

! the type of scf, RHF, UHF or ROHF
character(len=8) :: scfType

! options for running the fragment jobs
integer(kind=SP) :: timeLimit, memoryInWords, ddiMemoryInWords, maxSCFIterations
real(kind=DP)    :: energyTolerance, integralCutoff, densityConvergence

! gradient selection array
integer(kind=SP), allocatable, dimension(:) :: gradSelArray, fragDoGrad

end module m_options


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
