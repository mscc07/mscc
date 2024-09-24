!
!! frag.f90
!!
!! The heart of all fragmentation activities
!!
!! @author V.Ganesh
!! @version 2.0
!!

!!
!! The main subroutine to start fragmentation 
!!
!! isor  : is it a part of optimization run?
!! ishes : is it a hessian run?
!! nserch: in a optmize run which geometry are we at?
!! useDM : use the previous DM?
!! dirFrg: direct SFC for fragments?
!! useMT : use multithreaded code?
!! rdExt : read externally?
!! www   : wheel with in a wheel ?
!! radCut: radius cutoff
!! mplvl : MP2 or heigher level
!! frgetl: fragment SCF energy tolerance
!! frgitl: fragment SCF integral tolerance
!! frgdtl: fragment SCF density tolerance
!! memfrg: memory for fragment SCF run
!! addum : add dummy atoms to the fragments ?
!! useddi: use parallel runs for fragments ?
!! mddifr: distributed memory for parallel run
!! frzatm: freeze the unnecessary atoms in an optimization run
!! icntmap: contraction map
!! frfuz  : fuzzy convergence?
!! isdft  : is it a dft run?
!! selgrd : compute only selected gradients?
!! armygd : use army grid for DFT?
!! verbse : verbose output?
!! helcor : do hel-fey correction?
!! setene : do cardinality based energy evaluation
!! setgrd : do cardinality based gradeint evaluation
!! hydorg : place hydrogens at original atoms position?
!! dodiis : do DIIS for fragments?
!! pchrg  : point charge?
!! setdm  : set based DM?
!! setcrg : set based charge evaluation?
!! radthr : radius threashold. If the min R-goodness at any point in optimization falls below 
!!          by this much amount from the original R goodness then refragment
!! ctring : cut the rings? the planar rings are not usually cut. setting this option to .true.
!!          will make the ring detection algorithm make all rings appear as non-planar and hence
!!          they become elegible to cut
!! strscf : the type of the scf being run, RHF, UHF or ROHF
!! maxvt  : maximum number of SCF iterations
subroutine FRGMNT(isor, ishes, nserch, useDM, dirFrg, useMT, rdExt, www, radCut, mplvl, frgetl, frgitl, frgdtl, &
                  & memfrg, addum, useddi, mddifr, frzatm, icntmap, frfuz, isdft, selgrd, forcdm, armygd, verbse, helcor, &
                  & setene, setgrd, hydorg, dodiis, pchrg, setdm, setcrg, radthr, ctring, strscf, maxvt, isatmgrd, isgrdcl)
use m_datatypes
use m_input
use m_units
use m_strings
use m_options
use m_fragment
use m_collate, only : collate, addDummyAtoms
use m_cardinal, only: makeCardinalityExpression
use m_atomInfo, only : initAtomInfo
use m_utilityFunctions, only : getUnit, writeGAMESSInputFiles
use m_utilityFunctions, only : writeFragments, writeFragmentsAsXYZ, writePickInfo, makeFormule

implicit none

logical(kind=SP) :: isor, ishes, useDM, dirFrg, useMT, rdExt, www, addum, useddi, setene, setgrd, setdm, setcrg
logical(kind=SP) :: frzatm, frfuz, isdft, selgrd, forcdm, armygd, verbse, helcor, hydorg, dodiis, pchrg, ctring
logical(kind=SP) :: isatmgrd
integer(kind=SP) :: nserch, mplvl, memfrg, mddifr, i, maxvt
real(kind=DP) :: radCut, frgetl, frgitl, frgdtl, radthr
real(kind=DP) :: orgRGoodness
integer(kind=SP), dimension(*) :: icntmap, isgrdcl

save orgRGoodness

character(len=255) gtprefx, gtmainout
character(len=8) strscf

   !write(*,*) ',', strscf, ',', maxvt, isdft, addum

   ! get the prefix string
   prefix = GTPREFX(iPrefixSize) 

   ! get main output filename
   outfile = GTMAINOUT(outfilesize)

   ! setup the options
   isOptimizationRun  = isor
   usePrevDM          = useDM
   directFragSCF      = dirFrg
   useMultiThreading  = useMT
   readExternally     = rdExt
   wheelWithInWheel   = www
   optimizationStep   = nserch
   radiusCutoff       = radCut
   mpLevel            = mplvl
   energyTolerance    = frgetl
   integralCutoff     = frgitl
   densityConvergence = frgdtl
   isDifferentFrag    = ((nserch .gt. 0) .and. (useDM .or. www)) .or. forcdm
   memoryInWords      = memfrg
   doAddDummyAtoms    = addum
   useDDIMemory       = useddi
   ddiMemoryInWords   = mddifr
   freezAtoms         = frzatm
   fuzzyConv          = frfuz
   isDFTRun           = isdft
   doSelectedGradients= selgrd
   armyGrid           = armygd
   verbose            = verbse 
   isPointCharge      = pchrg
   helFeyCorrection   = helcor
   setBasedEnergy     = setene
   setBasedGradients  = setgrd
   isHessianRun       = ishes
   hydrogenAtOrigin   = hydorg
   doFragDIIS         = dodiis
   setBasedDM         = setdm
   setBasedCharges    = setcrg
   cutRing            = ctring
   scfType            = trim(strscf)
   maxSCFIterations   = maxvt
   isSelGrad          = isatmgrd

   isFinalFrags       = .false.

   ! check correctness of call
!   if (isdft .and. (mplvl.gt.1)) then
!      write(*,*) 'Can not do DFT and MP2 calculations together, options incompatible'
!      write(*,*) 'ABORTING execution!!'
!      call ENDING
!      stop
!   endif

   ! initilise the unit numbers
   u_inp = getUnit(.true.)
   u_out = getUnit()
   u_xml = getUnit()
   u_xyz = getUnit()
   u_frg = getUnit()
   u_lst = getUnit()
   u_key = getUnit()
   u_car = getUnit()   
   u_mainout = getUnit()

!   if (isdft) then
!      write(*,*) 'This is MTA-DFT run.'
!   else if (mplvl.gt.1) then
!      write(*,*) 'This is MTA-MP2 run.'
!   else 
!      write(*,*) 'This is MTA-HF run.'
!   endif

!   write(*,*) 'MTA SCF Type is: ', trim(scfType)

!   if (isHessianRun) write(*,*) 'This is MTA-HESSIAN run.'

!   if (doFragDIIS) write(*,*) 'Fragment SCF will be DIIS enabled'

   open (u_mainout, file=outfile(1:outfilesize), access='append')
   ! initilise the atom info
   call initAtomInfo()
   write(u_mainout,*) 'Inited atom info...'

   ! read in the input
   call readInput()
   write(u_mainout,*) 'Reading input complete..'
   write(u_mainout,*)

   ! then set up gradient selection array if requested
   if (isSelGrad) then 
     allocate(gradSelArray(numberOfAtoms))
     write(u_mainout,*)'GRADSELECTION ARRAY:'
     do i=1, numberOfAtoms
        gradSelArray(i) = isgrdcl(i)
        write(u_mainout,*)gradSelArray(i)
     enddo
   endif

   ! then make up the max contractions 
   allocate(contractionMap(numberOfAtoms))
   do i=1, numberOfAtoms
      contractionMap(i) = icntmap(i)
   enddo

!ANUJA   write(u_mainout,*) 'Contractions per atom :'
!ANUJA   do i=1, numberOfAtoms
!ANUJA      write(u_mainout,*) contractionMap(i)
!ANUJA   enddo

   ! detect the number of contractions
   call setDummyContraction()

   ! do preprocessing ... set up bonds etc.
   call preproc()
   write(u_mainout,*) 
   write(u_mainout,*) 'Preprocessing over..'
   !Print*,"SUCCESS IN FRAG.f90" 

   ! fragment .. or read previous fragments
   if (readExternally) then
      call readFragmentsExternally()
! isFinalFrags is true for writing the atom goodness index 
      isFinalFrags = .true.
      call writePickInfo()
      orgRGoodness = minGoodness
      write(u_mainout,*) 'Externally read fragments... (.lst file)'
   else if (.not. isDifferentFrag) then
      call collate()
      orgRGoodness = minGoodness
      write(u_mainout,*) 'Collation over...'
      write(u_mainout,*) 
   else 
      call readFromKey()
! isFinalFrags is true for writing the atom goodness index 
      isFinalFrags = .true.
      call writePickInfo()
      write(u_mainout,*) 'Externally read fragments... (.key file)'

      if (minGoodness .lt. orgRGoodness) then
         if ((orgRGoodness-minGoodness) .gt. radthr) then
            write(u_mainout,*) 'WARNING: User specified threashold of R-Goodness ', &
           &            radthr, ' violated.'

            call initFragArrays()
            write(u_mainout,*) 'Now refragmenting the system....'
            call collate()

            if (minGoodness .gt. orgRGoodness) then
               orgRGoodness = minGoodness
            endif

            write(u_mainout,*) 'Collation over...'

            write(u_mainout,*) 'Will not use previous DM for this run.'
            isDifferentFrag = .false.
         endif
      endif
   endif   

   ! set up the cardinality run if required
   if (setene .or. setgrd .or. setdm .or. setcrg) then
      write(u_mainout,*) 'Cardinality expression setup...'
      call makeCardinalityExpression()

      if (doAddDummyAtoms) then
         write(u_mainout,*) 'Adding dummy atoms to overlap fragments ...'
         do i = 1, numberOfOverlapFragments
            call addDummyAtoms(i+numberOfFragments)
         enddo
         write(u_mainout,*) '... Adding over'
      else
         write(u_mainout,*) 'No dummy atoms were added!'
      endif   
   else
      open(u_car, file=prefix(1:iPrefixSize) // 'carsign')
      write(u_car, *) '0'
      close(u_car)
   endif

   ! check multiplicity
   call checkMultiplicity()
   
   write(u_mainout,*) 'Sorting fragments on size...'
   ! sort the stuff
   call sortFragmentsOnSize()

   write(u_mainout,*) 'Setting up addtional parameters...'
   ! write the gamess i/p files
   call setupAdditionalOptions()  

   write(u_mainout,*) 'Writing i/p files ...'
   call writeGAMESSInputFiles()
   write(u_mainout,*) 'Writing i/p files over...'

   if (readCharge) write(u_mainout,*)"WARNING: Some fragments have charge."
   ! dump final info
!ANUJA   write(u_mainout,*) 'FINAL FRAGMENTS:', numberOfFragments
   write(u_mainout,*)
   write(u_mainout,*) 'FINAL FRAGMENTS:'
! isFinalFrags is true for writing the summary of fragments in the main output
   call writeFragments("fragD.list")
   call writeFragmentsAsXYZ("stage8.xyz")     

   ! write the out put files and analysis
   call makeFormule()
   
   ! clean up
   call deallocateArrays()

   close(u_mainout)

end subroutine FRGMNT


!! 
!! read in the input information
subroutine readInput()
use m_datatypes
use m_input
use m_units
use m_strings
use m_options
use m_utilityFunctions
use m_atomInfo, only : getAtomicNumber
use m_utilityFunctions, only : capitalise, toLowerCase

implicit none

integer(kind=SP)  :: istat, i
character(len=80) :: dummy

   ! open the input file for processing 
   open(u_inp, file=prefix(1:iPrefixSize)//inputFileName, status="old", iostat=istat)

   if (istat .ne. 0) then
      write(u_mainout,*) 'Could not read input file: ', prefix(1:iPrefixSize)//inputFileName
      write(u_mainout,*) 'Will abort!'
      call ABRT()
   endif

   ! read in the atom geometry
   read(u_inp, *, iostat=istat) minFragmentSize, maxFragmentSize     ! options here
   print*, "Minimum ",  minFragmentSize, "Maximum ", maxFragmentSize ! options here ssk
   call checkIOError(istat, prefix(1:iPrefixSize)//inputFileName)
   read(u_inp, '(A80)', iostat=istat) basisSet                       ! basis set here
   call checkIOError(istat, prefix(1:iPrefixSize)//inputFileName)

   ! if dft run, then read dft type
   if (isDFTRun) then
      read(u_inp, '(A80)', iostat=istat) dftType                     ! dft type here
      call checkIOError(istat, prefix(1:iPrefixSize)//inputFileName)
   endif

   read(u_inp, *, iostat=istat) numberOfAtoms                        ! number of atoms
   call checkIOError(istat, prefix(1:iPrefixSize)//inputFileName)
   read(u_inp, *, iostat=istat) dummy                                ! title line
   call checkIOError(istat, prefix(1:iPrefixSize)//inputFileName)

   write(u_mainout,'(" Total number of atoms are : ", i5)') numberOfAtoms

   ! make all the basis set string to be lower case
   call toLowerCase(basisSet)

   ! initialize the memory
   call initializeArrays()

   ! and then do the actual reading
   do i = 1, numberOfAtoms
      atoms(i)%index = i

      read(u_inp, *, iostat=istat) atoms(i)%symbol, atoms(i)%x, atoms(i)%y, atoms(i)%z
      call checkIOError(istat, prefix(1:iPrefixSize)//inputFileName)

      call capitalise(atoms(i)%symbol)
      call getAtomicNumber(atoms(i)%symbol, atoms(i)%atomicNumber)
   end do

   ! close the input file
   close(u_inp)


   ! reorder the i/p data (the atoms) so that we always get 
   ! the same fragments irrespective of the order in with the 
   ! atom coordinates are provided.
   ! Note: this "loophole" was suggested by Alistair Rendell, ANU
   call reorderAtoms()

   return
end subroutine readInput

!!
!! reorder that atom based on nearness to the center of mass.
!! This reordering is required as we *always* want the same set
!! of fragments to be generated irrespective of the order in  
!! which the input coordinates are provided.
!! 
!! Appears to be a simple but working solution for reproducibility
!! of results.
subroutine reorderAtoms()
use m_datatypes
use m_input
use m_units
use m_strings
use m_options
use m_atomInfo
use m_utilityFunctions

implicit none

integer(kind=SP) :: i, j, tmp
real(kind=DP) :: sumOfMass, mass, x, y, z
type(t_point3d) :: centerOfMass
real(kind=DP), allocatable, dimension(:) :: distances

!ANUJA   write(u_mainout,*) 'Attempting reoreder...'

   centerOfMass%x = 0.0
   centerOfMass%y = 0.0
   centerOfMass%z = 0.0
   sumOfMass = 0.0

   ! find some center (actually center of mass)
   do i = 1, numberOfAtoms
      mass = atomicMass(atoms(i)%atomicNumber)
      centerOfMass%x = centerOfMass%x + (mass * atoms(i)%x)
      centerOfMass%y = centerOfMass%y + (mass * atoms(i)%y)
      centerOfMass%z = centerOfMass%z + (mass * atoms(i)%z)
      sumOfMass = sumOfMass + mass
   enddo

!ANUJA   write(u_mainout,*) centerOfMass, sumOfMass

   centerOfMass%x = centerOfMass%x / sumOfMass
   centerOfMass%y = centerOfMass%y / sumOfMass
   centerOfMass%z = centerOfMass%z / sumOfMass

   ! calculate distance of each atom from the center of mass
   allocate(distances(numberOfAtoms))
   do i = 1, numberOfAtoms
      x = atoms(i)%x - centerOfMass%x
      y = atoms(i)%y - centerOfMass%y
      z = atoms(i)%z - centerOfMass%z

      distances(i) = x*x + y*y + z*z
   enddo

   ! then create a new sort map so that we can use this
   ! new order for traversal only, the original indices
   ! remain unchanged
   do i = 1, numberOfAtoms
      sortedAtomIndices(i) = i
   enddo

   ! do a simple sort
   do i = 1, numberOfAtoms
      do j = i+1, numberOfAtoms
         if (distances(j) < distances(i)) then
            x = distances(j)
            distances(j) = distances(i)
            distances(i) = x

            tmp = sortedAtomIndices(j)
            sortedAtomIndices(j) = sortedAtomIndices(i)
            sortedAtomIndices(i) = tmp
         endif
      enddo
   enddo

!ANUJA   write(u_mainout,*) 'After reorder...'
!ANUJA   write(u_mainout,*) 'Indices: ', sortedAtomIndices(1:numberOfAtoms)
!ANUJA   write(u_mainout,*) 'Distances: ', distances(1:numberOfAtoms)
 
   deallocate(distances)
end subroutine reorderAtoms

!!
!! readExternally() - read the fragments externally from 
!! [input-]frag.key file
subroutine readFromKey()
use m_datatypes
use m_input
use m_units
use m_options
use m_strings
use m_collate
use m_fragment
use m_utilityFunctions

implicit none

integer(kind=SP) :: i, fragIndex, atomIndex, istat

    ! read key file
    open(u_key, file=prefix(1:iPrefixSize) // keyFileName, status="old", iostat=istat)

    if (istat .ne. 0) then
       write(u_mainout,*) 'Could not read key file: ', prefix(1:iPrefixSize)//keyFileName
       flush(u_mainout)
       write(u_mainout,*) 'Will abort!'
       flush(u_mainout)
       call ABRT()
    endif
    
    read(u_key, *, iostat=istat) numberOfFragments
    call checkIOError(istat, prefix(1:iPrefixSize)//keyFileName)

    ! for each line in [input-]frag.key
    ! read it as a fragment! - no dummy atoms!
    do fragIndex = 1, numberOfFragments
       read(u_key, *, iostat=istat) fragments(fragIndex)%numberOfTotalAtoms
       call checkIOError(istat, prefix(1:iPrefixSize)//keyFileName)

       do i = 1, fragments(fragIndex)%numberOfTotalAtoms
          read(u_key,*, iostat=istat) atomIndex 
          call checkIOError(istat, prefix(1:iPrefixSize)//keyFileName)
 
          !! unable to map atomindices ... stop execution!
          if (atomIndex .eq. -1) then
             write(*, '("Fragment no. :", I4, " Atom index : ", I4)') fragIndex, i
             write(*, '("Aborting! cannot map atom indices!!")')
             call ABRT
          endif    

          fragments(fragIndex)%atomIndices(i) = atomIndex
       enddo
    enddo     
    
    ! close files
    close(u_key)

    ! before comming out, do atom pair selections
    call setupAtomPairMap()

    ! and add dummy atoms
    if (doAddDummyAtoms) then
       do i = 1, numberOfFragments
          call addDummyAtoms(i)
       enddo
    endif

    call writeFragments("frag.list")    
end subroutine readFromKey

!! 
!! This subroutine reads the fragment coordinates externally and 
!! appropriately initilises the files needed for doing the posterior 
!! analysis
subroutine readFragmentsExternally()
use m_datatypes
use m_input
use m_units
use m_strings
use m_options
use m_collate
use m_fragment
use m_constants
use m_utilityFunctions
use m_utilityFunctions, only: getUnit

implicit none

integer(kind=SP) :: u_fragList, fragIdx, atmIdx, atomIndex
integer(kind=SP) :: getIndexOfAtom, i, istat

character(len=CH) :: symbol
real(kind=DP) :: x, y, z

    ! read frag.list
    u_fragList = getUnit()

    open(u_fragList, file=prefix(1:iPrefixSize) // fragListFileName, status="old", iostat=istat)

    if (istat .ne. 0) then
       write(u_mainout,*) 'Could not read fragment list file: ', prefix(1:iPrefixSize)//fragListFileName
       write(u_mainout,*) "Will try to read from 'javakey' file!"

       call readFromKey()
       return       
    endif

    
    read(u_fragList, *, iostat=istat) numberOfFragments
    call checkIOError(istat, prefix(1:iPrefixSize)//fragListFileName)

    ! for each line in frag.lst
    ! read it as a fragment! - no dummy atoms!
    do fragIdx = 1, numberOfFragments
       read(u_fragList, *, iostat=istat) fragments(fragIdx)%numberOfTotalAtoms
       call checkIOError(istat, prefix(1:iPrefixSize)//fragListFileName)

       do atmIdx = 1, fragments(fragIdx)%numberOfTotalAtoms
          read(u_fragList,*, iostat=istat) symbol, x, y, z
          call checkIOError(istat, prefix(1:iPrefixSize)//fragListFileName)

          atomIndex = getIndexOfAtom(x, y, z)      
 
          !! unable to map atomindices ... stop execution!
          if (atomIndex .eq. -1) then
             write(u_mainout, '("Fragment no. :", I4, " Atom index : ", I4)') fragIdx, atmIdx
             write(u_mainout, '("Aborting! cannot map atom indices!!")')
             write(u_mainout,*) "Trying to read from 'javakey' file"

             close(u_fragList)

             call readFromKey()
             return
          endif    

          fragments(fragIdx)%atomIndices(atmIdx) = atomIndex
       enddo
    enddo     

    close(u_fragList)

    ! before comming out, do atom pair selections
    call setupAtomPairMap()

    ! and add dummy atoms
    if (doAddDummyAtoms) then
       do i = 1, numberOfFragments
          call addDummyAtoms(i)
       enddo
    endif

    call writeFragments("frag.list")    
end subroutine readFragmentsExternally

!!
!! get the index of this atom
function getIndexOfAtom(x, y, z)
use m_datatypes
use m_input
use m_utilityFunctions, only: findDistance

implicit none

real(kind=DP)    :: x, y, z, l, m, n
integer(kind=SP) :: i, getIndexOfAtom
logical(kind=SP) :: foundAtom

    foundAtom = .FALSE.
    do i = 1, numberOfAtoms
       l = x - atoms(i)%x
       m = y - atoms(i)%y
       n = z - atoms(i)%z

       if (sqrt(l*l + m*m + n*n) < 0.01) then
          foundAtom = .TRUE.
          exit
       endif
    enddo     
    
    if (.not. foundAtom) i = -1

    getIndexOfAtom = i
end function getIndexOfAtom

!!
!! initilises the memory for various datastructures
subroutine initializeArrays()
use m_datatypes
use m_input
use m_fragment
use m_constants

implicit none

integer(kind=SP) :: i

   allocate(atoms(numberOfAtoms))
   allocate(sortedAtomIndices(numberOfAtoms))
   allocate(rings(numberOfAtoms*5))
   allocate(fragments(numberOfAtoms*2))
   allocate(fragmentAtomMap(numberOfAtoms))
   allocate(atomGoodnessMap(numberOfAtoms))
   allocate(fragmentAtomPairMap(numberOfAtoms, numberOfAtoms))
   allocate(fragmentCharges(numberOfAtoms*2))
   allocate(fragmentMultiplicity(numberOfAtoms*2))

   do i = 1, numberOfAtoms
      atoms(i)%index  = zero_int
      atoms(i)%symbol = ""
      atoms(i)%x = zero_double
      atoms(i)%y = zero_double
      atoms(i)%z = zero_double
      atoms(i)%bondedAtoms   = zero_int
      atoms(i)%bondTypes     = zero_int
      atoms(i)%noOfBonds     = zero_int
      atoms(i)%noOfWeakBonds = zero_int
      atoms(i)%isDummy       = .false.

      rings(i)%ringAtoms     = zero_int
      rings(i)%isPlanar      = .false.
      rings(i)%noOfRingAtoms = zero_int

      fragments(i)%atomIndices           = zero_int
      fragments(i)%numberOfTotalAtoms    = zero_int
      fragments(i)%numberOfDummyAtoms    = zero_int
      fragments(i)%centeredOn            = zero_int
      fragments(i)%numberOfContractions  = zero_int
      fragments(i)%numberOfFreezedAtom   = zero_int
      fragments(i)%signOfEnergy          = 1
   end do
   
   numberOfRings       = zero_int
   numberOfFragments   = zero_int
   fragmentAtomMap     = zero_int
   atomGoodnessMap     = zero_int
   fragmentAtomPairMap = -1
   fragmentCharges     = zero_int
   fragmentMultiplicity= 1

end subroutine initializeArrays

!!
!! initFragArrays() - initilize only the frag arrays
subroutine initFragArrays
use m_input
use m_fragment
use m_constants

implicit none

integer(kind=SP) :: i

   deallocate(fragments, fragmentCharges, fragmentMultiplicity)
   deallocate(fragmentAtomMap, atomGoodnessMap, fragmentAtomPairMap)

   allocate(fragments(numberOfAtoms*2))
   allocate(fragmentAtomMap(numberOfAtoms))
   allocate(atomGoodnessMap(numberOfAtoms))
   allocate(fragmentAtomPairMap(numberOfAtoms, numberOfAtoms))
   allocate(fragmentCharges(numberOfAtoms*2))
   allocate(fragmentMultiplicity(numberOfAtoms*2))

   do i = 1, numberOfAtoms
      fragments(i)%atomIndices          = zero_int
      fragments(i)%numberOfTotalAtoms   = zero_int
      fragments(i)%numberOfDummyAtoms   = zero_int
      fragments(i)%centeredOn           = zero_int
      fragments(i)%numberOfContractions = zero_int
      fragments(i)%numberOfFreezedAtom  = zero_int
      fragments(i)%signOfEnergy         = 1
   end do

   numberOfFragments   = zero_int
   fragmentAtomMap     = zero_int
   atomGoodnessMap     = zero_int
   fragmentAtomPairMap = -1
   fragmentCharges     = zero_int
   fragmentMultiplicity= 1
end subroutine initFragArrays

!!
!! setup additional options for fragment SCF runs
subroutine setupAdditionalOptions()
use m_options
use m_fragment
use m_input

implicit none

integer(kind=SP) :: i, j, k

   timeLimit     = 10000000

   if (isSelGrad) then
     allocate(fragDoGrad(numberOfFragments+numberOfOverlapFragments))
     fragDoGrad = 0

     do i = 1,numberOfFragments+numberOfOverlapFragments
       do j = 1,fragments(i)%numberOfTotalAtoms
         do k = 1,numberOfAtoms 
            if ((fragments(i)%atomIndices(j).eq.k).and.(gradSelArray(k).eq.1)) fragDoGrad(i) = 1
         enddo
       enddo
     enddo
   endif

end subroutine setupAdditionalOptions

!!
!! sortFragmentsOnSize() - sort the fragments on size, only the main fragments
subroutine sortFragmentsOnSize()
use m_units
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: i, j, n1, n2, temp

    allocate(currentFragmentPositions(numberOfFragments+numberOfOverlapFragments))

    do i = 1, numberOfFragments+numberOfOverlapFragments
       currentFragmentPositions(i) = i
    enddo

    !! TODO: Skipping at the moment
    return 

    do i = 1, numberOfFragments
       n1 = fragments(currentFragmentPositions(i))%numberOfContractions

       do j = 1, i 
          n2 = fragments(currentFragmentPositions(j))%numberOfContractions

          if (n1 .gt. n2) then 
             temp                        = currentFragmentPositions(i)
             currentFragmentPositions(i) = currentFragmentPositions(j)
             currentFragmentPositions(j) = temp
          endif
       enddo
    enddo

    write(u_mainout,*) 'REORDING AS IN .......'
    do i = 1, numberOfFragments+numberOfOverlapFragments
       write(u_mainout,*) i , ">", currentFragmentPositions(i)
    enddo
end subroutine sortFragmentsOnSize

!!
!! checkMultiplicity() - check multiplicity of each fragment
!! if there is an error, stop execution
subroutine checkMultiplicity() 
use m_datatypes
use m_input
use m_units
use m_strings
use m_options
use m_atomInfo
use m_fragment
use m_utilityFunctions

implicit none

integer(kind=SP) :: i, j, multiplicity, istat, icharge, nBonds, nb, k, np, nn

   readCharge = .false. .or. (trim(scfType) .eq. 'UHF') .or. (trim(scfType) .eq. 'ROHF')

   write(u_mainout,*) "Those marked with '*' are having charge"
   do i = 1, numberOfFragments+numberOfOverlapFragments
      multiplicity = 0
      icharge = 0; np = 0; nn = 0
      do j = 1, fragments(i)%numberOfTotalAtoms
         multiplicity = multiplicity + atoms(fragments(i)%atomIndices(j))%atomicNumber

         nBonds = 0 
         nb = 0

         do k = 1, atoms(fragments(i)%atomIndices(j))%noOfBonds
            if (atoms(fragments(i)%atomIndices(j))%bondTypes(k).ne.WEAK_BOND) then
               nb = nb + atoms(fragments(i)%atomIndices(j))%bondTypes(k) 
            endif
         enddo
         ! TODO: check the correctness
         if (atoms(fragments(i)%atomIndices(j))%atomicNumber .le.  6) then
            nBonds = defaultValencies(atoms(fragments(i)%atomIndices(j))%atomicNumber) - nb 
         else
            nBonds = nb - defaultValencies(atoms(fragments(i)%atomIndices(j))%atomicNumber) 
         endif 

         !if (nBonds .gt. 0) then
         !   np = np + 1
         !else if (nBonds .lt. 0) then
         !   nn = nn + 1
         !endif
 
         ! write(*,*) fragments(i)%atomIndices(j), atoms(fragments(i)%atomIndices(j))%symbol, &
         ! &           nBonds, defaultValencies(atoms(fragments(i)%atomIndices(j))%atomicNumber)

         icharge = icharge + nBonds

         !write(*,*) np, nn
      enddo

      do j = 1, fragments(i)%numberOfDummyAtoms
         multiplicity = multiplicity + fragments(i)%dummyAtoms(j)%atomicNumber
      enddo

      if (mod(multiplicity, 2) .ne. 0) then
         write(u_mainout,*) i, multiplicity, icharge, '*'
         fragmentCharges(i) = icharge
         readCharge = .true.
      else 
         write(u_mainout,*) i, multiplicity
      endif
   enddo

   if (readCharge) then
      write(u_mainout,*) 'As some fragments are charged, I will attempt to read all charges form:', &
     &           prefix(1:iPrefixSize)//chargeFileName
      write(u_mainout,*)

      open(u_inp, file=prefix(1:iPrefixSize)//chargeFileName, status="old", iostat=istat)

      if (istat .ne. 0) then
         write(u_mainout,*) 'Could not read charge input file: ', prefix(1:iPrefixSize)//chargeFileName
         write(u_mainout,*) 'Will use the charges computed internally'
         return
      endif

      ! read in the atom geometry
      do i = 1, numberOfFragments+numberOfOverlapFragments
         read(u_inp, *, iostat=istat) fragmentCharges(i), fragmentMultiplicity(i) 
         call checkIOError(istat, prefix(1:iPrefixSize)//chargeFileName)
      enddo

      close(u_inp)
   endif
end subroutine

!!
!! deallocate the arrays
subroutine deallocateArrays()
use m_input
use m_fragment
use m_options

implicit none

   deallocate(atoms, rings, fragments, fragmentCharges, fragmentMultiplicity, sortedAtomIndices)
   deallocate(fragmentAtomMap, atomGoodnessMap, fragmentAtomPairMap)

   if (allocated(currentFragmentPositions)) then 
      deallocate(currentFragmentPositions)
   endif

   if (allocated(gradSelArray)) deallocate(gradSelArray)
   if (allocated(fragDoGrad)) deallocate(fragDoGrad)

   deallocate(contractionMap)
end subroutine deallocateArrays

!!
!! set the dummy contraction
subroutine setDummyContraction()
  use m_input

  do i = 1, numberOfAtoms
     if (atoms(i)%symbol .eq. "H ") then
        dummyContraction = contractionMap(i)
        exit
     endif
  enddo
end subroutine setDummyContraction

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

