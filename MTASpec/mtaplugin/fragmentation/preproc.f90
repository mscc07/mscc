!!
!! preproc.f90
!!
!! All the pre processing is done here
!!
!! @author V.Ganesh
!! @version 2.0
!!

!!
!! This subroutine does the following:
!!  1) setup the connectivity
!!  2) identify the rings, and their types
!!
subroutine preproc
use m_input
use m_units
use m_fragment
use m_datatypes
use m_utilityFunctions, only : writeXML

implicit none

integer(kind=SP) :: i

   ! make the molecular graph
   call buildConnectivity()
   ! identify rings
   call identifyRings()

   ! write xml output of stage 1
   call writeXML("stage1.mwi")

end subroutine preproc

!!
!! confirmWeakBond() - confirm the existance of weak bond 
!! atomIndex1, atomIndex2 : the atom indices to check for
function confirmWeakBond(atomIndex1, atomIndex2)
use m_input
use m_options
use m_atomInfo
use m_constants
use m_datatypes
use m_utilityFunctions, only : isNullVector, magnitude, angleBetween

implicit none

logical(kind=SP) :: confirmWeakBond
integer(kind=SP) :: atomIndex1, atomIndex2
real(kind=DP)    :: angle1, angle2, cutoff1, cutoff2

type(t_point3d) :: axis1, axis2
type(t_point3d) :: vectorFrom1To2, vectorFrom2To1

interface computeAxis
   type(t_point3d) function computeAxis(atomIndex)
      use m_input
      use m_datatypes

      implicit none
         
      integer(kind=SP) :: atomIndex
   end function computeAxis
end interface computeAxis

   if (.not. doAddDummyAtoms) then
      confirmWeakBond = .false.
      return
   endif

   ! first handle obvious cases
   if ((atoms(atomIndex1)%noOfBonds .eq. 0) &
  &   .or. (atoms(atomIndex2)%noOfBonds .eq. 0)) then
      confirmWeakBond = .true.
      return
   endif

   if ((weakBondAngles(atoms(atomIndex1)%atomicNumber) .eq. 0.0) &
  &    .or. (weakBondAngles(atoms(atomIndex2)%atomicNumber) .eq. 0.0)) then
      confirmWeakBond = .false.
      return
   endif

   ! now since we are here, we need to be more elaborate
   axis1 = computeAxis(atomIndex1)
   axis2 = computeAxis(atomIndex2)

   ! null vectors??
   if (isNullVector(axis1) .or. isNullVector(axis2)) then
      confirmWeakBond = .false.    
      return
   endif

   ! discard near zero interactions
   if ((magnitude(axis1) .lt. WEAK_BOND_AXIS_REJECTION_FACTOR) &
  &   .or. (magnitude(axis2) .lt. WEAK_BOND_AXIS_REJECTION_FACTOR)) then
      confirmWeakBond = .false.
      return
   endif

   ! compute vectors connecting atoms
   vectorFrom1To2%x = atoms(atomIndex2)%x - atoms(atomIndex1)%x
   vectorFrom1To2%y = atoms(atomIndex2)%y - atoms(atomIndex1)%y
   vectorFrom1To2%z = atoms(atomIndex2)%z - atoms(atomIndex1)%z

   vectorFrom2To1%x = atoms(atomIndex1)%x - atoms(atomIndex2)%x
   vectorFrom2To1%y = atoms(atomIndex1)%y - atoms(atomIndex2)%y
   vectorFrom2To1%z = atoms(atomIndex1)%z - atoms(atomIndex2)%z

   ! find the two angles
   angle1 = angleBetween(axis1, vectorFrom1To2)
   angle2 = angleBetween(axis2, vectorFrom2To1)
   
   cutoff1 = weakBondAngles(atoms(atomIndex1)%atomicNumber)
   cutoff2 = weakBondAngles(atoms(atomIndex2)%atomicNumber)

   if ((angle1 .lt. cutoff1) .and. (angle2 .lt. cutoff2)) then 
      confirmWeakBond = .true.
      return         
   else
      confirmWeakBond = .false.
      return         
   endif

end function confirmWeakBond

!!
!! computeAxis() - method to compute the resultant axis w.r.t an atom 
!! center based on its connectivity
function computeAxis(atomIndex) 
use m_input
use m_atomInfo
use m_datatypes
use m_utilityFunctions, only : normalize

implicit none

integer(kind=SP) :: atomIndex, connectedAtom, i
type(t_point3d) :: axis, p1, computeAxis

   axis%x = 0.0; axis%y = 0.0; axis%z = 0.0;

   p1%x = atoms(atomIndex)%x
   p1%y = atoms(atomIndex)%y
   p1%z = atoms(atomIndex)%z

   do i = 1, atoms(atomIndex)%noOfBonds
      if (atoms(atomIndex)%bondTypes(i) .ne. WEAK_BOND) then
         connectedAtom = atoms(atomIndex)%bondedAtoms(i)

         axis%x = axis%x + (p1%x - atoms(connectedAtom)%x)
         axis%y = axis%y + (p1%y - atoms(connectedAtom)%y)
         axis%z = axis%z + (p1%z - atoms(connectedAtom)%z)
      endif
   enddo

   call normalize(axis)

   computeAxis = axis
   return
end function computeAxis

!! 
!! buildConnectivity() - build the molecular graph
subroutine buildConnectivity()
use m_input
use m_units
use m_atomInfo
use m_constants
use m_datatypes
use m_utilityFunctions, only : isStronglyBonded

implicit none

integer(kind=SP) :: i, j
real(kind=DP)    :: distance, radiusSum, x, y, z, dblOvr

interface confirmWeakBond
   function confirmWeakBond(atomIndex1, atomIndex2)
      use m_datatypes

      implicit none
      
      logical(kind=SP) :: confirmWeakBond
      integer(kind=SP) :: atomIndex1, atomIndex2
   end function confirmWeakBond
end interface confirmWeakBond

   ! first identify only strong bonds
   do i = 1, numberOfAtoms  ! loop over atom1
      do j = 1, i           ! loop over atom2
         x = atoms(i)%x - atoms(j)%x
         y = atoms(i)%y - atoms(j)%y
         z = atoms(i)%z - atoms(j)%z

         distance = sqrt(x*x + y*y + z*z)

         ! check for single bond
         radiusSum = covalentRadii(atoms(i)%atomicNumber) &
                     & + covalentRadii(atoms(j)%atomicNumber) 

         if (((radiusSum - COVALENT_BOND_TOLERANCE) < distance) &
        &    .and. (distance < (radiusSum + COVALENT_BOND_TOLERANCE))) then

            if (dblOverlap(atoms(i)%atomicNumber) .eq. 0) then
              if (dblOverlap(atoms(j)%atomicNumber) .eq. 0) then
                 dblOvr = DOUBLE_BOND_OVERLAP_PERCENTAGE
              else
                 dblOvr = dblOverlap(atoms(j)%atomicNumber)
              endif
            else
               dblOvr = dblOverlap(atoms(i)%atomicNumber)
            endif

            ! there is a single bond, now check if there exist a multi - bond
            if (distance < (dblOvr * radiusSum)) then
               ! confirmed multi - bond
               atoms(i)%noOfBonds = atoms(i)%noOfBonds + 1 
               atoms(i)%bondedAtoms(atoms(i)%noOfBonds) = j
               atoms(i)%bondTypes(atoms(i)%noOfBonds)   = MULTIPLE_BOND

               atoms(j)%noOfBonds = atoms(j)%noOfBonds + 1 
               atoms(j)%bondedAtoms(atoms(j)%noOfBonds) = i
               atoms(j)%bondTypes(atoms(j)%noOfBonds)   = MULTIPLE_BOND
            else
               ! confirmed single bond
               atoms(i)%noOfBonds = atoms(i)%noOfBonds + 1 
               atoms(i)%bondedAtoms(atoms(i)%noOfBonds) = j
               atoms(i)%bondTypes(atoms(i)%noOfBonds)   = SINGLE_BOND

               atoms(j)%noOfBonds = atoms(j)%noOfBonds + 1 
               atoms(j)%bondedAtoms(atoms(j)%noOfBonds) = i
               atoms(j)%bondTypes(atoms(j)%noOfBonds)   = SINGLE_BOND
            endif
         endif
      enddo
   enddo


   ! the identify weak bonds
   do i = 1, numberOfAtoms  ! loop over atom1
      do j = 1, i           ! loop over atom2
         ! first check if a bond already exist
         if (isStronglyBonded(i, j)) then
            cycle
         endif

         ! no bond? then check for weak interactions
         x = atoms(i)%x - atoms(j)%x
         y = atoms(i)%y - atoms(j)%y
         z = atoms(i)%z - atoms(j)%z

         distance = sqrt(x*x + y*y + z*z)

         ! check for weak bond
         radiusSum = vdWRadii(atoms(i)%atomicNumber) &
                     & + vdWRadii(atoms(j)%atomicNumber) 

         if ((distance < (radiusSum - WEAK_BOND_TOLERANCE_LOWER)) & 
        &    .and. (radiusSum - WEAK_BOND_TOLERANCE_UPPER) < distance) then            
            ! there may be a weak bond ... we need to confirm it
            if (confirmWeakBond(i, j)) then
               atoms(i)%noOfBonds = atoms(i)%noOfBonds + 1 
               atoms(i)%noOfWeakBonds = atoms(i)%noOfWeakBonds + 1 
               atoms(i)%bondedAtoms(atoms(i)%noOfBonds) = j
               atoms(i)%bondTypes(atoms(i)%noOfBonds)   = WEAK_BOND

               atoms(j)%noOfBonds = atoms(j)%noOfBonds + 1 
               atoms(j)%noOfWeakBonds = atoms(j)%noOfWeakBonds + 1 
               atoms(j)%bondedAtoms(atoms(j)%noOfBonds) = i
               atoms(j)%bondTypes(atoms(j)%noOfBonds)   = WEAK_BOND
            endif
         endif
      enddo
   enddo

!ANUJA   write(u_mainout,*) 'Connectivity :', numberOfAtoms
!ANUJA   do i = 1, numberOfAtoms
!ANUJA      do j = 1, atoms(i)%noOfBonds
!ANUJA         if (i .lt. atoms(i)%bondedAtoms(j)) then
!ANUJA            write(u_mainout,*) i, "(", atoms(i)%symbol, ")", &
!ANUJA            &          atoms(i)%bondedAtoms(j), "(", atoms(atoms(i)%bondedAtoms(j))%symbol, ")",  &
!ANUJA            &          atoms(i)%bondTypes(j)
!ANUJA         endif
!ANUJA      enddo
!ANUJA   enddo
end subroutine buildConnectivity

!! 
!! buildSimpleConnectivity() - build the molecular graph, only directly connected ones
subroutine buildSimpleConnectivity()
use m_input
use m_units
use m_atomInfo
use m_constants
use m_datatypes
use m_utilityFunctions, only : isStronglyBonded

implicit none

integer(kind=SP) :: i, j
real(kind=DP)    :: distance, radiusSum, x, y, z, dblOvr

   ! first identify only strong bonds
   do i = 1, numberOfAtoms  ! loop over atom1
      do j = 1, i           ! loop over atom2
         x = atoms(i)%x - atoms(j)%x
         y = atoms(i)%y - atoms(j)%y
         z = atoms(i)%z - atoms(j)%z

         distance = sqrt(x*x + y*y + z*z)

         ! check for single bond
         radiusSum = covalentRadii(atoms(i)%atomicNumber) &
                     & + covalentRadii(atoms(j)%atomicNumber) 

         if (((radiusSum - COVALENT_BOND_TOLERANCE) < distance) &
        &    .and. (distance < (radiusSum + COVALENT_BOND_TOLERANCE))) then
            ! confirmed single bond
            atoms(i)%noOfBonds = atoms(i)%noOfBonds + 1 
            atoms(i)%bondedAtoms(atoms(i)%noOfBonds) = j
            atoms(i)%bondTypes(atoms(i)%noOfBonds)   = SINGLE_BOND

            atoms(j)%noOfBonds = atoms(j)%noOfBonds + 1 
            atoms(j)%bondedAtoms(atoms(j)%noOfBonds) = i
            atoms(j)%bondTypes(atoms(j)%noOfBonds)   = SINGLE_BOND
         endif
      enddo
   enddo

   write(u_mainout,*) 'Simple Connectivity :', numberOfAtoms
   do i = 1, numberOfAtoms
      do j = 1, atoms(i)%noOfBonds
         if (i .lt. atoms(i)%bondedAtoms(j)) then
            write(u_mainout,*) i, "(", atoms(i)%symbol, ")", &
            &          atoms(i)%bondedAtoms(j), "(", atoms(atoms(i)%bondedAtoms(j))%symbol, ")",  &
            &          atoms(i)%bondTypes(j)
         endif
      enddo
   enddo
end subroutine buildSimpleConnectivity


!!
!! identifyRings() - identify rings in the moelcular graph
subroutine identifyRings()
use m_input
use m_units
use m_options
use m_graph
use m_constants
use m_datatypes

implicit none

integer(kind=SP) :: i

   allocate(color(MAXIMUM_ATOMS_IN_RING))
   allocate(parent(MAXIMUM_ATOMS_IN_RING))

   color  = WHITE
   parent = -1 

   do i = 1, numberOfAtoms
      if (color(i) .eq. WHITE) then
         call traverseAndSaveRings(i)
      endif
   enddo

   ! remove subset rings 
   call removeSubsets()

   write(u_mainout,*) 'Rings :'
   do i = 1, numberOfRings
      ! set the planarity type of this ring
      if (cutRing) then
         rings(i)%isPlanar = .false.
      else
         call setPlanarity(i)
      endif
   enddo

   ! remove subset rings 
   call removeSubsets()

   do i = 1, numberOfRings
      write(u_mainout,'(I4, " : ", L1, " : (", I4, "), ", 50I5)') &
     &  i, rings(i)%isPlanar, rings(i)%noOfRingAtoms, (rings(i)%ringAtoms(1:rings(i)%noOfRingAtoms))
   enddo

   deallocate(color, parent)
end subroutine identifyRings


!!
!! traverseAndSaveRings() - recursively traverse the molecular graph
!! and save the rings
recursive subroutine traverseAndSaveRings(v)
use m_input
use m_graph
use m_constants
use m_datatypes
use m_utilityFunctions, only : isStronglyBonded

implicit none

integer(kind=SP) :: v, i, vertex

   color(v) = GRAY ! begin to process this node

   do i = 1, numberOfAtoms
      if (isStronglyBonded(i, v)) then
         if (color(i) .eq. WHITE) then ! node not yet processed
            parent(i) = v
            call traverseAndSaveRings(i)
         else if (color(i) .eq. BLACK) then ! a cycle is detected!
            ! record it
            numberOfRings = numberOfRings + 1
            rings(numberOfRings)%noOfRingAtoms = rings(numberOfRings)%noOfRingAtoms + 1
            rings(numberOfRings)%ringAtoms(rings(numberOfRings)%noOfRingAtoms) = i

            vertex = i
                        
            ! record the cycle
            do while (.true.)                

               ! delete really large rings
               if (rings(numberOfRings)%noOfRingAtoms .ge. MAXIMUM_ATOMS_IN_RING) then
                  numberOfRings = numberOfRings - 1
                  exit
               endif

               vertex = parent(vertex)               
               
               if (vertex .eq. -1) then
                  exit
               else if (vertex .eq. v) then
                  rings(numberOfRings)%noOfRingAtoms = rings(numberOfRings)%noOfRingAtoms + 1
                  rings(numberOfRings)%ringAtoms(rings(numberOfRings)%noOfRingAtoms) = vertex
                  exit
               else
                  rings(numberOfRings)%noOfRingAtoms = rings(numberOfRings)%noOfRingAtoms + 1
                  rings(numberOfRings)%ringAtoms(rings(numberOfRings)%noOfRingAtoms) = vertex
               endif
            enddo            
         endif
      endif
   enddo

   color(v) = BLACK ! processing over
end subroutine traverseAndSaveRings

!!
!! removeSubsets() - remove subset rings
subroutine removeSubsets()
use m_input
use m_datatypes

implicit none

logical(kind=SP), dimension(MAXIMUM_ATOMS_IN_RING) :: contained
logical(kind=SP) :: subSet
integer(kind=SP) :: i, j, k, actualAtomsCompared

   do i = 1, numberOfRings
      do j = 1, numberOfRings
         ! do not compare the same ring
         if (i .eq. j) then
            cycle
         endif

         ! check which is the smaller ring, and descide accordingly
         contained = .false.         
         if (rings(i)%noOfRingAtoms .gt. rings(j)%noOfRingAtoms) then
            actualAtomsCompared = rings(j)%noOfRingAtoms
            contained(1:actualAtomsCompared) = (rings(i)%ringAtoms(1:actualAtomsCompared) &
           &                                   .eq. rings(j)%ringAtoms(1:actualAtomsCompared))            
         else if (rings(i)%noOfRingAtoms .lt. rings(j)%noOfRingAtoms) then
            actualAtomsCompared = rings(i)%noOfRingAtoms
            contained(1:actualAtomsCompared) = (rings(i)%ringAtoms(1:actualAtomsCompared) &
           &                                   .eq. rings(j)%ringAtoms(1:actualAtomsCompared))
         else
            cycle  ! equal sizes are not compared
         endif

         subSet = contained(1)
         do k = 2, actualAtomsCompared
            subSet = subSet .and. contained(k)
         enddo

         if (subSet) then
            if (rings(i)%noOfRingAtoms .gt. rings(j)%noOfRingAtoms) then
               actualAtomsCompared = rings(i)%noOfRingAtoms - rings(j)%noOfRingAtoms + 2
               do k = 2, actualAtomsCompared
                  rings(i)%ringAtoms(k) = rings(i)%ringAtoms(k + rings(j)%noOfRingAtoms - 2)
               enddo

               if ((rings(i)%noOfRingAtoms - actualAtomsCompared + 1) .le. 2) cycle

               rings(i)%noOfRingAtoms = rings(i)%noOfRingAtoms - actualAtomsCompared + 1
            else if (rings(i)%noOfRingAtoms .lt. rings(j)%noOfRingAtoms) then
               actualAtomsCompared = rings(j)%noOfRingAtoms - rings(i)%noOfRingAtoms + 2
               do k = 2, actualAtomsCompared
                  rings(j)%ringAtoms(k) = rings(j)%ringAtoms(k + rings(i)%noOfRingAtoms - 2)
               enddo

               if ((rings(j)%noOfRingAtoms - actualAtomsCompared + 1) .le. 2) cycle

               rings(j)%noOfRingAtoms = rings(j)%noOfRingAtoms - actualAtomsCompared + 1
            endif
         endif
      enddo
   enddo

end subroutine removeSubsets

!!
!! setPlanarity() - set the planarity of the given ring number
subroutine setPlanarity(ringNumber)
use m_input
use m_constants
use m_datatypes
use m_utilityFunctions, only : cross, angleBetween

implicit none

integer(kind=SP) :: ringNumber, ringLength, i, i2, i3
real(kind=DP)    :: angle
type(t_point3d)  :: a12, a23, a34, n1, n2

   ringLength = rings(ringNumber)%noOfRingAtoms

   do i = 1, ringLength-2
      a12%x = atoms(rings(ringNumber)%ringAtoms(i+1))%x - atoms(rings(ringNumber)%ringAtoms(i))%x
      a12%y = atoms(rings(ringNumber)%ringAtoms(i+1))%y - atoms(rings(ringNumber)%ringAtoms(i))%y
      a12%z = atoms(rings(ringNumber)%ringAtoms(i+1))%z - atoms(rings(ringNumber)%ringAtoms(i))%z

      if (i+2 .gt. rings(ringNumber)%noOfRingAtoms) then
         i2 = 1        
      else
         i2 = i + 2
      endif

      a23%x = atoms(rings(ringNumber)%ringAtoms(i2))%x - atoms(rings(ringNumber)%ringAtoms(i+1))%x
      a23%y = atoms(rings(ringNumber)%ringAtoms(i2))%y - atoms(rings(ringNumber)%ringAtoms(i+1))%y
      a23%z = atoms(rings(ringNumber)%ringAtoms(i2))%z - atoms(rings(ringNumber)%ringAtoms(i+1))%z

      if (i+3 .gt. rings(ringNumber)%noOfRingAtoms) then
         i3 = 2        
      else
         i3 = i + 3
      endif

      a34%x = atoms(rings(ringNumber)%ringAtoms(i3))%x - atoms(rings(ringNumber)%ringAtoms(i2))%x
      a34%y = atoms(rings(ringNumber)%ringAtoms(i3))%y - atoms(rings(ringNumber)%ringAtoms(i2))%y
      a34%z = atoms(rings(ringNumber)%ringAtoms(i3))%z - atoms(rings(ringNumber)%ringAtoms(i2))%z

      ! compute the normals to the plane
      n1 = cross(a12, a23)
      n2 = cross(a23, a34)

      ! then find the angle between the two normals - the angle between the planes
      angle = angleBetween(n1, n2)

      if (angle .gt. TORSSIAN_ANGLE_TOLERANCE) then
         ! we have a non planar ring
         rings(ringNumber)%isPlanar = .false.
         return
      endif
   enddo

   ! if control reaches here, its a planar ring
   rings(ringNumber)%isPlanar = .true.
   return
end subroutine setPlanarity
