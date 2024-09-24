!!
!! collate.f90
!!
!! The fragmentation algorithm.
!!
!! @author V.Ganesh
!! @version 2.0
!!
module m_collate
use m_datatypes

implicit none

contains

!!
!! collate() - the fragmentation algorithm:
!! a) Make 'n' separate fragments by centering a non-pendant
!!    atom with a sphere of radius 'r'.
!! b) Do a depth or a bredth wise traversal of molecular grap
!!      For each atom on the path
!!          - check the next connected atom and merge the 
!!            fragments as one 
!!          - check all the atoms that can be picked from this
!!            fragment with 'r' criterion
!!          - update fragment map table
!! c) Make final stage merging, update FMT
subroutine collate()
use m_input
use m_units
use m_options
use m_fragment
use m_datatypes
use m_utilityFunctions, only : isPendantAtom, isRingAtom
use m_utilityFunctions, only : writeFragments, writeFragmentsAsXYZ, writePickInfo

implicit none

integer(kind=SP) :: i, idx
logical(kind=SP) :: didAddAtoms

   ! stage 1, iterate through each nonpendent atom and then 
   ! generate a fragment centered on it.
   do i = 1, numberOfAtoms
      idx = sortedAtomIndices(i)
      !if (.not. isPendantAtom(i)) then
         ! if not a pendent atom
!         print*, "SSK sortedAtomIndices => ", idx, i
         call makeAtomCenteredFragment(idx, radiusCutoff)
         ! if part of ring ... TODO ... not implemented ??
      !endif
   enddo   

!   write(u_mainout,*) 'Number of fragments generated at stage1: ', numberOfFragments
!   write(6,*) 'Number of fragments generated at stage1: ', numberOfFragments
   
   call writeFragments("stage1.list")
   call writeFragmentsAsXYZ("stage1.xyz")
   call writePickInfo() 

   !...............
   ! stage2, do a bredth/ depth first traversal of the molecular graph
   call mergeConnectedFragments()

!   write(u_mainout,*) 'Number of fragments generated at stage2: ', numberOfFragments

   call writeFragments("stage2.list")
   call writeFragmentsAsXYZ("stage2.xyz")
   call writePickInfo() 

   ! stage3, update the FMT and remove unnecessary fragments
   call updateFMTAndRemoveFragments()

!   write(u_mainout,*) 'Number of fragments generated at stage3: ', numberOfFragments

   call writeFragments("stage3.list")
   call writeFragmentsAsXYZ("stage3.xyz")
   call writePickInfo() 
   !...............

   ! stage4, merge common fragments
   call mergeCommonFragments()

!   write(u_mainout,*) 'Number of fragments generated at stage4: ', numberOfFragments

   call writeFragments("stage4.list")
   call writeFragmentsAsXYZ("stage4.xyz")
   call writePickInfo() 

   ! stage5, update the FMT and remove unnecessary fragments
   call updateFMTAndRemoveFragments()

!   write(u_mainout,*) 'Number of fragments generated at stage5: ', numberOfFragments

   call writeFragments("stage5.list")
   call writeFragmentsAsXYZ("stage5.xyz")
   call writePickInfo() 

   ! stage6, included the missed atoms
   do i = 1, numberOfFragments
      do while(.true.) 
         didAddAtoms = .false.
         call includeMissedAtoms(i, didAddAtoms)
         if (.not. didAddAtoms) exit
      enddo
   enddo

   call updateFMTAndRemoveFragments()

!   write(u_mainout,*) 'Number of fragments generated at stage6: ', numberOfFragments
   call writeFragments("stage6.list")
   call writeFragmentsAsXYZ("stage6.xyz")     
   call writePickInfo() 

   ! stage7, remove dangling atoms dummy atoms
   do i = 1, numberOfFragments
      call removeDanglingAtoms(i)
   enddo

   ! stage7a, merge common fragments
   call mergeCommonFragments()

   ! stage 7b, add dummy atoms
   if (doAddDummyAtoms) then
     do i = 1, numberOfFragments
        call addDummyAtoms(i)
     enddo
   endif

   call updateFMTAndRemoveFragments()

   call writeFragments("stage7.list")
   call writeFragmentsAsXYZ("stage7.xyz")     
! isFinalFrags is true for writing the atom goodness index 
   isFinalFrags = .true.

   call writePickInfo() 

   ! stage7c, evaluate the fragment centers and the bounding region
   call computeCenterAndBounds()

   ! stage8, before comming out, do atom pair selections
   call setupAtomPairMap()   
end subroutine collate

!!
!! computeCenterAndBounds() -
!! compute the center and bounds of each fragment
subroutine computeCenterAndBounds()
use m_options
use m_input
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: i, j
real(kind=DP) :: x, y, z, r1, r2

  do i=1, numberOfFragments
     ! assume that there is atleast one atom??
     if (fragments(i)%numberOfTotalAtoms .eq. 0) cycle

     fragments(i)%center%x = 0.0
     fragments(i)%center%y = 0.0
     fragments(i)%center%z = 0.0
     
     fragments(i)%maxBound%x = atoms(fragments(i)%atomIndices(1))%x
     fragments(i)%maxBound%y = atoms(fragments(i)%atomIndices(1))%y
     fragments(i)%maxBound%z = atoms(fragments(i)%atomIndices(1))%z

     fragments(i)%minBound%x = atoms(fragments(i)%atomIndices(1))%x
     fragments(i)%minBound%y = atoms(fragments(i)%atomIndices(1))%y
     fragments(i)%minBound%z = atoms(fragments(i)%atomIndices(1))%z

     do j=2, fragments(i)%numberOfTotalAtoms
        x = atoms(fragments(i)%atomIndices(j))%x
        y = atoms(fragments(i)%atomIndices(j))%y
        z = atoms(fragments(i)%atomIndices(j))%z

        ! center
        fragments(i)%center%x = fragments(i)%center%x + x
        fragments(i)%center%y = fragments(i)%center%y + y
        fragments(i)%center%z = fragments(i)%center%z + z

        ! bounds
        if (x > fragments(i)%maxBound%x) fragments(i)%maxBound%x = x
        if (y > fragments(i)%maxBound%y) fragments(i)%maxBound%y = y
        if (z > fragments(i)%maxBound%z) fragments(i)%maxBound%z = z

        if (x < fragments(i)%minBound%x) fragments(i)%minBound%x = x
        if (y < fragments(i)%minBound%y) fragments(i)%minBound%y = y
        if (z < fragments(i)%minBound%z) fragments(i)%minBound%z = z
     enddo

     ! center
     fragments(i)%center%x = fragments(i)%center%x / fragments(i)%numberOfTotalAtoms
     fragments(i)%center%y = fragments(i)%center%y / fragments(i)%numberOfTotalAtoms
     fragments(i)%center%z = fragments(i)%center%z / fragments(i)%numberOfTotalAtoms 

     ! compute the radius
     x = fragments(i)%maxBound%x - fragments(i)%center%x
     y = fragments(i)%maxBound%y - fragments(i)%center%y
     z = fragments(i)%maxBound%z - fragments(i)%center%z

     r1 = sqrt(x*x + y*y + z*z)

     x = fragments(i)%minBound%x - fragments(i)%center%x
     y = fragments(i)%minBound%y - fragments(i)%center%y
     z = fragments(i)%minBound%z - fragments(i)%center%z

     r2 = sqrt(x*x + y*y + z*z)

     fragments(i)%radius = max(r1, r2)

     if (verbose) then
        write(*,'("Frag:", i4, " c:", 3F8.3, " mx:", 3F8.3, " mn:", 3F8.3, " r:", F8.3)') &
        &  i , fragments(i)%center, fragments(i)%maxBound,  &
        &      fragments(i)%minBound, fragments(i)%radius
     endif
  enddo

end subroutine computeCenterAndBounds

!!
!! mergeConnectedFragments() -
!! stage2 - of collate process, merge the fragments, by doing
!! a bredth/ depth first traversal of the molecular graph
subroutine mergeConnectedFragments()
use m_input
use m_graph
use m_datatypes
use m_utilityFunctions, only : isPendantAtom

implicit none

integer(kind=SP) :: i, idx

   allocate(visited(numberOfAtoms))
   visited = .false.

   do i = 1, numberOfAtoms
      idx = sortedAtomIndices(i)
      if (.not. visited(idx)) then         
          visited(idx) = .true.   
          call traverseAndMergeFragment(idx)
      endif
   enddo
   
   deallocate(visited)
end subroutine mergeConnectedFragments

!!
!! updateFMTAndRemoveFragments() - update the fragment atom map table
!! and remove the fragments from which no atoms are picked.
subroutine updateFMTAndRemoveFragments() 
use m_input
use m_units
use m_options
use m_fragment
use m_datatypes

use m_utilityFunctions, only : findDistanceGoodness

implicit none

integer(kind=SP) :: i, j, nNewFragments
real(kind=DP)    :: distance
integer(kind=SP), allocatable, dimension(:)  :: mapFragIndices
type(t_fragment), allocatable, dimension(:) :: newFragments


    !! for avoiding mixing up of goodness values from previous stages
    atomGoodnessMap = -1.0

    ! first update the FMT
    do i = 1, numberOfFragments
       do j = 1, fragments(i)%numberOfTotalAtoms
          ! find the goodness values
          distance = findDistanceGoodness(fragments(i)%atomIndices(j), i)

          ! update the tables
          if (distance .gt. atomGoodnessMap(fragments(i)%atomIndices(j))) then
             atomGoodnessMap(fragments(i)%atomIndices(j)) = distance
             fragmentAtomMap(fragments(i)%atomIndices(j)) = i
          endif
       enddo
    enddo

    ! then remove the fragments that are useless, that is no atoms are picked 
    ! from the given fragment
    allocate(newFragments(numberOfFragments) , mapFragIndices(numberOfFragments))
    nNewFragments  = 0
    mapFragIndices = 0
    do i = 1, numberOfFragments
       if (count(mask=fragmentAtomMap .eq. i, dim=1) .gt. 0) then
          ! if some atom at all is selected, we copy it
          nNewFragments = nNewFragments + 1
          newFragments(nNewFragments) = fragments(i)
          mapFragIndices(i)           = nNewFragments
          ! write(*,*) 'Fragment ', i, ' is now ', nNewFragments
!ANUJA       else
!ANUJA          write(u_mainout,*) 'Removing fragment : ', i
       endif 
    enddo

    ! map new indices
    do i = 1, numberOfAtoms
       fragmentAtomMap(i) = mapFragIndices(fragmentAtomMap(i))
    enddo

    ! remove the useless stuff
    do i = 1, nNewFragments
       fragments(i) = newFragments(i)
    enddo
    numberOfFragments = nNewFragments    

    deallocate(newFragments, mapFragIndices)
end subroutine updateFMTAndRemoveFragments

!! 
!! mergeCommonFragments() -
!! stage3 - merge common fragments based on the number of atoms common between
!! them, with the condition that the maximal number of atoms in a fragment
!! constraint is obeyed
subroutine mergeCommonFragments() 
use m_input
use m_units
use m_options
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: i, j, k, mergeTarget, mergeWith
integer(kind=SP) :: noOfCommonAtoms, expectedMergeSize
logical(kind=SP) :: doneMerging
logical(kind=SP) :: allAtomsPresent

    doneMerging = .false.
    do while (.not. doneMerging) 
!ANUJA       write(u_mainout,*) 'Number of fragments to merge: ', numberOfFragments
       out_loop: do i = 1, numberOfFragments
          do j = i, numberOfFragments
             doneMerging = (i .eq. numberOfFragments) .and. (j .eq. numberOfFragments)
 
             if (i .eq. j) cycle
             
             ! write(*,*) 'Checking if ', i, ' can be merged with ', j

             expectedMergeSize = fragments(i)%numberOfTotalAtoms
             allAtomsPresent = .true.

             ! do a dummy merge 'i' into 'j', to check for size constraints
             noOfCommonAtoms = 0
             do k = 1, fragments(j)%numberOfTotalAtoms
                if (count(mask=fragments(i)%atomIndices .eq. fragments(j)%atomIndices(k), dim=1) .le. 0) then
                   expectedMergeSize = expectedMergeSize + 1
                else
                   noOfCommonAtoms = noOfCommonAtoms + 1
                   allAtomsPresent = .false.
                endif
             enddo

             ! if we exeed expected size, this merging is not possible, so return
             if (noOfCommonAtoms.ne.fragments(j)%numberOfTotalAtoms .and. &
             &   noOfCommonAtoms.ne.fragments(i)%numberOfTotalAtoms) then
                 if (.not.allAtomsPresent) then
                    if (expectedMergeSize .gt. maxFragmentSize) then
                       cycle
                    endif
                 endif
             endif

             ! and check for similarity
             ! TODO: change the percentage similarity
             ! if (noOfCommonAtoms .ge. fragments(i)%numberOfTotalAtoms*0.5) then
             if (noOfCommonAtoms .gt. 0) then
                mergeTarget = i
                mergeWith   = j

                !doneMerging = (i .eq. numberOfFragments)

                ! write(*,*) 'Trying to merge', mergeTarget, mergeWith, i
                call mergeFragments(mergeTarget, mergeWith)
                ! write(*,*) '... done!'
                exit out_loop
             endif                        
          enddo
       enddo out_loop
    enddo
    
end subroutine mergeCommonFragments

!!
!! traverseAndMergeFragment() - start bredth/ depth first traversal 
!! starting from vertex v and merge the fragments with the 
!! connected ones
recursive subroutine traverseAndMergeFragment(v) 
use m_input
use m_graph
use m_datatypes
use m_utilityFunctions, only : isPendantAtom2

implicit none

integer(kind=SP) :: i, bondedAtom, v

   do i = 1, atoms(v)%noOfBonds
      bondedAtom = atoms(v)%bondedAtoms(i)
      if (.not. visited(bondedAtom)) then
         ! merge ...
         call mergeFragmentsCenteredOn(v, bondedAtom)

         visited(bondedAtom) = .true. 

         ! and then traverse it, along the dept if needed
         ! call traverseAndMergeFragment(bondedAtom)
      endif
   enddo

end subroutine traverseAndMergeFragment

!!
!! mergeFragments() - merge the fragments numbered indexOfA, indexOfB
!! where index indexOfA is always less than indexOfB
subroutine mergeFragments(indexOfA, indexOfB)
use m_input
use m_options
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: indexOfA, indexOfB
integer(kind=SP) :: i, expectedMergeSize, noOfCommonAtoms
logical(kind=SP) :: allAtomsPresent

   ! check to ensure
   if (indexOfA .ge. indexOfB) return

   expectedMergeSize = fragments(indexOfA)%numberOfTotalAtoms

   noOfCommonAtoms = 0
   allAtomsPresent = .true.

   ! do a dummy merge 'b' into 'a', to check for size constraints
   do i = 1, fragments(indexOfB)%numberOfTotalAtoms
      if (count(mask=fragments(indexOfA)%atomIndices .eq. fragments(indexOfB)%atomIndices(i), dim=1) .le. 0) then
         expectedMergeSize = expectedMergeSize + 1
      else
         noOfCommonAtoms = noOfCommonAtoms + 1
         allAtomsPresent = .false.
      endif
   enddo

   ! if we exeed expected size, this merging is not possible, so return
   ! but we are ofcourse lenient if they are subsets
   if (noOfCommonAtoms.ne.fragments(indexOfA)%numberOfTotalAtoms .and. &
  &    noOfCommonAtoms.ne.fragments(indexOfB)%numberOfTotalAtoms) then
       if (.not.allAtomsPresent) then
          if (expectedMergeSize .gt. maxFragmentSize) then
             return
          endif
       endif
   endif

   ! write(*,*) 'Merging ', indexOfA, ' with ', indexOfB

   ! else do the actual merge 'b' into 'a'
   do i = 1, fragments(indexOfB)%numberOfTotalAtoms
      if (count(mask=fragments(indexOfA)%atomIndices .eq. fragments(indexOfB)%atomIndices(i), dim=1) .le. 0) then
         fragments(indexOfA)%numberOfTotalAtoms = & 
         &        fragments(indexOfA)%numberOfTotalAtoms + 1
         fragments(indexOfA)%atomIndices(fragments(indexOfA)%numberOfTotalAtoms) = &
         &        fragments(indexOfB)%atomIndices(i)
      endif
   enddo

   ! invalidate theirs origins!
   fragments(indexOfA)%centeredOn = -1

   ! push all the fragments after 'b' by one
   do i = indexOfB, numberOfFragments-1
      fragments(i) = fragments(i+1)
   enddo

   ! update the FMT
   do i = 1, numberOfAtoms
      if (fragmentAtomMap(i) .ge. indexOfB) then
         fragmentAtomMap(i) = fragmentAtomMap(i) - 1
      endif
   enddo

   ! decrement fragment count
   numberOfFragments = numberOfFragments - 1
end subroutine mergeFragments

!!
!! mergeFragmentsCenteredOn() - merge the fragments centered on 
!! atom centers a and b, where index a is always less than b
subroutine mergeFragmentsCenteredOn(a, b)
use m_input
use m_options
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: indexOfA, indexOfB, a, b

   indexOfA = findFragmentCenteredOn(a)
   if (indexOfA .le. 0) return

   indexOfB = findFragmentCenteredOn(b)
   if (indexOfB .le. 0) return
   
   call mergeFragments(indexOfA, indexOfB)
   
end subroutine mergeFragmentsCenteredOn

!!
!! findFragmentCenteredOn() - finds the fragment index centered on
!! a particular atom index
integer(kind=SP) function findFragmentCenteredOn(a)
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: i, a

   findFragmentCenteredOn = -1

   do i = 1, numberOfFragments
      if (fragments(i)%centeredOn .eq. a) then
         findFragmentCenteredOn = i
         return
      endif
   enddo

end function findFragmentCenteredOn

!!
!! makeAtomCenteredFragment() - make atom centered fragment
!! atomIndex : the atom index around which the fragment is 
!!             to be set up
!! radiusCutoff : radius cutoff 
subroutine makeAtomCenteredFragment(atomIndex, radiusCutoff)
use m_input
use m_datatypes

implicit none

integer(kind=SP) :: atomIndex
real(kind=DP)    :: radiusCutoff

    call makeACenteredFragment(atoms(atomIndex)%x, &
                             & atoms(atomIndex)%y, &
                             & atoms(atomIndex)%z, &
                             & radiusCutoff, &
                             & atomIndex)

end subroutine makeAtomCenteredFragment

!!
!! makeACenteredFragment() - make a fragment centered around a point
!! x, y, z : describe the point 
!! radiusCutoff : the radius cutoff from that point
subroutine makeACenteredFragment(x, y, z, radiusCutoff, atomIndex)
use m_input
use m_fragment
use m_datatypes
use m_utilityFunctions, only : findDistanceGoodness
use m_utilityFunctions, only : isPendantAtom2, isPlanarRingAtom

implicit none

real(kind=DP)    :: x, y, z, xD, yD, zD, distSquared, radiusCutoff
real(kind=DP)    :: radiusSquared, distance
integer(kind=SP) :: atomIndex, i
logical(kind=SP) :: didAddAtoms
    
    !...............
    ! if this atom is already taken care of, 
    ! we quitely come out
    ! if (fragmentAtomMap(atomIndex) .gt. 0) then
    !   return
    ! endif
    !...............
 
    radiusSquared = radiusCutoff * radiusCutoff        

    numberOfFragments = numberOfFragments + 1
    !write(*,'(i5,3f15.6)')numberOfFragments,x,y,z !by ssk    

    ! make the fragment
    do i = 1, numberOfAtoms
       xD = x - atoms(i)%x
       yD = y - atoms(i)%y
       zD = z - atoms(i)%z

       distSquared = xD*xD + yD*yD + zD*zD

       ! the distance check
       if (distSquared.le.radiusSquared) then
          fragments(numberOfFragments)%numberOfTotalAtoms = & 
         &    fragments(numberOfFragments)%numberOfTotalAtoms + 1

          fragments(numberOfFragments)%atomIndices( &
         &   fragments(numberOfFragments)%numberOfTotalAtoms) = i
       endif
    enddo    

    ! ensure that all the connected pendant atoms 
    ! are also included in the currently made fragment
    do while(.true.) 
       didAddAtoms = .false.
       call includeMissedAtoms(numberOfFragments, didAddAtoms)           
       if (.not. didAddAtoms) exit
    enddo

    ! set up the the atomIndex
    fragments(numberOfFragments)%centeredOn = atomIndex

    ! update the fragment map table
    distance = findDistanceGoodness(atomIndex, numberOfFragments)
    fragmentAtomMap(atomIndex) = numberOfFragments
    atomGoodnessMap(atomIndex) = distance

    !...............
    ! update the FMT, so that all the atoms connected to this
    ! are picked from the same fragment
    do i = 1, atoms(atomIndex)%noOfBonds
       distance = findDistanceGoodness(atoms(atomIndex)%bondedAtoms(i), &
       &                               numberOfFragments)
       fragmentAtomMap(atoms(atomIndex)%bondedAtoms(i)) = numberOfFragments
       atomGoodnessMap(atoms(atomIndex)%bondedAtoms(i)) = distance
    enddo
    !...............

    ! ensure removal of disconnected atoms
    !! call removeDanglingAtoms(numberOfFragments)

    ! all the pendant atoms that satisfy the radius 
    ! cutoff criterion, should be included in this fragment
    do i = 1, fragments(numberOfFragments)%numberOfTotalAtoms
       if (isPendantAtom2(fragments(numberOfFragments)%atomIndices(i))) then
          distance = findDistanceGoodness(fragments(numberOfFragments)%atomIndices(i), &
          &                                numberOfFragments)
          if ((distance.ge.radiusCutoff .and. &
          &   distance.ge.atomGoodnessMap(fragments(numberOfFragments)%atomIndices(i))) .or. &
          &   (atomGoodnessMap(fragments(numberOfFragments)%atomIndices(i)).eq.0.0)) then
             ! write(*,*) 'Picking pendant ', fragments(numberOfFragments)%atomIndices(i), &
             ! &          ' from ', numberOfFragments
             fragmentAtomMap(fragments(numberOfFragments)%atomIndices(i)) = numberOfFragments
             atomGoodnessMap(fragments(numberOfFragments)%atomIndices(i)) = distance
          endif
       endif
    enddo
end subroutine makeACenteredFragment

!!
!! includeMissedAtoms() - include the missed atoms
subroutine includeMissedAtoms(fragIndex, didAddAtoms)
use m_input
use m_fragment
use m_datatypes
use m_utilityFunctions, only : isPendantAtom2, isPlanarRingAtom, isSuspendedAtom

implicit none

integer(kind=SP) :: i, j, connectedIndex, nMissedAtoms, fragIndex, bondedAtom
integer(kind=SP), allocatable, dimension(:) :: missedAtoms
integer(kind=SP), dimension(MAX_ATOMS_IN_FRAGMENT) :: atomIndices
logical(kind=SP) :: didAddAtoms

    didAddAtoms = .false.

    atomIndices = 0
    atomIndices(1:fragments(fragIndex)%numberOfTotalAtoms) = &
    &  fragments(fragIndex)%atomIndices(1:fragments(fragIndex)%numberOfTotalAtoms)

    if (fragments(fragIndex)%numberOfTotalAtoms .le. 0) return

    nMissedAtoms = 0
!    allocate(missedAtoms(fragments(fragIndex)%numberOfTotalAtoms))
    allocate(missedAtoms(MAX_ATOMS_IN_FRAGMENT))
    missedAtoms = -1

    ! ensure that all the connected pendant atoms 
    ! are also included in the currently made fragment
    do i = 1, fragments(fragIndex)%numberOfTotalAtoms
       connectedIndex = fragments(fragIndex)%atomIndices(i) 
       do j = 1, atoms(connectedIndex)%noOfBonds
          bondedAtom = atoms(connectedIndex)%bondedAtoms(j)
          ! write(*,*) connectedIndex, 'missing?', bondedAtom  
          if (isPendantAtom2(bondedAtom) .or. &
          &   isPlanarRingAtom(bondedAtom) .or. &
          &   isSuspendedAtom(fragIndex, bondedAtom) .or. &
          &   (atoms(connectedIndex)%bondTypes(j) .gt. SINGLE_BOND)) then
             if (count(mask=atomIndices .eq. bondedAtom, dim=1) .le. 0) then
                if (count(mask=missedAtoms .eq. bondedAtom, dim=1) .le. 0) then
                   nMissedAtoms = nMissedAtoms + 1
                   missedAtoms(nMissedAtoms) = bondedAtom
                   didAddAtoms = .true.
                   ! write(*,*) 'Missing atom ', bondedAtom, & 
                   ! &          ' will be included in ', fragIndex
                endif
             endif
          endif
       enddo
    enddo

    ! write(*,*) fragIndex, missedAtoms

    ! include the missed atoms
    do i = 1, nMissedAtoms
       fragments(fragIndex)%numberOfTotalAtoms = &
      &         fragments(fragIndex)%numberOfTotalAtoms + 1
       fragments(fragIndex)%atomIndices( &
      &         fragments(fragIndex)%numberOfTotalAtoms) = &
      &                                         missedAtoms(i)
    enddo

    deallocate(missedAtoms)
end subroutine includeMissedAtoms

!!
!! removeDanglingAtoms() - remove dangling atoms from a 
!! fragment
subroutine removeDanglingAtoms(fragIndex)
use m_input
use m_units
use m_atomInfo
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: atomIndex, i, j, fragIndex, nValidIndices
integer(kind=SP) :: connectedIndex
logical(kind=SP) :: connected
integer(kind=SP), allocatable, dimension(:) :: validIndices

    allocate(validIndices(MAX_ATOMS_IN_FRAGMENT))

    nValidIndices = 0
    validIndices  = 0

    do i = 1, fragments(fragIndex)%numberOfTotalAtoms
       atomIndex = fragments(fragIndex)%atomIndices(i)

       ! skip non-hydrogen / atoms
       if (defaultValencies(atoms(atomIndex)%atomicNumber) .gt. 2) then 
          nValidIndices = nValidIndices + 1
          validIndices(nValidIndices) = atomIndex
          cycle
       endif

       connected = .false.

       do j = 1, atoms(atomIndex)%noOfBonds
          connectedIndex = count(mask=fragments(fragIndex)%atomIndices .eq. atoms(atomIndex)%bondedAtoms(j))

          if (connectedIndex .gt. 0) then
             if (atoms(atomIndex)%bondTypes(j) .ne. WEAK_BOND) then
                connected = .true.
                exit 
             endif
          endif
       enddo

       ! remove the disconnected atom ... keep track of the conneted ones
       if (connected) then          
          nValidIndices = nValidIndices + 1
          validIndices(nValidIndices) = atomIndex
       else
          cycle 
          !write(u_mainout,*) 'Dangling atom ', i , ' will be removed from', numberOfFragments
       endif
    enddo

    ! update the stuff
    fragments(fragIndex)%numberOfTotalAtoms = nValidIndices
    fragments(fragIndex)%atomIndices        = validIndices

    if (nValidIndices .eq. 0) then
       write(u_mainout,*) 'Removing fragment ', numberOfFragments , ' altogether ..'
       numberOfFragments = numberOfFragments - 1
    endif

    deallocate(validIndices)

end subroutine removeDanglingAtoms

!! 
!! countBoundaryAtoms() the number of boundary atoms in a fragment
subroutine countBoundaryAtoms(fragIndex)
use m_fragment
use m_utilityFunctions, only : getBondOrderInFragment, getBondOrder
use m_utilityFunctions, only : getStrongBondOrderInFragment, getStrongBondOrder, normalize

implicit none

integer(kind=SP) :: fragIndex, i, bondOrderInFragment, bondOrder

    fragments(fragIndex)%numberOfBoundaryAtoms = 0

    ! first mark the boundary atoms
    do i = 1, fragments(fragIndex)%numberOfTotalAtoms
       ! check the degree in fragment and the degree in the actual 
       ! molecule to identify the boundary atoms
       bondOrderInFragment = getStrongBondOrderInFragment(fragIndex, &
      &                         fragments(fragIndex)%atomIndices(i))
       bondOrder = getStrongBondOrder(fragments(fragIndex)%atomIndices(i))

       if (bondOrderInFragment .ne. bondOrder) then
          fragments(fragIndex)%numberOfBoundaryAtoms = & 
          &        fragments(fragIndex)%numberOfBoundaryAtoms + 1
       endif
    enddo

end subroutine countBoundaryAtoms

!!
!! addDummyAtoms() - add dummy atoms to the given fragment
!! index - fragIndex
subroutine addDummyAtoms(fragIndex)
use m_input
use m_options
use m_fragment
use m_atomInfo
use m_datatypes
use m_atomInfo, only : getAtomicNumber
use m_utilityFunctions, only : getBondOrderInFragment, getBondOrder
use m_utilityFunctions, only : getStrongBondOrderInFragment, getStrongBondOrder, normalize

implicit none

real(kind=DP)    :: bondDistance
integer(kind=SP) :: fragIndex, connIndex, atomicNumber
integer(kind=SP) :: i, j, bondOrderInFragment, bondOrder
logical(kind=SP), allocatable, dimension(:) :: isBoundaryAtom
type(t_point3d)  :: vec
type(t_atom)     :: dummy

    ! write(*,*) 'Adding dummy atoms for fragment :', fragIndex 

    allocate(isBoundaryAtom(fragments(fragIndex)%numberOfTotalAtoms))

    isBoundaryAtom = .false.

    ! add appropriate dummy atoms for this fragment

    ! first mark the boundary atoms
    do i = 1, fragments(fragIndex)%numberOfTotalAtoms
       ! check the degree in fragment and the degree in the actual 
       ! molecule to identify the boundary atoms
       bondOrderInFragment = getStrongBondOrderInFragment(fragIndex, &
      &                         fragments(fragIndex)%atomIndices(i))
       bondOrder = getStrongBondOrder(fragments(fragIndex)%atomIndices(i))

       ! write(*,*) 'Bond order of ', i, bondOrderInFragment, bondOrder
       if (bondOrderInFragment .ne. bondOrder) then
          ! mark this as a boundary atom
          isBoundaryAtom(i) = .true. 
          ! write(*,*) 'Atom ', fragments(fragIndex)%atomIndices(i), ' is on boundary'
       endif
    enddo

    ! then for each marked boundary atom, add appropriate dummy atom(s)
    do i = 1, fragments(fragIndex)%numberOfTotalAtoms
       if (isBoundaryAtom(i)) then
          ! check each bonded atom for the missing one
          do j = 1, atoms(fragments(fragIndex)%atomIndices(i))%noOfBonds
             connIndex = atoms(fragments(fragIndex)%atomIndices(i))%bondedAtoms(j)

             if (atoms(fragments(fragIndex)%atomIndices(i))%bondTypes(j) .eq. WEAK_BOND) then
                cycle
             endif

             if (count(mask=fragments(fragIndex)%atomIndices.eq.connIndex, dim=1) .le. 0) then

                ! add the dummy atom
                vec%x = atoms(connIndex)%x - atoms(fragments(fragIndex)%atomIndices(i))%x
                vec%y = atoms(connIndex)%y - atoms(fragments(fragIndex)%atomIndices(i))%y
                vec%z = atoms(connIndex)%z - atoms(fragments(fragIndex)%atomIndices(i))%z
                
                ! normalize vector
                call normalize(vec)

                ! and then try to add the stuff                
                dummy%symbol = "H"
                call getAtomicNumber(dummy%symbol, atomicNumber) 
                dummy%atomicNumber = atomicNumber
                bondDistance = covalentRadii(atomicNumber) 
                atomicNumber = atoms(fragments(fragIndex)%atomIndices(i))%atomicNumber
                bondDistance = bondDistance + covalentRadii(atomicNumber)

                ! form the dummy atom
                if (hydrogenAtOrigin) then
                   ! place at the 'cut' possition
                   dummy%x = atoms(connIndex)%x
                   dummy%y = atoms(connIndex)%y 
                   dummy%z = atoms(connIndex)%z
                else
                   ! place at the position of the covalent bond with 'H'
                   dummy%x = (vec%x * bondDistance) + atoms(fragments(fragIndex)%atomIndices(i))%x
                   dummy%y = (vec%y * bondDistance) + atoms(fragments(fragIndex)%atomIndices(i))%y
                   dummy%z = (vec%z * bondDistance) + atoms(fragments(fragIndex)%atomIndices(i))%z
                endif

                dummy%noOfBonds    = 1
                dummy%bondedAtoms(1) = fragments(fragIndex)%atomIndices(i)
                dummy%bondTypes(1)   = SINGLE_BOND
                dummy%isDummy = .true.

                ! add it to the list
                fragments(fragIndex)%numberOfDummyAtoms = fragments(fragIndex)%numberOfDummyAtoms + 1
                fragments(fragIndex)%dummyAtoms(fragments(fragIndex)%numberOfDummyAtoms) = dummy
             endif
          enddo
       endif
    enddo

    deallocate(isBoundaryAtom)

    ! write(*,*) 'Done adding dummy atoms for fragment :', fragIndex 
end subroutine addDummyAtoms

!!
!! setupAtomPairMap() - set up atom pair map
subroutine setupAtomPairMap()
use m_input
use m_units
use m_fragment
use m_datatypes

use m_utilityFunctions, only : findDistanceGoodness

implicit none

integer(kind=SP) :: i, j, k, maxGoodnessIndex
real(kind=DP)    :: theGoodness, maxGoodness

real(kind=DP), dimension(:,:), allocatable :: pairGoodness
    
    allocate(pairGoodness(numberOfFragments, numberOfAtoms))

    pairGoodness = -1

    do k = 1, numberOfFragments
       do i = 1, numberOfAtoms
          pairGoodness(k, i) = findDistanceGoodness(i, k)
       enddo
    enddo

    open (100, file="PickInfo")
    write(u_mainout,*)"Atom pick index as  ATOM :: FRAGMENT"
    do i = 1, numberOfAtoms
       do j = 1, i
          maxGoodnessIndex = -1
          maxGoodness      = -1.0
          do k = 1, numberOfFragments
             theGoodness = pairGoodness(k, i)
             theGoodness = min(theGoodness, pairGoodness(k, j))

             if (maxGoodness .eq. -1.0) then 
                maxGoodness      = theGoodness
                maxGoodnessIndex = k  
             else
                if (maxGoodness .lt. theGoodness) then
                   maxGoodness      = theGoodness
                   maxGoodnessIndex = k
                endif
             endif
          enddo

          ! check if a valid stuff
          if (maxGoodness .ne. -1.0) then
             fragmentAtomPairMap(i, j) = maxGoodnessIndex
             fragmentAtomPairMap(j, i) = maxGoodnessIndex
!             write(u_mainout,*) i, ', ', j, ' will be picked from ', fragmentAtomPairMap(i, j)
             write(100,*) i, '  ', j, '  ', fragmentAtomPairMap(i, j)
             
             if (i .eq. j) then 
                fragmentAtomMap(i) = maxGoodnessIndex
                atomGoodnessMap(i) = maxGoodness
                write(u_mainout,*) i, ' :: ', fragmentAtomPairMap(i, j)
                ! write(*,*) i, ', ', j, ' will be picked from ', fragmentAtomPairMap(i, j)
             endif
          endif
       enddo
    enddo
    close(100)

    deallocate(pairGoodness)
    
end subroutine setupAtomPairMap

end module m_collate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
