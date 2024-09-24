!!
!! misc.f90
!!
!! All the utility functions are here.
!!
!! @author V.Ganesh
!! @version 2.0
!!
module m_utilityFunctions
use m_datatypes
use m_input

implicit none

contains

!!
!! function to return a free file unit number 
!!
!! reset: true - reset the unit count to the starting value
!!        false - (default) get the next unit number 
integer(kind=SP) function getUnit(reset)
use m_datatypes
implicit none

integer(kind=SP) :: unitNumber=90
logical(kind=SP), optional :: reset

save unitNumber

   if (present(reset)) then
      if (reset) then
         unitNumber = 90
      endif
   endif

   unitNumber = unitNumber + 1

   getUnit = unitNumber

end function getUnit

!!
!! toUpperCase() - method to change case of the string to 
!! all upper case.
!!
!! stringToBeChanged: intent [inout] the character 
!!        array representing the string
subroutine toUpperCase(stringToBeChanged)
use m_datatypes
implicit none

character(*) stringToBeChanged
integer(kind=SP) stringLength, index

   stringLength = len(stringToBeChanged)

   do index = 1, stringLength
      if ((stringToBeChanged(index:index) .gt. char(96)) .and. &
     &    (stringToBeChanged(index:index) .lt. char(128))) then
         stringToBeChanged(index:index) = char( & 
     &              ichar(stringToBeChanged(index:index)) - 32)
      end if
   enddo  
end subroutine toUpperCase

!!
!! toLowerCase() - method to change case of the string to 
!! all lower case.
!!
!! stringToBeChanged: intent [inout] the character 
!!        array representing the string
subroutine toLowerCase(stringToBeChanged)
use m_datatypes
implicit none

character(*) stringToBeChanged
integer(kind=SP) stringLength, index

   stringLength = len(stringToBeChanged)

   do index = 1, stringLength
      if ((stringToBeChanged(index:index) .gt. char(64)) .and. &
     &    (stringToBeChanged(index:index) .lt. char(97))) then
         stringToBeChanged(index:index) = char(  & 
     &              ichar(stringToBeChanged(index:index)) + 32)
      end if
   enddo  
end subroutine toLowerCase

!!
!! capitalise() - capitalises a string
!! stringToBeChanged: intent [inout] the character 
!!        array representing the string
subroutine capitalise(stringToBeChanged)
use m_datatypes
implicit none
   
character(*) stringToBeChanged
integer(kind=SP) stringLength, index

   stringLength = len(stringToBeChanged)

   call toUpperCase(stringToBeChanged(1:1))
   call toLowerCase(stringToBeChanged(2:stringLength))

end subroutine capitalise

!!
!! isStronglyBonded() - check if the atom pairs are strongly bonded
!! atomIndex1, atomIndex2 : the atom indices to check for
logical(kind=SP) function isStronglyBonded(atomIndex1, atomIndex2) 
use m_input
use m_datatypes

implicit none

integer(kind=SP) :: i, atomIndex1, atomIndex2

   do i = 1, atoms(atomIndex1)%noOfBonds
      if ((atoms(atomIndex1)%bondedAtoms(i) .eq. atomIndex2) &
     &   .and. (atoms(atomIndex1)%bondTypes(i) .ne. WEAK_BOND)) then
         isStronglyBonded = .true.
         return
      endif
   enddo

   isStronglyBonded = .false.
   return
end function isStronglyBonded

!!
!! getBondType() - get the bond type for this atom pair
!! atomIndex1, atomIndex2 : the atom indices to check for
integer(kind=SP) function getBondType(atomIndex1, atomIndex2) 
use m_input
use m_datatypes

implicit none

integer(kind=SP) :: i, atomIndex1, atomIndex2

   do i = 1, atoms(atomIndex1)%noOfBonds
      if (atoms(atomIndex1)%bondedAtoms(i) .eq. atomIndex2) then
         getBondType = atoms(atomIndex1)%bondTypes(i)
         return
      endif
   enddo

   getBondType = NO_BOND
   return
end function getBondType

!!
!! getBondOrder() - get the number of atoms connected to this atom
!! atomIndex : the atom index to check for
integer(kind=SP) function getBondOrder(atomIndex) 
use m_input
use m_datatypes

implicit none

integer(kind=SP) :: atomIndex

   getBondOrder = atoms(atomIndex)%noOfBonds

   return
end function getBondOrder

!!
!! getStrongBondOrder() - get the number of atoms connected to this atom
!! via a strong bond
!! atomIndex : the atom index to check for
integer(kind=SP) function getStrongBondOrder(atomIndex) 
use m_input
use m_datatypes

implicit none

integer(kind=SP) :: atomIndex

   getStrongBondOrder = atoms(atomIndex)%noOfBonds - atoms(atomIndex)%noOfWeakBonds

   return
end function getStrongBondOrder

!!
!! getBondOrderInFragment() - get bond order in fragment
!! fragIndex - the fragment to be probed
!! atomIndex - the atom index to be probed in the fragment
integer(kind=SP) function getBondOrderInFragment(fragIndex, atomIndex)
use m_input
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: fragIndex, atomIndex, degree, i, connIndex

    degree = 0
    do i = 1, atoms(atomIndex)%noOfBonds
       connIndex = atoms(atomIndex)%bondedAtoms(i)

       if (count(mask=fragments(fragIndex)%atomIndices.eq.connIndex, dim=1) .gt. 0) then
          degree = degree + 1
       endif
    enddo

    getBondOrderInFragment = degree
end function getBondOrderInFragment

!!
!! getStrongBondOrderInFragment() - get bond order in fragment, of strongly
!! connected atoms only
!! fragIndex - the fragment to be probed
!! atomIndex - the atom index to be probed in the fragment
integer(kind=SP) function getStrongBondOrderInFragment(fragIndex, atomIndex)
use m_input
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: fragIndex, atomIndex, degree, i, connIndex

    degree = 0

    if (atomIndex .le. 0) return

    do i = 1, atoms(atomIndex)%noOfBonds

       if (atoms(atomIndex)%bondTypes(i) .eq. WEAK_BOND) then
          cycle
       endif

       connIndex = atoms(atomIndex)%bondedAtoms(i)

       if (count(mask=fragments(fragIndex)%atomIndices.eq.connIndex, dim=1) .gt. 0) then
          degree = degree + 1
       endif
    enddo

    getStrongBondOrderInFragment = degree
end function getStrongBondOrderInFragment

!!
!! findDistance() - find the distance between two atoms a and b
real(kind=DP) function findDistance(a, b)
use m_input
use m_datatypes

implicit none

real(kind=DP) :: x, y, z
integer(kind=SP) :: a, b

    x = atoms(a)%x - atoms(b)%x
    y = atoms(a)%y - atoms(b)%y
    z = atoms(a)%z - atoms(b)%z

    findDistance = sqrt(x*x + y*y + z*z)
end function findDistance

!!
!! isPendantAtom() - is the given atom index a pendant atom
!! atomIndex : the atom index to check for
logical(kind=SP) function isPendantAtom(atomIndex)
use m_input
use m_datatypes

implicit none

integer(kind=SP) :: atomIndex

   !isPendantAtom = ((atoms(atomIndex)%noOfBonds .eq. 1)  .and. (atoms(atomIndex)%atomicNumber .eq. 1))
   isPendantAtom = (atoms(atomIndex)%atomicNumber .eq. 1)

   return
end function isPendantAtom

!!
!! isPendantAtom2() - is the given atom index a pendant atom
!! atomIndex : the atom index to check for
logical(kind=SP) function isPendantAtom2(atomIndex)
use m_input
use m_datatypes

implicit none

integer(kind=SP) :: atomIndex

   ! isPendantAtom2 = (atoms(atomIndex)%noOfBonds .eq. 1)
   isPendantAtom2 = ((atoms(atomIndex)%noOfBonds .eq. 1) .or. (atoms(atomIndex)%atomicNumber .eq. 1))

   return
end function isPendantAtom2


!!
!! isRingAtom() - is the given atom index a part of any ring?
!! atomIndex : the atom index to check for
logical(kind=SP) function isRingAtom(atomIndex)
use m_input
use m_datatypes

implicit none

integer(kind=SP) :: atomIndex, i

   isRingAtom = .false.

   do i = 1, numberOfRings
      if (count(mask=(rings(i)%ringAtoms.eq.atomIndex), dim=1) .gt. 0) then
         isRingAtom = .true.
         return
      endif
   enddo

end function isRingAtom

!!
!! isSuspendedAtom() - check whether this is suspended
!! atom. ie. this atom is connected to two or more atoms, 
!! which are part of the fragment, but not atomIndex
logical(kind=SP) function isSuspendedAtom(fragIndex, atomIndex) 
use m_input
use m_options
use m_fragment
use m_datatypes

implicit none

logical(kind=SP) :: tst
integer(kind=SP) :: i, noOfConnected, fragIndex, atomIndex

   isSuspendedAtom = .false.

   noOfConnected = 0

   do i = 1, fragments(fragIndex)%numberOfTotalAtoms
      if (isStronglyBonded(atomIndex, fragments(fragIndex)%atomIndices(i))) then
         noOfConnected = noOfConnected + 1
      endif
   end do

   tst = .false.
   if (.not. doAddDummyAtoms) then
      tst = (noOfConnected .ge. 1)
   else
      tst = (noOfConnected .ge. 2)
   endif

   if ((tst) &
  &    .and. (count(mask=fragments(fragIndex)%atomIndices .eq. atomIndex, dim=1) .le. 0)) then
      isSuspendedAtom = .true.
   endif

   return
end function isSuspendedAtom

!!
!! isPlanarRingAtom() - is the given atom index a part of any planar ring?
!! atomIndex : the atom index to check for
logical(kind=SP) function isPlanarRingAtom(atomIndex)
use m_input
use m_datatypes

implicit none

integer(kind=SP) :: atomIndex, i

   isPlanarRingAtom = .false.

   do i = 1, numberOfRings
      if (.not. rings(i)%isPlanar) cycle

      if (count(mask=(rings(i)%ringAtoms.eq.atomIndex), dim=1) .gt. 0) then
         isPlanarRingAtom = .true.
         return
      endif
   enddo

end function isPlanarRingAtom

!!
!! magnitude() - finds the magnitude of a vector
real(kind=DP) function magnitude(v)
use m_input
use m_datatypes

implicit none

type(t_point3d) :: v
real(kind=DP)    :: mag

   mag = sqrt((v%x*v%x) + (v%y*v%y) + (v%z*v%z))

   magnitude = mag
   return
end function magnitude

!!
!! normalize() - normalizes an vector
subroutine normalize(v)
use m_input
use m_datatypes

implicit none

type(t_point3d) :: v
real(kind=DP)    :: mag

   mag = magnitude(v)

   v%x = v%x / mag
   v%y = v%y / mag
   v%z = v%z / mag
end subroutine normalize

!!
!! isNullVector() - true / false indicating whether its a null vector
logical(kind=SP) function isNullVector(v)
use m_input
use m_datatypes

implicit none

type(t_point3d) :: v

   isNullVector = ((v%x .eq. 0) .and. (v%y .eq. 0) .and. (v%z .eq. 0))
   return
end function isNullVector

!!
!! dot() - dot product of two vectors
real(kind=DP) function dot(a, b)
use m_input
use m_datatypes

implicit none

type(t_point3d) :: a, b
real(kind=DP)    :: aDotb

   aDotb = 0.0
   
   aDotb = aDotb + (a%x * b%x)
   aDotb = aDotb + (a%y * b%y)
   aDotb = aDotb + (a%z * b%z)

   dot = aDotb
   return
end function dot

!!
!! cross() - cross product of two vectors
type(t_point3d) function cross(a, b)
use m_input
implicit none

type(t_point3d) :: a, b, res

   res%x = a%y * b%z - a%z * b%y;
   res%y = a%z * b%x - a%x * b%z;
   res%z = a%x * b%y - a%y * b%x;

   cross = res
   return
end function cross

!!
!! angleBetween() - find angle between two vectors
real(kind=DP) function angleBetween(a, b)
use m_input
use m_datatypes

implicit none

type(t_point3d) :: a, b
real(kind=DP)    :: aDotb, ab

    aDotb = dot(a, b)
    ab    = magnitude(a) * magnitude(b)
   
    angleBetween = acos(aDotb / ab)
    return
end function angleBetween


!!
!! writeFragments() - write the fragments on to standard o/p
subroutine writeFragments(fileName)
use m_input
use m_units
use m_options
use m_strings
use m_fragment
use m_datatypes

implicit none

character(*)     :: fileName
integer(kind=SP) :: i, j, k, m
real(kind=DP)    :: averageSize, totalFragmentAtoms

   ! open xyz file
   open(u_xyz, file=fileName)
   open(u_key, file=prefix(1:iPrefixSize) // keyFileName)

   ! write(*,*) numberOfFragments
   write(u_xyz,*) numberOfFragments
   flush(u_xyz)
   write(u_key,*) numberOfFragments
   flush(u_key)

   do m = 1, numberOfFragments+numberOfOverlapFragments
      if (allocated(currentFragmentPositions)) then
         i = currentFragmentPositions(m)
      else
         i = m
      endif

      ! write(*,*) fragments(i)%numberOfTotalAtoms
      write(u_xyz,*) fragments(i)%numberOfTotalAtoms
      flush(u_xyz)
      write(u_key,*) fragments(i)%numberOfTotalAtoms
      flush(u_key)

      fragments(i)%numberOfContractions = 0
      do j = 1, fragments(i)%numberOfTotalAtoms
         k = fragments(i)%atomIndices(j)
         write(u_key,*) k
         flush(u_key)
         ! write(*,*) atoms(k)%symbol, atoms(k)%x, atoms(k)%y, atoms(k)%z
         write(u_xyz,'(A2,3F15.6)') atoms(k)%symbol, atoms(k)%x, atoms(k)%y, atoms(k)%z
         flush(u_xyz)

         fragments(i)%numberOfContractions = fragments(i)%numberOfContractions + contractionMap(k)
      enddo

      fragments(i)%numberOfContractions = fragments(i)%numberOfContractions + & 
                &  (fragments(i)%numberOfDummyAtoms * dummyContraction)
   enddo

   ! close file
   close(u_xyz)
   close(u_key)

   averageSize = 0.0
   totalFragmentAtoms = 0.0
   do m = 1, numberOfFragments
      if (allocated(currentFragmentPositions)) then
         i = currentFragmentPositions(m)
      else
         i = m
      endif

      if (isFinalFrags) then
!ANUJA         write(u_mainout,'(4i5)') i, fragments(i)%numberOfTotalAtoms+fragments(i)%numberOfDummyAtoms, & 
!ANUJA        &           fragments(i)%numberOfContractions, fragments(i)%numberOfFreezedAtom
         write(u_mainout,'(4i5)') i, fragments(i)%numberOfTotalAtoms+fragments(i)%numberOfDummyAtoms 
!        &           fragments(i)%numberOfContractions, fragments(i)%numberOfFreezedAtom
      endif

      averageSize = averageSize + fragments(i)%numberOfTotalAtoms+fragments(i)%numberOfDummyAtoms
      totalFragmentAtoms = totalFragmentAtoms + fragments(i)%numberOfTotalAtoms
   enddo

   call computePickInfoStats()

   if (isFinalFrags) then
     if (numberOfOverlapFragments .gt. 0) then
         write(u_mainout,*) '   '
         write(u_mainout,*) 'Summary of Overlaps:'
      
         do i = numberOfFragments+1, numberOfFragments+numberOfOverlapFragments
!ANUJA            write(u_mainout,'(4i5)') i, fragments(i)%numberOfTotalAtoms+fragments(i)%numberOfDummyAtoms, &
!ANUJA       &                    fragments(i)%numberOfContractions, fragments(i)%numberOfFreezedAtom
            write(u_mainout,'(4i5)') i, fragments(i)%numberOfTotalAtoms+fragments(i)%numberOfDummyAtoms
!       &                    fragments(i)%numberOfContractions, fragments(i)%numberOfFreezedAtom
         enddo
     endif

      write(u_mainout,*) '   '

      write(u_mainout,'("Number of main fragments     : ", i3)') numberOfFragments
      write(u_mainout,'("Number of overlap fragments     : ", i3)') numberOfOverlapFragments
      write(u_mainout,'("Average size of fragments    : ", f5.2)') (averageSize / numberOfFragments)
      write(u_mainout,'("Minimum Goodness             : ", f5.2, " @ ", i5)') minGoodness, minGoodnessIndex
      write(u_mainout,'("Minimum non pendant Goodness : ", f5.2, " @ ", i5)') minNonPendantGoodness, minNonPendantIndex
  
      totalFragmentAtoms = totalFragmentAtoms / numberOfAtoms ! ??

      write(u_mainout,'("Scaling Factor               : ", f5.2)') totalFragmentAtoms
      write(u_mainout,*)
   endif


!   call ENGSCL(totalFragmentAtoms)
end subroutine writeFragments

!!
!! writeFragmentsAsXYZ() - write the fragments on to standard o/p and to .xyz file
subroutine writeFragmentsAsXYZ(fileName)
use m_units
use m_input
use m_fragment
use m_datatypes

implicit none

character(*)     :: fileName
integer(kind=SP) :: i, j, k, m

   ! open xyz file
   open(u_xyz, file=fileName)

   do m = 1, numberOfFragments
      if (allocated(currentFragmentPositions)) then
         i = currentFragmentPositions(m)
      else
         i = m
      endif

      write(u_xyz,'(I5)') fragments(i)%numberOfTotalAtoms + fragments(i)%numberOfDummyAtoms
      write(u_xyz,'("frag",I5)') i
      do j = 1, fragments(i)%numberOfTotalAtoms
         k = fragments(i)%atomIndices(j)
         write(u_xyz,'(A2,3F15.6)') atoms(k)%symbol, atoms(k)%x, atoms(k)%y, atoms(k)%z
      enddo
      do j = 1, fragments(i)%numberOfDummyAtoms
         write(u_xyz,'(A2,3F15.6)') fragments(i)%dummyAtoms(j)%symbol, & 
        &  fragments(i)%dummyAtoms(j)%x, fragments(i)%dummyAtoms(j)%y, &
        &  fragments(i)%dummyAtoms(j)%z
      enddo
   enddo

   ! close the files
   close(u_xyz)
end subroutine writeFragmentsAsXYZ

!!
!! findDistanceGoodness() - find the distance goodness of a given atom 
!! index, in a given fragment
real(kind=DP) function findDistanceGoodness(atomIndex, fragIndex) 
use m_input
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: i, j, atomIndex, fragIndex
real(kind=DP)    :: minDistanceGoodness, distance

    minDistanceGoodness = -1.0

    if (count(mask=fragments(fragIndex)%atomIndices .eq. atomIndex, dim=1) .le. 0) then
       findDistanceGoodness = minDistanceGoodness
       return
    endif

    do i = 1, numberOfAtoms
       if (i .eq. atomIndex) cycle

       distance = findDistance(atomIndex, i)

       ! then this atom has to be present in the specified fragment
       if (count(mask=fragments(fragIndex)%atomIndices .eq. i, dim=1) .le. 0) then
          ! if not, then the this the min dist goodness
          if (minDistanceGoodness .eq. -1.0) then
             minDistanceGoodness = distance
          else
             minDistanceGoodness = min(minDistanceGoodness, distance)
          endif
       endif
    enddo

    findDistanceGoodness = minDistanceGoodness
end function findDistanceGoodness

!!
!! writePickInfo() - write the pick info on the std. o/p
subroutine writePickInfo() 
use m_input
use m_units
use m_options
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: i, j
real(kind=DP) :: distance 
real(kind=DP), dimension(:,:), allocatable :: allGoodness

!ANUJA    write(u_mainout,*) numberOfAtoms 
    if (isFinalFrags) then
       write(u_mainout,*)
       write(u_mainout,*) 'Atom goodness index :'
       do i=1, numberOfAtoms
          write(u_mainout,'(i5, ":", f5.1)') i, atomGoodnessMap(i)
       enddo
    endif

    if (verbose) then
       allocate(allGoodness(numberOfAtoms, numberOfFragments))

       write(u_mainout,*) 'All atom goodness index :'
       ! first update the FMT
       do i = 1, numberOfFragments
          do j = 1, fragments(i)%numberOfTotalAtoms
             ! find the goodness values
             distance = findDistanceGoodness(fragments(i)%atomIndices(j), i)

             ! update the tables             
             allGoodness(fragments(i)%atomIndices(j), i) = distance             
          enddo          
       enddo

       do i = 1, numberOfAtoms
          write(u_mainout,'(I5,"(",I3,I2") : ", 50F5.1)') i, fragmentAtomMap(i), &
          &                             count(mask=allGoodness(i,:).ne.0,dim=1), &
          &                             (allGoodness(i,j), j=1, numberOfFragments)
       enddo

       deallocate(allGoodness)
    endif

!ANUJA    write(u_mainout,*) 'Fragment pick index :'
!ANUJA    write(u_mainout,*) (fragmentAtomMap(i), i=1, numberOfAtoms)

    !minGoodness =  minval(atomGoodnessMap)
    !minGoodnessIndex = maxloc(atomGoodnessMap, dim=1, mask=atomGoodnessMap.eq.minval(atomGoodnessMap))

    !write(*,*) 'Minimum goodness ', minGoodness, ' at ', minGoodnessIndex

    !minNonPendantGoodness = -1.0
    !minNonPendantIndex    = 0

    !do i = 1, numberOfAtoms
    !   if (.not. isPendantAtom(i)) then
    !      if (minNonPendantGoodness.eq.-1.0) then
    !         minNonPendantGoodness = atomGoodnessMap(i)
    !         minNonPendantIndex    = i
    !      else
    !         if (minNonPendantGoodness .gt. atomGoodnessMap(i)) then
    !            minNonPendantGoodness = atomGoodnessMap(i)
    !            minNonPendantIndex    = i
    !         endif
    !      endif
    !   endif
    !enddo

!ANUJA    write(u_mainout,*) 'Minimum goodness ', minGoodness, ' at ', minGoodnessIndex
!ANUJA    write(u_mainout,*) 'Minimum non pendant goodness ', minNonPendantGoodness, ' at ', &
!ANUJA   &                                            minNonPendantIndex

    if (count(mask=fragmentAtomMap.eq.0, dim=1) .gt. 0) then
       write(u_mainout,*) 'Warning! There is atleast one atom that is never picked!'
    endif    
end subroutine writePickInfo

!!
!! computePickInfo()
subroutine computePickInfoStats()
use m_input
use m_options
use m_fragment
use m_datatypes

implicit none

integer(kind=SP) :: i, j

    minGoodness =  minval(atomGoodnessMap)
    minGoodnessIndex = maxloc(atomGoodnessMap, dim=1, mask=atomGoodnessMap.eq.minval(atomGoodnessMap))

    minNonPendantGoodness = -1.0
    minNonPendantIndex    = 0

    do i = 1, numberOfAtoms
       if (.not. isPendantAtom(i)) then
          if (minNonPendantGoodness.eq.-1.0) then
             minNonPendantGoodness = atomGoodnessMap(i)
             minNonPendantIndex    = i
          else
             if (minNonPendantGoodness .gt. atomGoodnessMap(i)) then
                minNonPendantGoodness = atomGoodnessMap(i)
                minNonPendantIndex    = i
             endif
          endif
       endif
    enddo
end subroutine computePickInfoStats

!!
!! writeXML() - write XML file of the pseudo molecular graph in the
!! supplied file name
subroutine writeXML(fileName)
use m_units
use m_input
use m_strings
use m_fragment
use m_atomInfo
use m_datatypes

implicit none

character(*)      :: fileName
character(len=80) :: lineToPrint
integer(kind=SP)  :: i, j

   ! open xml file
   open(u_xml, file=fileName, pad='NO')

   ! write the header
   write(u_xml,'(80a)') trim(xmlHeader)
   write(u_xml,'(80a)') trim(xmlComment)
   write(u_xml,'(80a)') '<molecule title="' // trim(fileName) // '">'
  
   ! and then the other stuff
   do i = 1, numberOfAtoms
      lineToPrint(1:80) = ""
      write(lineToPrint,*) (atoms(i)%index-1)
      lineToPrint = leftTrim(lineToPrint)
      write(u_xml,*) tabString // '<atom atomIndex="' // trim(lineToPrint) // '"'
      write(u_xml,*) tabString // ' symbol="' // trim(atomicSymbols(atoms(i)%atomicNumber)) // '"'

      lineToPrint(1:80) = ""
      write(lineToPrint,'(f15.6)') (atoms(i)%x)
      lineToPrint = leftTrim(lineToPrint)
      write(u_xml,*) tabString // ' x="' // trim(lineToPrint) // '"'
      lineToPrint(1:80) = ""
      write(lineToPrint,'(f15.6)') (atoms(i)%y)
      lineToPrint = leftTrim(lineToPrint)
      write(u_xml,*) tabString // ' y="' // trim(lineToPrint) // '"'
      lineToPrint(1:80) = ""
      write(lineToPrint,'(f15.6)') (atoms(i)%z)
      lineToPrint = leftTrim(lineToPrint)
      write(u_xml,*) tabString // ' z="' // trim(lineToPrint) // '"'
      write(u_xml,*) tabString // ' charge="0.0">'
      write(u_xml,*) tabString // ' <connectedList>'
       
      ! connected list for only pseudo atoms      
      do j = 1, atoms(i)%noOfBonds
         write(u_xml,*) tabString // '<ca '
         lineToPrint(1:80) = ""
         write(lineToPrint,*) (atoms(i)%bondedAtoms(j)-1)
         lineToPrint = leftTrim(lineToPrint)
         write(u_xml,*) tabString // 'index="' // trim(lineToPrint) &
        &               // '" bondType="Single bond" />'
      enddo   

      write(u_xml,*) tabString // ' </connectedList>'
      write(u_xml,*) tabString // '</atom>'
   enddo

   write(u_xml,'(80a)') '</molecule>'

   ! close xml file
   close(u_xml)   
end subroutine writeXML

!!
!! writeGAMESSInputFiles() - write GAMESS input files for the
!! fragments generated, with appropriate selection matrix 
!! entries.
subroutine writeGAMESSInputFiles()
use m_units
use m_input
use m_strings
use m_options
use m_fragment
use m_datatypes
use m_atomInfo
use m_cardinal
use m_atomInfo, only : getAtomicNumber

implicit none

character(len=30) :: fragFileName
integer(kind=SP) :: i, j, k, m, index1, index2, index3, index4, noOfSelectedAtoms
integer(kind=SP) :: atomicNumber, theIndex, numberOfSelectionElements
integer(kind=SP), dimension(:), allocatable :: selectionMatrix, elementsNotToFreez

    open(u_lst, file=prefix(1:iPrefixSize) // 'frag.lst')
    write(u_lst, *) numberOfFragments+numberOfOverlapFragments

    ! iterate through all the fragments
    do m = 1, numberOfFragments+numberOfOverlapFragments
       ! use the original index
       i = m

       index1 = i / 1000
       index2 = (i - index1 * 1000) / 100
       index3 = (i - index1 * 1000 - index2 * 100) / 10
       index4 = (i - index1 * 1000 - index2 * 100 - index3 * 10)

       fragFileName(1:4) = "frag"
       write(fragFileName(5:8),'(4I1)') index1, index2, index3, index4
       
       open(u_frg, file=prefix(1:iPrefixSize) // fragFileName(1:8) // ".inp")

       ! not use the translated positions
       i = currentFragmentPositions(m)

       ! make the selection matrix
       numberOfSelectionElements = fragments(i)%numberOfTotalAtoms + fragments(i)%numberOfDummyAtoms
       numberOfSelectionElements = (numberOfSelectionElements * (numberOfSelectionElements+1)) / 2

       ! if (mod((numberOfSelectionElements * numberOfSelectionElements+1), 2) .ne. 1) then
       !    numberOfSelectionElements = numberOfSelectionElements + 1
       ! endif

       allocate(selectionMatrix(numberOfSelectionElements))
       allocate(elementsNotToFreez(fragments(i)%numberOfTotalAtoms + fragments(i)%numberOfDummyAtoms))
       selectionMatrix = 0
       theIndex = 0
       elementsNotToFreez = 0
       noOfSelectedAtoms = 0

       ! write(*,*) 'Atoms in ', i, ' are ', fragments(i)%atomIndices

       do j = 1, fragments(i)%numberOfTotalAtoms
          do k = 1, j
             theIndex = theIndex + 1

             ! write(*,*) fragments(i)%atomIndices(j), fragments(i)%atomIndices(k), &
             ! &  fragmentAtomPairMap(fragments(i)%atomIndices(j), fragments(i)%atomIndices(k)), i

             if (fragmentAtomPairMap(fragments(i)%atomIndices(j), & 
            &                        fragments(i)%atomIndices(k)) .eq. i) then
               selectionMatrix(theIndex) = 1

               if (fragments(i)%atomIndices(j) .eq. fragments(i)%atomIndices(k)) then
                 !  write(*,*) fragments(i)%atomIndices(j) , ' and ' , fragments(i)%atomIndices(k), &
                 ! &           ' will be picked from ', i, theIndex

!ANUJA                   write(u_mainout,'(i5," and ", i5, " will be picked from ", 2i5)') fragments(i)%atomIndices(j) , & 
!ANUJA                  &               fragments(i)%atomIndices(k), i, theIndex
!ANUJA VERBOSE                   write(u_mainout,'("Atom ", i3," is picked from fragment ", i3)') fragments(i)%atomIndices(j), i 

                   noOfSelectedAtoms = noOfSelectedAtoms + 1
                   elementsNotToFreez(noOfSelectedAtoms) = 1
               endif
             endif
          enddo
       enddo

       ! first write the GAMESS headers


       call writeGAMESSHeader(u_frg, prefix(1:iPrefixSize) // fragFileName(1:8), i, & 
                &      elementsNotToFreez, numberOfSelectionElements, &
                &      (fragments(i)%numberOfTotalAtoms+fragments(i)%numberOfDummyAtoms))

       ! then write the coordinates
       write(u_lst, *) fragments(i)%numberOfTotalAtoms+fragments(i)%numberOfDummyAtoms, &
       &               fragments(i)%numberOfDummyAtoms

       ! ... first write the actual atoms
       do j = 1, fragments(i)%numberOfTotalAtoms
          atomicNumber = atoms(fragments(i)%atomIndices(j))%atomicNumber
          write(u_frg, '(A3,2X,I4,3(5X,F10.5))') atoms(fragments(i)%atomIndices(j))%symbol, &
         &                atomicNumber, atoms(fragments(i)%atomIndices(j))%x, &
         &                atoms(fragments(i)%atomIndices(j))%y, &
         &                atoms(fragments(i)%atomIndices(j))%z
          write(u_lst, '(A3,2X,3(5X,F10.5))') atoms(fragments(i)%atomIndices(j))%symbol, &
         &                atoms(fragments(i)%atomIndices(j))%x, &
         &                atoms(fragments(i)%atomIndices(j))%y, &
         &                atoms(fragments(i)%atomIndices(j))%z
       enddo

       ! ... then the dummy atoms
       do j = 1, fragments(i)%numberOfDummyAtoms
          atomicNumber = fragments(i)%dummyAtoms(j)%atomicNumber
          write(u_frg, '(A3,2X,I4,3(5X,F10.5))') fragments(i)%dummyAtoms(j)%symbol, &
         &                atomicNumber, fragments(i)%dummyAtoms(j)%x, &
         &                fragments(i)%dummyAtoms(j)%y, &
         &                fragments(i)%dummyAtoms(j)%z
          write(u_lst, '(A3,2X,3(5X,F10.5))') fragments(i)%dummyAtoms(j)%symbol, &
         &                fragments(i)%dummyAtoms(j)%x, &
         &                fragments(i)%dummyAtoms(j)%y, &
         &                fragments(i)%dummyAtoms(j)%z
       enddo

       ! the end of data
       write(u_frg, *) "$end"

       ! then the fragment DM selection matrix
       write(u_frg, *) "$mat"

       ! write the matrix .. and free the memory
       write(u_frg, '(80i1)') selectionMatrix(1:numberOfSelectionElements)

       deallocate(selectionMatrix, elementsNotToFreez)

       ! write the atom indices
       ! write(*,*) 'its ', i, fragments(i)%numberOfTotalAtoms
       do j = 1, fragments(i)%numberOfTotalAtoms
          write(u_frg, *) fragments(i)%atomIndices(j)
       enddo

       ! the end of slection data
       write(u_frg, *) "$end"

       ! close the fragment
       close(u_frg)
    enddo

    close(u_lst)
end subroutine writeGAMESSInputFiles

!!
!! writeGAMESSHeader() - write the GAMESS headers with proper options
subroutine writeGAMESSHeader(u_gam, fragName, fragIndex, elementsNotToFreez, numberOfSelectionElements, noOfFragmentAtoms)
use m_input
use m_options
use m_fragment
use m_datatypes

implicit none

character(*)     :: fragName
integer(kind=SP) :: u_gam, fragIndex, numberOfSelectionElements, i, j, & 
                   & numberOfFreezeLines, noOfAtomsToFreez, noOfFragmentAtoms, nprint
integer(kind=SP), dimension(*) :: elementsNotToFreez
integer(kind=SP), dimension(:), allocatable :: selectionMatrix, elementsToFreez
real(kind=DP) :: subFragmentCutoff

        
    ! the $control part
    ! NOTE: on nprint option, The current version of GAMESS has some problem
    ! with the nprint option being set to -5 especially in parallel environment.
    ! So if you have any problem running the code in parallel, with unexplained
    ! killing of processes then just remove nprint=-5 options in the following
    ! code and recompile.
    nprint=-5 
    if (verbose .or. isPointCharge) nprint=7

    if (isOptimizationRun) then
       if ((.not. usePrevDM) .and. wheelWithInWheel) then
          write(u_gam, '(" $contrl scftyp=",A4," mplevl=", I2 ," runtyp=optmta nprint=",I2," icharg=",I2," mult=",I2," $end")') & 
          & trim(scfType), &
          & mpLevel, & 
          & nprint, & 
          & fragmentCharges(fragIndex), &
          & fragmentMultiplicity(fragIndex)
          energyTolerance = 1.0d-8
          integralCutoff  = 1.0d-10
       else
          write(u_gam, '(" $contrl scftyp=",A4," mplevl=", I2 ," runtyp=optfrg nprint=",I2," icharg=",I2," mult=",I2," $end")') &
          & trim(scfType), &
          & mpLevel, & 
          & nprint, & 
          & fragmentCharges(fragIndex), &
          & fragmentMultiplicity(fragIndex)
       endif
       if (isSelGrad) then
          if (fragDoGrad(fragIndex).eq.0) write(u_gam, '(" $contrl runtyp=engfrg $end")') 
       endif
    else 
       ! check if it is a hessian run
       if (isHessianRun .and. (optimizationStep.eq.0)) then
          write(u_gam, '(" $contrl scftyp=",A4," mplevl=", I2 ," runtyp=hesfrg nprint=",I2," icharg=",I2," mult=",I2," $end")') & 
          & trim(scfType), &
          & mpLevel, & 
          & nprint, & 
          & fragmentCharges(fragIndex), &
          & fragmentMultiplicity(fragIndex)
       else if (directFragSCF) then
          write(u_gam, '(" $contrl scftyp=",A4," mplevl=", I2 ," runtyp=engfrg nprint=",I2," icharg=",I2," mult=",I2," $end")') &
          & trim(scfType), &
          & mpLevel, & 
          & nprint, & 
          & fragmentCharges(fragIndex), &
          & fragmentMultiplicity(fragIndex)
       else
      write(u_gam, '(" $contrl scftyp=",A4," mplevl=", I2 ," runtyp=engfrg nprint=",I2," icut=8 icharg=",I2," mult=",I2," $end")') &
          & trim(scfType), &
          & mpLevel, & 
          & nprint, & 
          & fragmentCharges(fragIndex), &
          & fragmentMultiplicity(fragIndex)
       endif
    endif

    ! the $system part
    if (useDDIMemory) then
        write(u_gam, '(" $system timlim=", I8 , " memory=", I9 , " memddi=", I9 ," $end")') & 
               &      timeLimit, memoryInWords, ddiMemoryInWords
        if (mpLevel .gt. 1) then
            write(u_gam, *) '$mp2 aoints=dist $end'
        endif
    else
        write(u_gam, '(" $system timlim=", I8 , " memory=", I9 , " $end")') & 
               &      timeLimit, memoryInWords
    endif

    ! the $scf part
    write(u_gam, '(" $scf dirscf=.", L1, ". conv=", D8.2, " diis=.", L1, ". maxvt=", I4, " $end")') &
   &      directFragSCF, densityConvergence, doFragDIIS, maxSCFIterations

    ! the $intgrl part
    if (.not.directFragSCF) then
       write(u_gam, *) "$intgrl SCHWRZ=.true. $end"
    endif

    ! the $statpt part
    if (isOptimizationRun) then
       write(u_gam, *) "$statpt nstep=0 $end"
    endif

    ! check if we need to freeze atoms
    if (freezAtoms) then
       write(u_gam, *) "$statpt ifreez(1)= "
       noOfAtomsToFreez = 0
       do i = 1, noOfFragmentAtoms
          if (elementsNotToFreez(i) .eq. 0) then
             noOfAtomsToFreez = noOfAtomsToFreez + 1
          endif
       enddo

       fragments(fragIndex)%numberOfFreezedAtom = noOfAtomsToFreez

       numberOfFreezeLines = noOfAtomsToFreez/10      
       
       allocate(elementsToFreez(noOfAtomsToFreez*3))
       elementsToFreez = 0
       j = 1
       
       do i=1, noOfFragmentAtoms
          if (elementsNotToFreez(i) .eq. 0) then
             elementsToFreez(j)   = (i-1)*3
             elementsToFreez(j+1) = ((i-1)*3)+1
             elementsToFreez(j+2) = ((i-1)*3)+2
             j = j + 3
          endif
       enddo

       do i=1, (numberOfFreezeLines*30), 10
          write(u_gam, '(I5,",",I5,",",I5,",",I5,",",I5,",",I5,",",I5,",",I5,",",I5,",",I5)') & 
         &              (elementsToFreez(j), j=i,i+9)
       enddo

       do j=i, i+(mod(noOfAtomsToFreez,10)*3)-2
          write(u_gam, '(I5,",")') elementsToFreez(j)
       enddo
       write(u_gam, '(I5)') elementsToFreez(j)

       write(u_gam, *) "$end"
       
       deallocate(elementsToFreez)
    endif

    ! the $guess part
    write(u_gam, *) "$guess guess=huckel $end"

    ! the most important $cntmta part
    if (wheelWithInWheel) then
       write(u_gam, *) "$cntmta verbse=.false. domwny=.true. savedm=.true. apndm=.true. $end"
    else       
       write(u_gam, '(" $cntmta verbse=.", L1, ". domwny=.false. savedm=.true. apndm=.true. $end")') &
   &         (verbose .or. isPointCharge)
    endif

    write(u_gam, '(" $cntmta seldm=.", L1, ". usecut=.true. engtl=", D8.2, " cintfg=", D8.2, " $end")') &
   &      (.not. setBasedDM), energyTolerance, integralCutoff

    ! wanna use multithreaded code
    if (.not. directFragSCF) then
       write(u_gam, '(" $cntmta usemt=.", L1, ". $end")') useMultiThreading
    endif

    ! reuse DM or is it different set of fragments?
    if (usePrevDM .and. isDifferentFrag .and.(.not. wheelWithInWheel)) then
       write(u_gam, *) "$cntmta rddm=.true. domwny=.true. reusdm=.false. $end"
    endif

    ! dummy atoms, fuzzy convergence etc.
    write(u_gam,'(" $cntmta fuzcov=.", L1, ". savgrd=.true. prntdm=.true. ndat=",i3," $end")') & 
   &      fuzzyConv, fragments(fragIndex)%numberOfDummyAtoms

    ! the wheel with in a wheel option
    if ((.not. usePrevDM) .and. wheelWithInWheel) then
       subFragmentCutoff = radiusCutoff/3
       if (subFragmentCutoff < 2.5) subFragmentCutoff = 2.5
       write(u_gam, '(" $cntmta maxsz=", I2, " radcut=", D8.2," $end")') maxFragmentSize/3, subFragmentCutoff
       write(u_gam, '(" $cntmta dirfrg=.f. doe0=.t. corgrd=.t. $end")')
    endif    

    ! notification is not required for the fragment jobs, and selected gradients?
    write(u_gam, '(" $cntmta notify=.f. selgrd=.", L1, ". $end")') doSelectedGradients

    ! the basis set
    write(u_gam, *) basisSet(2:len_trim(basisSet))

    ! hel-fey corrections?
    write(u_gam, '(" $cntmta helcor=.", L1, ". $end")') helFeyCorrection

    ! if dft run, then write its type
    if (isDFTRun) then
       write(u_gam, *) dftType(2:len_trim(dftType))

       ! use army grid?
       write(u_gam, '(" $cntmta armygd=.", L1, ". $end")') armyGrid
    endif

    ! the sign of energy and its derivatives .. only useful if we are using CG-MTA
    write(u_gam, '(" $setmta iensgn=", I2, " $end")') fragments(fragIndex)%signOfEnergy

    ! set based point charges?
    write(u_gam, '(" $setmta setcrg=.", L1, ". $end")') setBasedCharges

    ! data starts here
    write(u_gam, *) "$data"

    ! fragment name
    write(u_gam,*) fragName

    ! the symmetry
    write(u_gam,*) "c1"
end subroutine writeGAMESSHeader

!!
!! leftTrim() - spaces from the left of the string
character(len=80) function leftTrim(theString)
use m_datatypes

implicit none

character(*)     :: theString
integer(kind=SP) :: i, j

   theString = trim(theString)
   leftTrim   = ""

   do i = 1, len(theString)
      if (theString(i:i) .eq. ' ') then
         cycle
      else
         exit
      endif
   enddo

   do j = i, len(theString)
      leftTrim(j-i+1:j-i+1) = theString(j:j)
   enddo
end function leftTrim

!!
!! checkIOError() - check for an i/o error..
!! if there is an error, abort the program
subroutine checkIOError(ioError, fileName)
use m_units
use m_datatypes

implicit none

integer(kind=SP) :: ioError
character(*)    :: fileName

   if (ioError .ne. 0) then
      write(u_mainout,*) 'Error in reading file : ', fileName
!      call SETERR(10, "Error in reading file : "//fileName)
      call ABRT()
   endif
end subroutine checkIOError

!!
!! makeFormule() - make the formule of 
subroutine makeFormule()
use m_input
use m_units
use m_strings
use m_fragment

integer(kind=SP) :: i, j, k, l, m, dummy
integer(kind=SP), allocatable, dimension(:) :: signsOfEnergy
logical(kind=SP) :: foundSymbol, equalityMaintained

   numberOfUniqueAtoms = 0

   ! iterate through each atom and then, collect unique atoms
   do i = 1, numberOfAtoms
      foundSymbol = .false.

      if (numberOfUniqueAtoms .ne. 0) then
         do j = 1, numberOfUniqueAtoms
            if (molecularFormula(j)%symbol .eq. atoms(i)%symbol) then
               foundSymbol = .true.
               exit
            endif
         enddo
      else 
         j = 1
      endif

      if (.not. (foundSymbol)) then
         molecularFormula(numberOfUniqueAtoms+1)%symbol = atoms(i)%symbol
         molecularFormula(numberOfUniqueAtoms+1)%items  = 1
         numberOfUniqueAtoms = numberOfUniqueAtoms + 1
      else
         molecularFormula(j)%items = molecularFormula(j)%items + 1
      endif
   enddo

   ! print out the molecular formula of the whole molecule
   write(u_mainout,*) 'The molecular formula is : '
   do i = 1, numberOfUniqueAtoms
      write(u_mainout, '(A2," : ", I4,", ")', advance='NO') molecularFormula(i)%symbol, molecularFormula(i)%items
   enddo

   write(u_mainout,*)

   numberOfUniqueBonds = 0

   ! now do the bond stats
   do i = 1, numberOfUniqueAtoms
      do j = i, numberOfUniqueAtoms
         numberOfUniqueBonds = numberOfUniqueBonds + 1

         molecularBondFormula(numberOfUniqueBonds)%symbol1 = molecularFormula(i)%symbol
         molecularBondFormula(numberOfUniqueBonds)%symbol2 = molecularFormula(j)%symbol
         molecularBondFormula(numberOfUniqueBonds)%items   = 0
      enddo
   enddo

   do i = 1, numberOfAtoms
      do j = 1, atoms(i)%noOfBonds
         foundSymbol = .false.

         do k = 1, numberOfUniqueBonds
            !if (atoms(i)%bondedAtoms(j) .lt. i) cycle

            if (((molecularBondFormula(k)%symbol1 .eq. atoms(i)%symbol) &
                    & .and. (molecularBondFormula(k)%symbol2 .eq. atoms(atoms(i)%bondedAtoms(j))%symbol)) & 
                  .and. (atoms(i)%bondTypes(j) .ne. NO_BOND)) then
               foundSymbol = .true.
               exit
            endif

         enddo
         
         if (foundSymbol) then
            molecularBondFormula(k)%items = molecularBondFormula(k)%items + 1
         endif
      enddo
   enddo

   ! print out the bond stats
   write(u_mainout,*) 'Bond stats:'

   do i = 1, numberOfUniqueBonds
      write(u_mainout,'(A2,"-",A2," : ",I4)') molecularBondFormula(i)%symbol1, &
     &    molecularBondFormula(i)%symbol2, molecularBondFormula(i)%items
   enddo   

   ! now for the fragments
   do i = 1, numberOfFragments+numberOfOverlapFragments
      fragments(i)%numberOfUniqueAtoms = 0
      do j = 1, fragments(i)%numberOfTotalAtoms
         
         foundSymbol = .false.

         if (fragments(i)%numberOfUniqueAtoms .ne. 0) then
            do k = 1, fragments(i)%numberOfUniqueAtoms
               if (fragments(i)%molecularFormula(k)%symbol .eq. atoms(fragments(i)%atomIndices(j))%symbol) then
                  foundSymbol = .true.
                  exit
               endif
            enddo
         else 
            k = 1
         endif
         
         if (.not. (foundSymbol)) then
            fragments(i)%molecularFormula(fragments(i)%numberOfUniqueAtoms+1)%symbol = &
            &  atoms(fragments(i)%atomIndices(j))%symbol
            fragments(i)%molecularFormula(fragments(i)%numberOfUniqueAtoms+1)%items  = 1
            fragments(i)%numberOfUniqueAtoms = fragments(i)%numberOfUniqueAtoms + 1
         else
            fragments(i)%molecularFormula(k)%items = fragments(i)%molecularFormula(k)%items + 1
         endif
      enddo

      ! print out the molecular formula of the current fragment
      write(u_mainout,'("frag",I4, ":>  ")', advance='NO') i
      do j = 1, fragments(i)%numberOfUniqueAtoms
         write(u_mainout, '(A2," : ", I4,", ")', advance='NO') fragments(i)%molecularFormula(j)%symbol, &
        &  fragments(i)%molecularFormula(j)%items
      enddo

      write(u_mainout,*)

      fragments(i)%numberOfUniqueBonds = 0

      ! and now the bond stats
      do l = 1, fragments(i)%numberOfUniqueAtoms
         do m = l, fragments(i)%numberOfUniqueAtoms
            fragments(i)%numberOfUniqueBonds = fragments(i)%numberOfUniqueBonds + 1
            
            fragments(i)%molecularBondFormula(fragments(i)%numberOfUniqueBonds)%symbol1 = & 
            & fragments(i)%molecularFormula(l)%symbol
            fragments(i)%molecularBondFormula(fragments(i)%numberOfUniqueBonds)%symbol2 = & 
            & fragments(i)%molecularFormula(m)%symbol
            fragments(i)%molecularBondFormula(fragments(i)%numberOfUniqueBonds)%items   = 0
         enddo
      enddo
      
      do l = 1, fragments(i)%numberOfTotalAtoms
         do m = 1, atoms(fragments(i)%atomIndices(l))%noOfBonds
            foundSymbol = .false.
            
            do k = 1, fragments(i)%numberOfUniqueBonds
               !if (atoms(fragments(i)%atomIndices(l))%bondedAtoms(m) .lt. fragments(i)%atomIndices(l)) cycle
               
               if ((fragments(i)%molecularBondFormula(k)%symbol1 .eq. atoms(fragments(i)%atomIndices(l))%symbol) &
                    & .and. (fragments(i)%molecularBondFormula(k)%symbol2 & 
                    &        .eq. atoms(atoms(fragments(i)%atomIndices(l))%bondedAtoms(m))%symbol) &
                    & .and. (atoms(fragments(i)%atomIndices(l))%bondTypes(m) .ne. NO_BOND)) then
                  if (count(fragments(i)%atomIndices.eq.atoms(fragments(i)%atomIndices(l))%bondedAtoms(m), &
                  &     dim=1) .gt. 0) then
                     foundSymbol = .true.
                     exit
                  endif
               endif
            enddo
            
            if (foundSymbol) then
               fragments(i)%molecularBondFormula(k)%items = fragments(i)%molecularBondFormula(k)%items + 1
            endif
         enddo
      enddo

      ! print the bond stats
      write(u_mainout,*) '    Bond stats:'

      do j = 1, fragments(i)%numberOfUniqueBonds
         write(u_mainout,'("    ", A2,"-",A2," : ",I4)') fragments(i)%molecularBondFormula(j)%symbol1, &
        &                                        fragments(i)%molecularBondFormula(j)%symbol2, &
        &                                        fragments(i)%molecularBondFormula(j)%items
      enddo

      write(u_mainout,*) 
   enddo


   ! recompute the molecular formula and the bond formula based on the cardinality expression
   open(u_car, file=prefix(1:iPrefixSize) // 'carsign')
   read(u_car, *) dummy

   ! read in sign of energy terms
   if (numberOfOverlapFragments .gt. 0) then
      allocate(signsOfEnergy(numberOfOverlapFragments))
      read(u_car, *) (signsOfEnergy(i), i=1, numberOfOverlapFragments)
   else  
      allocate(signsOfEnergy(1))
   endif

   close(u_car)

   do i = 1, numberOfUniqueAtoms      
      cardinalMolecularFormula(i)%symbol = molecularFormula(i)%symbol
      cardinalMolecularFormula(i)%items  = 0

      ! first construct the molecular formula
      do j = 1, numberOfFragments+numberOfOverlapFragments
         do k = 1, fragments(j)%numberOfUniqueAtoms
            if (molecularFormula(i)%symbol .eq. fragments(j)%molecularFormula(k)%symbol) then               
               if (j .le. numberOfFragments) then
                  cardinalMolecularFormula(i)%items  = cardinalMolecularFormula(i)%items + fragments(j)%molecularFormula(k)%items
               else
                  cardinalMolecularFormula(i)%items  = cardinalMolecularFormula(i)%items + & 
                 &                                     (signsOfEnergy(j-numberOfFragments) * fragments(j)%molecularFormula(k)%items)
               endif
            endif
         enddo
      enddo      
   enddo

   write(u_mainout,*) 'The molecular formula based on cardinality is : '
   equalityMaintained = .true.
   do i = 1, numberOfUniqueAtoms
      write(u_mainout, '(A2," : ", I4,", ")', advance='NO') cardinalMolecularFormula(i)%symbol, cardinalMolecularFormula(i)%items
      equalityMaintained = equalityMaintained & 
      &                    .and. (cardinalMolecularFormula(i)%symbol .eq. molecularFormula(i)%symbol) &
      &                    .and. (cardinalMolecularFormula(i)%items .eq. molecularFormula(i)%items) 
   enddo

   write(u_mainout,*)
   write(u_mainout,*)
   if (equalityMaintained) then
      write(u_mainout,*) 'Cardinality of the molecular formula is maintained!'
   else
      write(u_mainout,*) 'Warning: Cardinality of the molecular formula seems to be incorrect!'
   endif
   write(u_mainout,*)

   do i = 1, numberOfUniqueBonds
      cardinalMolecularBondFormula(i)%symbol1 = molecularBondFormula(i)%symbol1
      cardinalMolecularBondFormula(i)%symbol2 = molecularBondFormula(i)%symbol2
      cardinalMolecularBondFormula(i)%items   = 0

      ! construct the bond formula
      do j = 1, numberOfFragments+numberOfOverlapFragments
         do k = 1, fragments(j)%numberOfUniqueBonds
            if ((((molecularBondFormula(i)%symbol1 .eq. fragments(j)%molecularBondFormula(k)%symbol1) & 
           &   .and. (molecularBondFormula(i)%symbol2 .eq. fragments(j)%molecularBondFormula(k)%symbol2))) &
           &   .or. (((molecularBondFormula(i)%symbol1 .eq. fragments(j)%molecularBondFormula(k)%symbol2) & 
           &   .and. (molecularBondFormula(i)%symbol2 .eq. fragments(j)%molecularBondFormula(k)%symbol1)))) then               
               if (j .le. numberOfFragments) then
                  cardinalMolecularBondFormula(i)%items  = cardinalMolecularBondFormula(i)%items & 
                 &                                         + fragments(j)%molecularBondFormula(k)%items
               else
                  cardinalMolecularBondFormula(i)%items  = cardinalMolecularBondFormula(i)%items + & 
                 &                 (signsOfEnergy(j-numberOfFragments) * fragments(j)%molecularBondFormula(k)%items)
               endif
            endif
         enddo
      enddo      
   enddo

   ! print out the bond stats
   write(u_mainout,*) 'Bond stats based on cardinality is:'

   equalityMaintained = .true.
   do i = 1, numberOfUniqueBonds
      write(u_mainout,'(A2,"-",A2," : ",I4)') cardinalMolecularBondFormula(i)%symbol1, & 
     &           cardinalMolecularBondFormula(i)%symbol2, cardinalMolecularBondFormula(i)%items
      equalityMaintained = equalityMaintained & 
      &                    .and. (cardinalMolecularBondFormula(i)%symbol1 .eq. molecularBondFormula(i)%symbol1) &
      &                    .and. (cardinalMolecularBondFormula(i)%symbol2 .eq. molecularBondFormula(i)%symbol2) &
      &                    .and. (cardinalMolecularBondFormula(i)%items .eq. molecularBondFormula(i)%items) 
   enddo  

   write(u_mainout,*)
   if (equalityMaintained) then
      write(u_mainout,*) 'Cardinality of the molecular bond stats is maintained!'
   else
      write(u_mainout,*) 'Warning: Cardinality of the molecular bond stats seems to be incorrect!'
   endif
   write(u_mainout,*)

   deallocate(signsOfEnergy)
end subroutine makeFormule
 
end module m_utilityFunctions

subroutine ABRT()
   use m_units
   write(u_mainout,*) 'ABRT called! Something went wrong .. better check what you are doing!!'
   STOP
end subroutine ABRT

! UTILITY FUNCTION TO RETURN THE PREFIX FOR FILE NAMES
FUNCTION GTPREFX(SIZ)
!
          IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!
          CHARACTER*255 GTPREFX
          INTEGER SIZ
          CHARACTER*1 NULL
          LOGICAL PROPER
!
          KOL = 0
!
          NULL = CHAR(0)
! GET THE ENV VARIABLE, 'INPNM', SPACES IN FILE NAMES
! ARE NOT ALLOWED!, ALSO IT IS NOT GAURENTEED TO
! RETURN PREDICTABLE VALUES, IF 'INPNM' ENV VARIABLE
! RESULTS IN A VERY LARGE STRING (>255 CHARS) OR IF
! WE ARE NOT RUNNING ON A UNIX LIKE OS!!
!
          CALL GETENV('INPNM', GTPREFX)
          PROPER = .FALSE.
          DO KOL=1,255
             IF(GTPREFX(KOL:KOL).EQ.' '  .OR. &
              &   GTPREFX(KOL:KOL).EQ.NULL) THEN
                PROPER = .TRUE.
                EXIT
             ENDIF
          ENDDO
          IF (.NOT. PROPER) KOL = 256

          IF (KOL .lt. 255) THEN
             GTPREFX(KOL:KOL) = '-'
          ENDIF

          SIZ = KOL

          RETURN
END
!

! UTILITY FUNCTION TO RETURN THE MAIN OUTPUT FILENAME
FUNCTION GTMAINOUT(SIZ)
!
          IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!
          CHARACTER*255 GTMAINOUT
          INTEGER SIZ
          CHARACTER*1 NULL
          LOGICAL PROPER
!
          KOL = 0
!
          NULL = CHAR(0)
! GET THE ENV VARIABLE, 'INPNM', SPACES IN FILE NAMES
! ARE NOT ALLOWED!, ALSO IT IS NOT GAURENTEED TO
! RETURN PREDICTABLE VALUES, IF 'INPNM' ENV VARIABLE
! RESULTS IN A VERY LARGE STRING (>255 CHARS) OR IF
! WE ARE NOT RUNNING ON A UNIX LIKE OS!!
!
          CALL GETENV('OUTNM', GTMAINOUT)
          PROPER = .FALSE.
          DO KOL=1,255
             IF(GTMAINOUT(KOL:KOL).EQ.' '  .OR. &
              &   GTMAINOUT(KOL:KOL).EQ.NULL) THEN
                PROPER = .TRUE.
                EXIT
             ENDIF
          ENDDO
          IF (.NOT. PROPER) KOL = 256

!          IF (KOL .lt. 255) THEN
!             GTMAINOUT(KOL:KOL) = '-'
!          ENDIF

          SIZ = KOL

          RETURN
END
!

