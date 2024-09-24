!!
!! cardinal.f90
!!
!! A Set cardinality based, linear-scaling energy and gradient 
!! (and hessian?) evaluation scheme.
!!
!! @author V.Ganesh
!! @version 3.0
!!
module m_cardinal
use m_datatypes
use m_fragment

implicit none

integer(kind=SP) :: MINUS, PLUS, NO_SIGN, ONE_BIT, ZERO_BIT, BITS_PER_BYTE
logical(kind=SP) :: mergedIntoPreviousTrem
integer(kind=SP) :: numberOfWords, bitPower

parameter (MINUS    = -1)
parameter (PLUS     = 1 )
parameter (NO_SIGN  = 0 )
parameter (ONE_BIT  = 1 )
parameter (ZERO_BIT = 0 )
parameter (BITS_PER_BYTE = 8)

!!
!! the fragment list type
type t_fragmentList
   type(t_fragment),pointer :: frag  ! the fragment
   integer(kind=SP) :: signOfEnergy  ! sign of energy
   integer(kind=SP),pointer,dimension(:) :: atomIndexBits    ! atom indices translated to bits

   type(t_fragmentList), pointer :: next ! pointer to the next fragment 
endtype t_fragmentList

!! the head pointer, to the fragment list
type(t_fragmentList), pointer :: head, tail

!!
!! the fragment list as bit representation
type t_fragBits
   type(t_fragment) :: originalFragment ! the original fragment
   integer(kind=SP),pointer,dimension(:) :: atomIndexBits    ! atom indices translated to bits
endtype t_fragBits

!! the fragment list as bits (really!)
type(t_fragBits), allocatable, dimension(:) :: fragmentAsBits

!! list of true incices
integer(kind=SP), allocatable, dimension(:) :: trueIndices, bitMasks

contains

!!
!! setup a table of true indices and the bit values that need to be 
!! ORd or ANDed
subroutine setupTrueIncices()
use m_datatypes
use m_input

implicit none
    integer(kind=SP) :: i, idx

    allocate(trueIndices(numberOfAtoms))
    allocate(bitMasks(numberOfAtoms))


    do i = 1, numberOfAtoms
       idx = i-1
       trueIndices(i) = ishft(idx, -bitPower)+1
       bitMasks(i)    = ishftc(ONE_BIT, idx)       
    enddo

end subroutine setupTrueIncices

!!
!! set a bit in the array of bits representing atoms
subroutine setBit(atomBits, id)
use m_datatypes
use m_input
use m_fragment

implicit none
  integer(kind=SP),dimension(:) :: atomBits    
  integer(kind=SP) :: id
  
  atomBits(trueIndices(id)) = ior(atomBits(trueIndices(id)), bitMasks(id))
end subroutine setBit


!!
!! set a bit in the array of bits representing atoms
integer(kind=SP) function getBit(atomBits, id)
use m_datatypes
use m_input
use m_fragment

implicit none
  integer(kind=SP),dimension(:) :: atomBits    
  integer(kind=SP) :: id

  getBit = iand(atomBits(trueIndices(id)), bitMasks(id))
end function getBit

!!
!! get what 2**x is this number
integer(kind=SP) function getPower(n)
use m_datatypes
implicit none

   integer(kind=SP) :: n, m, i

   m = (n*BITS_PER_BYTE)
   do i = 0, m
      if (iand(m, ishft(ONE_BIT, i)) .ne. ZERO_BIT) then
         getPower = i
         return
      endif
   enddo

end function getPower

!!
!! convert main fragments to bit list
subroutine convertMainFragsToBits()
use m_datatypes
use m_fragment
use m_input
use m_units

implicit none

  integer(kind=SP) :: i, j, bitValue

  numberOfWords = numberOfAtoms / (SP*BITS_PER_BYTE)
  if (mod(numberOfAtoms, SP*BITS_PER_BYTE) .ne. 0) then
     numberOfWords = numberOfWords + 1
  endif

!ANUJA  write(u_mainout,*) 'Number of Words:', numberOfWords
!ANUJA  write(u_mainout,*) 'Power is: ', bitPower

  allocate(fragmentAsBits(numberOfFragments))
 
  do i = 1, numberOfFragments
     fragmentAsBits(i)%atomIndexBits    => NULL()
     allocate(fragmentAsBits(i)%atomIndexBits(numberOfWords))
     fragmentAsBits(i)%atomIndexBits    = ZERO_BIT
     fragmentAsBits(i)%originalFragment = fragments(i)

     ! fill in the bits to indicate a fragment
     do j = 1, fragments(i)%numberOfTotalAtoms
        call setBit(fragmentAsBits(i)%atomIndexBits, fragments(i)%atomIndices(j))
     enddo

     ! write(*,*) 'frag', i, ' code ', fragmentAsBits(i)%atomIndexBits
  enddo   


  ! verify that all atoms are present in the fragments
  do i = 1, numberOfWords
     bitValue = 0
     do j = 1, numberOfFragments
        bitValue = ior(bitValue, fragmentAsBits(j)%atomIndexBits(i))
     enddo

!ANUJA     write(u_mainout,*) 'Word ', i, ' is: ', bitValue
  enddo
end subroutine convertMainFragsToBits

!!
!! init the fragment list
subroutine initFragmentList() 
implicit none

  allocate(head)

  head%frag => NULL()
  head%signOfEnergy = NO_SIGN
  head%next => NULL()

  ! in the beginning the head and tail were same :)
  tail => head
  
  mergedIntoPreviousTrem = .false.
end subroutine initFragmentList

!!
!! check to see if we can merge this fragment into previous terms
!! if so, then does it and returns .T. else .F.
logical(kind=SP) function mergeIntoPreviousTerms(frag, signOfEnergy)
use m_datatypes
use m_fragment
use m_options

implicit none

type(t_fragment), pointer :: frag
type(t_fragmentList), pointer :: iterator, p
integer(kind=SP) :: signOfEnergy, i, j
logical(kind=SP) :: allAtomsPresent

  mergeIntoPreviousTerms = .false.

  iterator => head
  if (.not. associated(iterator)) return

  do j = 1, numberOfOverlapFragments
     ! check if number of atoms are same
     if (frag%numberOfTotalAtoms .eq. iterator%frag%numberOfTotalAtoms) then
        ! then check if the atoms are same!!
        allAtomsPresent = .true.
        do i = 1, frag%numberOfTotalAtoms
           if (count(mask=frag%atomIndices.eq.iterator%frag%atomIndices(i), dim=1) .le. 0) then
              allAtomsPresent = .false.
              exit
           endif
        enddo

        if (allAtomsPresent) then
           iterator%signOfEnergy = iterator%signOfEnergy + signOfEnergy
           mergeIntoPreviousTerms = .true.
           mergedIntoPreviousTrem = .true.

           !if (verbose) then
           !   write(*,'("MERGD INTO:",20I4)') j, &
           !  & iterator%frag%atomIndices(1:iterator%frag%numberOfTotalAtoms)
           !endif

           return
        endif
     endif

     iterator => iterator%next
     if (.not. associated(iterator)) exit
  enddo
end function mergeIntoPreviousTerms

!!
!! check to see if we can merge this fragment into previous terms
!! if so, then does it and returns .T. else .F.
!! Improved version with bit-wise operations
logical(kind=SP) function mergeIntoPreviousTerms2(frag, signOfEnergy, atomBits)
use m_datatypes
use m_fragment
use m_options

implicit none

type(t_fragment), pointer :: frag
type(t_fragmentList), pointer :: iterator, p
integer(kind=SP) :: signOfEnergy, i, j
logical(kind=SP) :: allAtomsPresent
integer(kind=SP), dimension(:) :: atomBits

  mergeIntoPreviousTerms2 = .false.

  iterator => head
  if (.not. associated(iterator)) return

  do j = 1, numberOfOverlapFragments
     ! check if number of atoms are same
     if (frag%numberOfTotalAtoms .eq. iterator%frag%numberOfTotalAtoms) then
        ! then check if the atoms are same!!
        allAtomsPresent = .true.
        do i = 1, numberOfWords
           if (iand(atomBits(i), iterator%atomIndexBits(i)) .ne. atomBits(i)) then
              allAtomsPresent = .false.
              exit
           endif
        enddo

        if (allAtomsPresent) then
           iterator%signOfEnergy = iterator%signOfEnergy + signOfEnergy
           mergeIntoPreviousTerms2 = .true.
           mergedIntoPreviousTrem  = .true.

           !if (verbose) then
           !   write(*,'("MERGD INTO:",20I4)') j, &
           !  & iterator%frag%atomIndices(1:iterator%frag%numberOfTotalAtoms)
           !endif

           return
        endif
     endif

     iterator => iterator%next
     if (.not. associated(iterator)) exit
  enddo
end function mergeIntoPreviousTerms2

!!
!! add a fragment to the fragment list
subroutine addFragmentToList(frag, signOfEnergy)
use m_datatypes
use m_fragment

implicit none

type(t_fragment), pointer :: frag
type(t_fragmentList), pointer :: newItem, iterator
integer(kind=SP) :: signOfEnergy, j
integer(kind=SP), dimension(:), allocatable :: atomBits

  mergedIntoPreviousTrem = .false.

  if (.not. associated(head%frag)) then
     head%frag => frag
     head%signOfEnergy = signOfEnergy
     head%next => NULL()

     ! convert to bit rep
     allocate(head%atomIndexBits(numberOfWords))
     head%atomIndexBits  = ZERO_BIT

     ! fill in the bits to indicate a fragment
     do j = 1, head%frag%numberOfTotalAtoms
        call setBit(head%atomIndexBits, head%frag%atomIndices(j))
     enddo
  else 
     allocate(atomBits(numberOfWords))
     atomBits  = ZERO_BIT

     ! fill in the bits to indicate a fragment
     do j = 1, frag%numberOfTotalAtoms
        call setBit(atomBits, frag%atomIndices(j))
     enddo

     ! try to see if this fragment is already present?
     if (mergeIntoPreviousTerms2(frag, signOfEnergy, atomBits)) then
        deallocate(frag)  
        deallocate(atomBits)
        return
     endif

     ! get the tail
     iterator => tail

     ! attach the new item there
     allocate(newItem)
     newItem%frag => frag
     newItem%signOfEnergy = signOfEnergy
     newItem%next => NULL()

     ! convert to bit rep
     allocate(newItem%atomIndexBits(numberOfWords))
     newItem%atomIndexBits  = atomBits

     iterator%next => newItem

     ! tail got an extension, the new one :)
     tail => newItem

     deallocate(atomBits)
  endif
end subroutine addFragmentToList

!!
!! get the sign of the term having 'n' items
integer(kind=SP) function getSignOfTerm(n)
use m_datatypes

implicit none

integer(kind=SP) :: n

  if (mod(n, 2) .eq. 0) then
     getSignOfTerm = MINUS
  else
     getSignOfTerm = PLUS
  endif
end function getSignOfTerm

!!
!! check if an intersection is possible between fragments i and j
logical(kind=SP) function isIntersectionPossible(i, j)
use m_datatypes
use m_fragment

implicit none

integer(kind=SP) :: i, j
real(kind=DP) :: dij, x, y, z

  x = fragments(j)%center%x - fragments(i)%center%x
  y = fragments(j)%center%y - fragments(i)%center%y
  z = fragments(j)%center%z - fragments(i)%center%z

  dij = sqrt(x*x + y*y + z*z)

  isIntersectionPossible = ((fragments(i)%radius + fragments(j)%radius) > dij)
end function isIntersectionPossible

!!
!! function to compute and add intersection of fragments, if the intersection is
!! null set, then .false. is returned, else returns .true.
logical(kind=SP) function computeIntersections(combs, noOfItems, sign)
use m_datatypes
use m_fragment

implicit none

integer(kind=SP) :: noOfItems, sign, i, j, k
integer(kind=SP), dimension(:) :: combs
integer(kind=SP), dimension(:), allocatable :: atomIndicies
type(t_fragment), pointer :: newFrag

  if (noOfItems .eq. 0) then
     computeIntersections = .false.
     return
  endif

  computeIntersections = .true. 

  allocate(newFrag)
  allocate(atomIndicies(MAX_ATOMS_IN_FRAGMENT))

  newFrag%atomIndices        = fragments(combs(1))%atomIndices
  newFrag%numberOfTotalAtoms = fragments(combs(1))%numberOfTotalAtoms
  newFrag%numberOfDummyAtoms   = 0
  newFrag%numberOfUniqueAtoms  = 0
  newFrag%numberOfUniqueBonds  = 0 
  newFrag%numberOfContractions = 0
  newFrag%numberOfFreezedAtom  = 0

  do i = 2, noOfItems
     k = 0
     atomIndicies = 0
     do j = 1, fragments(combs(i))%numberOfTotalAtoms
        if (count(mask=newFrag%atomIndices(:newFrag%numberOfTotalAtoms) &
       &    .eq.fragments(combs(i))%atomIndices(j), dim=1) .gt. 0) then
           k = k + 1
           atomIndicies(k) = fragments(combs(i))%atomIndices(j)
        endif
     enddo

     if (k .eq. 0) then
        newFrag%atomIndices(j) = atomIndicies(j)
        newFrag%numberOfTotalAtoms = k
        computeIntersections = .false.
        exit
     else
        newFrag%atomIndices = atomIndicies
        newFrag%numberOfTotalAtoms = k
     endif
  enddo

  deallocate(atomIndicies)

  if (computeIntersections) then
     !write(*,*) 'HERE:', newFrag%numberOfTotalAtoms
     call addFragmentToList(newFrag, sign)
  else
     deallocate(newFrag)
  endif
end function computeIntersections

!!
!! function to compute and add intersection of fragments, if the intersection is
!! null set, then .false. is returned, else returns .true.
!! This is a new version of the function that uses bit operations instead of
!! integer comparisons
logical(kind=SP) function computeIntersections2(combs, noOfItems, sign)
use m_datatypes
use m_input
use m_fragment

implicit none

integer(kind=SP) :: noOfItems, sign, i, j, k
integer(kind=SP), dimension(:) :: combs
integer(kind=SP), dimension(:), allocatable :: atomBits, newFragAtomBits
integer(kind=SP), dimension(:), allocatable :: atomIndicies
type(t_fragment), pointer :: newFrag

  if (noOfItems .eq. 0) then
     computeIntersections2 = .false.
     return
  endif

  computeIntersections2 = .true. 

  allocate(newFrag)
  allocate(atomIndicies(MAX_ATOMS_IN_FRAGMENT))
  allocate(atomBits(numberOfWords), newFragAtomBits(numberOfWords))

  newFrag%atomIndices        = fragments(combs(1))%atomIndices
  newFrag%numberOfTotalAtoms = fragments(combs(1))%numberOfTotalAtoms
  newFragAtomBits            = fragmentAsBits(combs(1))%atomIndexBits
  newFrag%numberOfDummyAtoms   = 0
  newFrag%numberOfUniqueAtoms  = 0
  newFrag%numberOfUniqueBonds  = 0 
  newFrag%numberOfContractions = 0
  newFrag%numberOfFreezedAtom  = 0

  atomBits = 0
  do i = 2, noOfItems
     k = 0

     do j = 1, numberOfWords

        !
        ! Note: Warning!!! The following should never be true
        if (combs(i) .le. 0) then
           cycle
        endif

        atomBits(j) = iand(fragmentAsBits(combs(i))%atomIndexBits(j), newFragAtomBits(j))
        k = ior(k, atomBits(j))
     enddo

     if (k .eq. ZERO_BIT) then
        newFrag%atomIndices = atomIndicies
        newFrag%numberOfTotalAtoms = k
        computeIntersections2 = .false.
        exit
     else
        k = 0
        atomIndicies = 0
        do j = 1, numberOfAtoms
           if (getBit(atomBits, j) .ne. ZERO_BIT) then
              k = k + 1
              atomIndicies(k) = j
           endif
        enddo

        newFrag%atomIndices = atomIndicies
        newFrag%numberOfTotalAtoms = k
        newFragAtomBits = atomBits
     endif
  enddo

  deallocate(atomIndicies, atomBits)

  if (computeIntersections2) then
     !write(*,*) 'HERE:', newFrag%numberOfTotalAtoms
     call addFragmentToList(newFrag, sign)
  else
     deallocate(newFrag)
  endif
end function computeIntersections2

!!
!! make the correctly ordered cardinality expression for N fragments
subroutine makeCardinalityExpression()
use m_datatypes
use m_fragment
use m_units
use m_input
use m_strings
use m_options

implicit none

integer(kind=SP) :: i, j, k, l, m, n, pos, sign
integer(kind=SP), dimension(:), allocatable :: combs
type(t_fragmentList), pointer :: iterator, p

  call initFragmentList() 
  bitPower = getPower(8)
  call setupTrueIncices()
  call convertMainFragsToBits()

  iterator => NULL()
  p => NULL()

  numberOfOverlapFragments = 0

  allocate(combs(numberOfFragments))

  ! initilize all combinations
  combs = 0

  do i = 1, numberOfFragments
     do j = i+1, numberOfFragments 
        combs(1) = i; combs(2) = j;

        ! in the outer loop only two fragment terms are present
        sign = getSignOfTerm(2)

        ! compute the intersections
        if (.not. computeIntersections2(combs, 2, sign)) then 
           ! write(*,'(20I4)') NO_SIGN, (combs(n), n=1, 2) 
           cycle
        endif

        if (.not. mergedIntoPreviousTrem) then
           numberOfOverlapFragments = numberOfOverlapFragments + 1

           !if (verbose) then
           !   write(*,'(20I4)') sign, (combs(n), n=1, 2) 
           !endif
        !else
           !if (verbose) then
           !   write(*,'(A4,20I4)') 'MERG', sign, (combs(n), n=1, 2) 
           !endif
        endif
      
        if (numberOfFragments .le. 2) cycle

        l=1; pos=2; m=pos+1

        do while(.true.)   
           do k=combs(pos)+1, numberOfFragments
              combs(m) = k

              if (m .gt. 1) then
                 if (combs(m) .le. combs(m-1)) exit
              endif 

              ! in the outer loop only two fragment terms are present
              sign = getSignOfTerm(m)

              ! compute the intersections
              if (.not. computeIntersections2(combs, m, sign)) then 
                 ! write(*,'(A4,20I4)') 'SKIP', (combs(n), n=1, m) 
                 if (combs(m) .eq. numberOfFragments) exit
                 m = m - 1
                 continue
              else              
                 if (.not. mergedIntoPreviousTrem) then
                    numberOfOverlapFragments = numberOfOverlapFragments + 1
                    !if (verbose) then
                    !   write(*,'(20I4)') sign, (combs(n), n=1, m) 
                    !endif
                 !else
                    !if (verbose) then
                    !   write(*,'(A4,20I4)') 'MERG', sign, (combs(n), n=1, m) 
                    !endif
                 endif
              endif

              m = m + 1              
           enddo

           pos=numberOfFragments-l; m=pos

           if (combs(pos) .eq. numberOfFragments) then
              l = l+1
           else
              l = 1
              !l = numberOfFragments-m
           endif

           if (pos .eq. 2) then
              exit
           endif
        enddo
     enddo
  enddo

  deallocate(combs)

  open(u_car, file=prefix(1:iPrefixSize) // 'carsign')
  ! put the correct count
  iterator => head
  numberOfOverlapFragments = 0
  do while (associated(iterator))
     if (iterator%signOfEnergy .ne. 0) then        
        numberOfOverlapFragments =  numberOfOverlapFragments + 1
     endif
     iterator => iterator%next
  enddo
  write(u_car, *) numberOfOverlapFragments

  ! fail safe
  if (numberOfOverlapFragments .eq. 0) then
     deallocate(head)

     close(u_car)     
  else 
   ! add all the overlap fragments at the end of the suff
   iterator => head
   numberOfOverlapFragments = 0

   write(u_mainout,*) 'Final Cardinality expression:' 
   do while (associated(iterator))
     if (iterator%signOfEnergy .ne. 0) then      
        numberOfOverlapFragments =  numberOfOverlapFragments + 1
        if ((numberOfOverlapFragments+numberOfFragments) .gt. numberOfAtoms*2) then
           write(u_mainout,*) 'Too many combinations, can not handle this much volume!!!'
           !! avoid memory leak!!!
           do while (associated(iterator))
              deallocate(iterator%frag)
              iterator%frag => NULL()

              p => iterator
              iterator => iterator%next

              deallocate(p)
              p => NULL()
           enddo
           call ABRT()
        endif
        iterator%frag%signOfEnergy = iterator%signOfEnergy
        fragments(numberOfOverlapFragments+numberOfFragments) = iterator%frag

        write(u_car,*) iterator%signOfEnergy
        write(u_mainout,'(2i5)') numberOfOverlapFragments, iterator%signOfEnergy
     endif

     if (associated(iterator%atomIndexBits)) then
        deallocate(iterator%atomIndexBits)
     endif
     deallocate(iterator%frag)
     iterator%frag => NULL()

     p => iterator
     iterator => iterator%next

     deallocate(p)
     p => NULL()
   enddo  

   close(u_car)
  endif

  ! remove auxallary stuff
  if (allocated(fragmentAsBits)) then
   do i = 1, numberOfFragments-numberOfOverlapFragments
     if (associated(fragmentAsBits(i)%atomIndexBits)) then
        deallocate(fragmentAsBits(i)%atomIndexBits)
        fragmentAsBits(i)%atomIndexBits => NULL()
     endif
   enddo

   deallocate(fragmentAsBits)
  endif

  if (allocated(trueIndices)) then
     deallocate(trueIndices)
  endif
  if (allocated(bitMasks)) then
     deallocate(bitMasks)
  endif

  write(u_mainout,*) 'Cardinality expression setup complete. Number of elements: ', numberOfOverlapFragments
end subroutine makeCardinalityExpression

end module m_cardinal
!! end of module m_cardinal

