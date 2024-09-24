!
! dmpick.f90 
!
! This module defines the routines for arranging the DM and making a 
! linear array of lower triangular elements of the DM which can be 
! directly fed to GAMESS. 
! The new version is written using some OO features of F90/95.
!
! @author V.Ganesh
!

module m_theData
use m_datatypes

implicit none
save

   !! the DM datastructure
   type t_value
      real(kind=DP), pointer :: value
   end type t_value

   type t_contractions
      type(t_value), dimension(:,:), pointer :: values
   end type t_contractions

   type(t_contractions), dimension(:,:), pointer :: dm4d
   real(kind=DP), dimension(:,:), allocatable :: dm

   integer(kind=SP) :: noOfValues, noOfAtoms, dmSize

   integer(kind=SP) :: u_dm

   !! the max contraction map
   integer(kind=SP), allocatable, dimension(:) :: contractionMap
end module m_theData

!! read in the DM
subroutine readInDM(dmFile)
  use m_datatypes
  use m_theData

  implicit none

  character(*) :: dmFile
  
  integer(kind=SP) :: iost, atomNo, b, i, j, k, m, n, tempI, maxContractions
  real(kind=DP)    :: value
  
  ! first compute the dimension
  noOfValues = 0
  noOfAtoms  = 0
  dmSize     = 0
  open(u_dm, file=dmFile)

  iost = 0
  do while(iost .eq. 0)
     read(u_dm, *, iostat=iost) atomNo, b, i, j

     if (iost .eq. 0)  then
        noOfValues = noOfValues + 1

        if ((atomNo .eq. b) .and. (i .eq. j)) dmSize = dmSize + 1
     endif

     if (atomNo .gt. noOfAtoms) noOfAtoms = atomNo
  enddo
  
  close(u_dm)

  write(*,*) "No. of Atoms  : ", noOfAtoms
  write(*,*) "No. of Values : ", noOfValues
  write(*,*) "DM Size       : ", dmSize

  ! allocate memory 
  allocate(dm4d(noOfAtoms, noOfAtoms))  
  do m = 1, noOfAtoms
     do n = 1, noOfAtoms
        dm4d(m, n)%values => NULL()
     enddo
  enddo 

  ! then read into appropriate places
  open(u_dm, file=dmFile)
  
  do k = 1, noOfValues
     read(u_dm, *) atomNo, b, i, j, value

     if (atomNo .lt. b) then
        ! swap atom indices
        tempI  = atomNo
        atomNo = b
        b      = tempI
        ! swap contractions indices
        tempI  = i
        i      = j
        j      = tempI
     endif

     if (.not. associated(dm4d(atomNo, b)%values)) then
        maxContractions = max(contractionMap(atomNo), contractionMap(b))
        allocate(dm4d(atomNo, b)%values(maxContractions, maxContractions))
        
        do m = 1, maxContractions
           do n = 1, maxContractions
              dm4d(atomNo, b)%values(m, n)%value => NULL()
           enddo
        enddo

        allocate(dm4d(atomNo, b)%values(i, j)%value)
        dm4d(atomNo, b)%values(i, j)%value = value
     else if (.not. associated(dm4d(atomNo, b)%values(i, j)%value)) then
        allocate(dm4d(atomNo, b)%values(i, j)%value)
        dm4d(atomNo, b)%values(i, j)%value = value  
     endif
  enddo
 
  close(u_dm)

  write(*,*) 'Matrix reading over'
end subroutine readInDM


!! read in the DM (lego assembler, based on cardinality expression)
subroutine readInDMLego(dmFile)
  use m_datatypes
  use m_theData

  implicit none

  character(*) :: dmFile
  
  integer(kind=SP) :: iost, atomNo, b, i, j, k, m, n, tempI, maxContractions
  real(kind=DP)    :: value
  
  ! first compute the dimension
  noOfValues = 0
  noOfAtoms  = 0
  dmSize     = 0
  open(u_dm, file=dmFile)

  iost = 0
  do while(iost .eq. 0)
     read(u_dm, *, iostat=iost) atomNo, b, i, j

     if (iost .eq. 0)  then
        noOfValues = noOfValues + 1

        if ((atomNo .eq. b) .and. (i .eq. j)) dmSize = dmSize + 1
     endif

     if (atomNo .gt. noOfAtoms) noOfAtoms = atomNo
  enddo
  
  close(u_dm)

  ! allocate memory 
  allocate(dm4d(noOfAtoms, noOfAtoms))  
  dmSize = 0
  do m = 1, noOfAtoms
     do n = 1, noOfAtoms
        dm4d(m, n)%values => NULL()
     enddo
     dmSize = dmSize + contractionMap(m)
  enddo 

  write(*,*) "No. of Atoms  : ", noOfAtoms
  write(*,*) "No. of Values : ", noOfValues
  write(*,*) "DM Size       : ", dmSize

  ! then read into appropriate places
  open(u_dm, file=dmFile)
  
  do k = 1, noOfValues
     read(u_dm, *) atomNo, b, i, j, value

     if (atomNo .lt. b) then
        ! swap atom indices
        tempI  = atomNo
        atomNo = b
        b      = tempI
        ! swap contractions indices
        tempI  = i
        i      = j
        j      = tempI
     endif

     if (.not. associated(dm4d(atomNo, b)%values)) then
        maxContractions = max(contractionMap(atomNo), contractionMap(b))
        allocate(dm4d(atomNo, b)%values(maxContractions, maxContractions))
        
        do m = 1, maxContractions
           do n = 1, maxContractions
              dm4d(atomNo, b)%values(m, n)%value => NULL()
           enddo
        enddo

        allocate(dm4d(atomNo, b)%values(i, j)%value)
        dm4d(atomNo, b)%values(i, j)%value = value
     else if (.not. associated(dm4d(atomNo, b)%values(i, j)%value)) then
        allocate(dm4d(atomNo, b)%values(i, j)%value)
        dm4d(atomNo, b)%values(i, j)%value = value    
     else
        dm4d(atomNo, b)%values(i, j)%value = dm4d(atomNo, b)%values(i, j)%value + value  
     endif
  enddo
 
  close(u_dm)

  write(*,*) 'Matrix reading over (using Lego, cardinality assembly)'
end subroutine readInDMLego

!! write the DM
subroutine writeDM(dmFile)
  use m_datatypes
  use m_theData

  implicit none

  character(*) :: dmFile
  integer(kind=SP) :: a, b, i, j, maxContractions
  integer(kind=SP) :: imap, jmap, iContraction, jContraction

  allocate(dm(dmSize, dmSize))
  dm = 0.0

  iContraction = 0
  jContraction = 0

  do a=1, noOfAtoms
     do b=1, a
        if (associated(dm4d(a, b)%values)) then
           maxContractions = max(contractionMap(a), contractionMap(b))
           do i=1, maxContractions
              do j=1, maxContractions

                 if (associated(dm4d(a, b)%values(i, j)%value)) then
                    imap = iContraction + i
                    jmap = jContraction + j

                    if (jmap .le. imap) then
                       dm(imap, jmap) = dm4d(a, b)%values(i, j)%value
                       dm(jmap, imap) = dm(imap, jmap)
                    endif
                 endif
              enddo
           enddo
        endif

        jContraction = jContraction + contractionMap(b)
     enddo

     iContraction = iContraction + contractionMap(a)
     jContraction = 0
  enddo

  write(*,*) 'Matrix linearising over'

  ! write the stuff
  open(u_dm, file=dmFile)
  write(u_dm, *) dmSize
  do i = 1, dmSize
     do j = 1, i
        write(u_dm, *) dm(i , j)
     enddo
  enddo
  close(u_dm)

  write(*,*) 'Matrix writing over'
end subroutine writeDM

!! to pick the DM, using the cardinality 
subroutine DMLEGO(icntmap, n)
  use m_datatypes
  use m_theData

  use m_utilityFunctions, only: getUnit

  implicit none

  character(len=255) prfx, gtprefx
  integer(kind=SP) :: iprfxsz

  integer(kind=SP) :: a, b, i, j, n
  integer(kind=SP), dimension(*) :: icntmap

  ! set up contraction map
  allocate(contractionMap(n))
  do i=1, n
      contractionMap(i) = icntmap(i)
  enddo

  write(*,*) 'Contraction map: ', contractionMap

  u_dm = getUnit()

  ! get the prefix string
  prfx = GTPREFX(iprfxsz)       

  ! read and write DM, overlap and fock matrix
  call readInDMLego(prfx(1:iprfxsz) // "pij")
  call writeDM(prfx(1:iprfxsz) // "p")
  deallocate(dm)
  call dmClean()

  call readInDMLego(prfx(1:iprfxsz) // "sij")
  call writeDM(prfx(1:iprfxsz) // "s")
  deallocate(dm)
  call dmClean()

  call readInDMLego(prfx(1:iprfxsz) // "fij")
  call writeDM(prfx(1:iprfxsz) // "f")
  deallocate(dm)
  call dmClean()

  deallocate(contractionMap)  
end subroutine DMLEGO

!! to pick the DM
subroutine DMPICK(icntmap, n)
  use m_datatypes
  use m_theData

  use m_utilityFunctions, only: getUnit

  implicit none

  character(len=255) prfx, gtprefx
  integer(kind=SP) :: iprfxsz

  integer(kind=SP) :: a, b, i, j, n
  integer(kind=SP), dimension(*) :: icntmap

  ! set up contraction map
  allocate(contractionMap(n))
  do i=1, n
      contractionMap(i) = icntmap(i)
  enddo

  write(*,*) 'Contraction map: ', contractionMap

  u_dm = getUnit()

  ! get the prefix string
  prfx = GTPREFX(iprfxsz)       

  ! read and write DM, overlap and fock matrix
  call readInDM(prfx(1:iprfxsz) // "pij")
  call writeDM(prfx(1:iprfxsz) // "p")
  deallocate(dm)
  call dmClean()

  call readInDM(prfx(1:iprfxsz) // "sij")
  call writeDM(prfx(1:iprfxsz) // "s")
  deallocate(dm)
  call dmClean()

  call readInDM(prfx(1:iprfxsz) // "fij")
  call writeDM(prfx(1:iprfxsz) // "f")
  deallocate(dm)
  call dmClean()

  deallocate(contractionMap)
end subroutine DMPICK

!! dmClean ... clean all the garbage
subroutine dmClean()
  use m_datatypes
  use m_theData

  implicit none 

  integer(kind=SP)  :: a, b, i, j, maxContractions

  do a=1, noOfAtoms
     do b=1, noOfAtoms
        if (associated(dm4d(a, b)%values)) then
           maxContractions = max(contractionMap(a), contractionMap(b))
           do i=1, maxContractions
              do j=1, maxContractions
                 if (associated(dm4d(a, b)%values(i, j)%value)) then
                    deallocate(dm4d(a, b)%values(i, j)%value)
                 endif
              enddo
           enddo

           deallocate(dm4d(a, b)%values)
        endif
      enddo
  enddo

  deallocate(dm4d)
end subroutine dmClean
