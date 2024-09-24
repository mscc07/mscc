!!
!! datatypes.f90
!!
!! This file defines various symbolic constants for defining dataypes
!! of various types.
!!
!! @author  V.Ganesh
!! @email   tcg@chem.unipune.ernet.in
!! @date    4th June 2003
!!

module m_datatypes
    implicit none
  
    integer(kind=4) :: SP, DP, CH, BI

    ! byte type
    parameter (BI = 1)

    ! character type
    parameter (CH = 2)

    ! single precision types
    parameter (SP = 8)

    ! double precision types
    parameter (DP = 8)

end module m_datatypes
! end of module m_datatypes
