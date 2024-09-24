!!
!! energy.f90
!!
!! Does a quick estimation of energy.
!!
!! @author: V.Ganesh
!!
subroutine ESTEN(ene, e1, e2, enuk)
  use m_datatypes 
  use m_utilityFunctions, only: getUnit

  implicit none

  real(kind=DP) :: e1, e2, ene, enuk
  integer(kind=SP) :: n, i, k, n1
  real(kind=DP), dimension(:), allocatable :: a, b

  character(len=255) prfx, gtprefx
  integer(kind=SP) :: iprfxsz, u_fil

  ! get the prefix string
  prfx = GTPREFX(iprfxsz)

  u_fil = getUnit()      

  open(unit=u_fil, file=prfx(1:iprfxsz) // "p")
  read(u_fil, *) n
  n1 = n
  n = n*(n+1.0)/2.0
  allocate(a(n))
  read(u_fil, *) (a(i), i=1, n)
  close(u_fil)

  open(unit=u_fil, file=prfx(1:iprfxsz) // "h")
  read(u_fil, *) n
  n = n*(n+1.0)/2.0
  allocate(b(n))
  read(u_fil, *) (b(i), i=1, n)
  close(u_fil)

  call findTrace(a, b, n1, e1)
  deallocate(b)

  open(unit=u_fil, file=prfx(1:iprfxsz) // "f")
  read(u_fil, *) n
  n = n*(n+1.0)/2.0
  allocate(b(n))
  read(u_fil, *) (b(i), i=1, n)
  close(u_fil)

  call findTrace(a, b, n1, e2)
  deallocate(a, b)

  write(*,*) e1, e2

  ene = ((e1 + e2) / 2.0) + enuk
end subroutine ESTEN

!! find trace
subroutine findTrace(a, b, n1, trace)
  use m_datatypes 
  implicit none

  real(kind=DP), dimension(*) :: a, b
  real(kind=DP) :: trace 
  integer(kind=SP) :: n1, n, k, i

  trace = 0.0

  n = n1*(n1+1.0)/2.0  

  do i = 1, n
     trace = trace + (a(i) * b(i))
  enddo

  trace = trace + trace

  k = 0
  do i = 1, n1
     k = k + i
     trace = trace - (a(k) * b(k))
  enddo
end subroutine findTrace
