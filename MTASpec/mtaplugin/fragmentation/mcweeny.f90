!! 
!! mcweeny-new.f90
!!
!! McWeeny refinement: P' = 1.5PSP - 0.5PSPSP
!!
!! please forgive me about any design issues
!! ... this is my first usable fortran program! ;)
!!
!! @author V.Ganesh
!!
subroutine McWeeny(pmat, smat, outmat)
 use m_datatypes

 implicit none
 
 integer(kind=SP) :: i, j, n, iteration, MAX_ITER, u_p, u_s
 real(kind=DP)    :: t1, t2, maxnorm, curval, traceps, CUTOFF
 real(kind=DP), allocatable, dimension(:,:) :: p, s
 character(*) :: pmat, smat, outmat
 
 parameter(MAX_ITER = 10)
 parameter(CUTOFF   = 1.0d-8)

 character(len=255) :: prfx, gtprefx
 integer(kind=SP)   :: iprfxsz

! set the unit numbers
 u_p = 88
 u_s = 89

 write(*,*) '====== McWeeny Refinement ===='
 write(*,*) 'P Matrix: ', pmat
 write(*,*) 'S Matrix: ', smat
 write(*,*) 'Refined Matrix: ', outmat
 
! get the prefix string
 prfx = GTPREFX(iprfxsz)      

 call cpu_time( t1 )

 open(u_p,file=pmat )
 open(u_s,file=smat )
! read the diemnsion
 read(u_p, *) n
 read(u_s, *) n
 allocate( p(n,n),s(n,n) )

 if (allocated(p) .and. allocated(s)) then
    write(*,*) 'Allocated memory for P and S : ', n
 else
    write(*,*) 'Memory allocation failure for P and S .. Aborting'
    call ABRT()
 endif 

 do i  = 1,n
  do j  = 1,i
   read(u_p,*)p(i,j)
   read(u_s,*)s(i,j)
   p(j,i)=p(i,j)
   s(j,i)=s(i,j)
  enddo
 enddo
 close(u_p)
 close(u_s)

! write whole 'p' and 's' to disk 
 open(u_p, file=prfx(1:iprfxsz) // 'p.mat')
 write(u_p,*) ((p(i,j),i=1,n),j=1,n)
 close(u_p)
 open(u_p, file=prfx(1:iprfxsz) // 's.mat')
 write(u_p,*) ((s(i,j),i=1,n),j=1,n)
 close(u_p)

 write(*,*) 'Starting McWeeny refinement ...'
 write(*,*) ''

 do iteration = 1, MAX_ITER
 
! read the matrices from disk if not the 
! first iteration
 if (iteration .ne. 1) then
    open(u_p, file=prfx(1:iprfxsz) // 'p.mat')
    read(u_p,*) ((p(i,j),i=1,n),j=1,n)
    close(u_p)
    open(u_p, file=prfx(1:iprfxsz) // 's.mat')
    read(u_p,*) ((s(i,j),i=1,n),j=1,n)
    close(u_p)
 endif

! S = PS
 s = matmul(p, s)
 open(u_p, file=prfx(1:iprfxsz) // 'ps.mat')
 write(u_p,*) ((s(i,j),i=1,n),j=1,n)
 close(u_p)

! S = PSP
 s = matmul(s, p)

! PSPSP
 open(u_p, file=prfx(1:iprfxsz) // 'ps.mat')
 read(u_p,*) ((p(i,j),i=1,n),j=1,n)
 close(u_p)

 p = matmul(p, s)

! P' = 1.5 * PSP (s) - 0.5 * PSPSP (p)
 p = (1.5 * s) - (0.5 * p)
 open(u_p, file=prfx(1:iprfxsz) // 'p.mat')
 write(u_p,*) ((p(i,j),i=1,n),j=1,n)
 close(u_p)

 call cpu_time( t2 )

! maxnorm : ||PSP-2P||
 maxnorm = maxval(abs(s - (2.0d0 * p)))

! read PS into S
 open(u_p, file=prfx(1:iprfxsz) // 'ps.mat')
 read(u_p,*) ((s(i,j),i=1,n),j=1,n)
 close(u_p)

! find trace(PS)
 traceps = 0.0d0
 do i = 1, n  
    traceps = traceps + s(i, i)
 enddo

 write(*,'(I5, ". Trace : ",d10.5, " Max norm : ",d10.5, &
&         " Time : ",f0.0)') &
&         iteration, traceps, maxnorm, (t2-t1)

 if (maxnorm .le. CUTOFF) then
    exit    
 endif
! end of mcweeny iteration
 enddo

 write(*,'(" Mcweeny converged in ",f0.0," seconds")') t2-t1
 write(*,'(5x,"writing refined DM into ",a20)') outmat

 open(u_p, file=prfx(1:iprfxsz) // 'p.mat')
 read(u_p,*) ((p(i,j),i=1,n),j=1,n)
 close(u_p)

 open(u_p,file=outmat)
 write(u_p,*) n
 do i = 1,n
 do j = 1,i
  write(u_p,*)p(i,j)
 enddo
 enddo
 close(u_p)

 write(*,*) '...done with writing!'
 
 ! clean up
 if (allocated(p)) deallocate( p )
 if (allocated(s)) deallocate( s )
end
! end of subroutine mcweeny
