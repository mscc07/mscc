      subroutine diagonalization(hamil, n, n1, n2, ehamil,vec)

        integer n
        integer n1
        integer n2
        integer i

        real(8), dimension(n,n) :: hamil
        real(8), dimension(n2*n) :: vec
        real(8), dimension(n1):: WORK
        real(8) ,dimension(n) :: ehamil !, vec

        N=n
        LDA=n
        LWORK=n1
        CALL DSYEV('V','L',N,hamil,LDA,ehamil,WORK,LWORK,INFO)
        do j=1,n2
        do i=1,n
                vec(i+((j-1)*n))=hamil(i,j)
                !write(*,*)"hamil(i,j)",hamil(i,j)

        enddo
        enddo
        !write(*,*)"vec",vec
        !do i=1,n
        !        write(*,*)hamil(i,1)
        !enddo
        LWKOPT=WORK(1)
      end subroutine

