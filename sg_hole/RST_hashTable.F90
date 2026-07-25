
!!$ Author:
!!$   Yuanqing Wu, KAUST, Saudi Arabia
!!$
!!$ History:
!!$   2017-2-9 by Yuanqing Wu
!!$
!!$ Support:
!!$   wuyuanq@gmail.com

module RST_hashTable

    use RST_globalSgData
    implicit none

contains

    function hash(pID) result(hIndex)

        type(pointID), intent(in) :: pID
        integer :: hIndex
        integer :: sum, i

        sum = 0
        if(DIM == 2) then
            do i = 1, DIM
                sum = sum + 2**pID%le(i)*pID%li(i)*prime(i)*prime(43*(DIM-1)*10-i)
            end do
        else
            do i = 1, DIM
                sum = sum + 2**pID%le(i)*pID%li(i)*prime(i)*prime(43*(DIM-2)*10-i)
            end do
        end if
        hIndex = mod(sum, HSIZE) + 1

    end function hash

    subroutine sgInterpo(P, z, xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, &!
        sLEst, local_vEst, local_CfEst)

        real(kind=8), intent(in) :: P
        real(kind=8), dimension(:), pointer, intent(in) :: z
        real(kind=8), dimension(:), pointer, intent(in out) :: xEst
        real(kind=8), dimension(:), pointer, intent(in out) :: yEst
        real(kind=8), intent(out) :: xiLEst
        real(kind=8), intent(out) :: xiGEst
        real(kind=8), intent(out) :: rhoLEst
        real(kind=8), intent(out) :: rhoGEst
        real(kind=8), intent(out) :: sLEst
        real(kind=8), dimension(:), pointer, intent(in out) :: local_vEst
        real(kind=8), intent(out) :: local_CfEst
        real(kind=8) :: stdP
        type(point), pointer :: pa
        logical :: isInRange
        integer :: hIndex
        real(kind=8) :: ratio
        real(kind=8), dimension(1:SURPLUSSIZE) :: estArray
        integer :: i

        stdP = (P-PMIN)/(PMAX-PMIN)
        estArray(1:SURPLUSSIZE) = 0.D0
        hIndex = 1
        pa => hashTable(1)
        do while(hIndex <= HSIZE)
            if(pa%myID%le(1) /= 0) then
                isInRange = .true.
                if((stdP<pa%lend(1)).or.(stdP>pa%rend(1))) then
                    isInRange = .false.
                else
                    ratio = ((pa%rend(1)-pa%lend(1))/2.D0 - abs(stdP-pa%coordinate(1)))/((pa%rend(1)-pa%lend(1))/2.D0)
                end if
                if(isInRange) then
                    do i = 2, DIM
                        if((z(i-1)<pa%lend(i)).or.(z(i-1)>pa%rend(i))) then
                            isInRange = .false.
                            exit
                        else
                            ratio = ratio * ((pa%rend(i)-pa%lend(i))/2.D0 - abs(z(i-1)-pa%coordinate(i)))/ &!
                                ((pa%rend(i)-pa%lend(i))/2.D0)
                        end if
                    end do
                end if
                if(isInRange) then
                    do i = 1, SURPLUSSIZE
                        if(pa%stamp(i)) then
                            estArray(i) = estArray(i) + pa%surplus(i)*ratio
                        end if
                    end do
                end if
            end if
            if(associated(pa%next)) then
                pa => pa%next
            else
                hIndex = hIndex + 1
                if(hIndex <= HSIZE) then
                    pa => hashTable(hIndex)
                end if
            end if
        end do

        xEst(1:DIM) = estArray(1:DIM)
        yEst(1:DIM) = estArray(DIM+1:2*DIM)
        xiLEst = estArray(2*DIM+1)
        xiGEst = estArray(2*DIM+2)
        rhoLEst = estArray(2*DIM+3)
        rhoGEst = estArray(2*DIM+4)
        sLEst = estArray(2*DIM+5)
        local_vEst(1:DIM) = estArray(2*DIM+6:3*DIM+5)
        local_CfEst = estArray(3*DIM+6)

    end subroutine sgInterpo

    subroutine retrieveHashTable()

        type(pointID) :: pID
        type(point), pointer :: pb, pc
        character(len=60) :: fhashtxt
        integer :: hIndex, ierr, i

        do i = 1, HSIZE
            hashTable(i)%myID%le(1:DIM) = 0
            hashTable(i)%myID%li(1:DIM) = 0
            hashTable(i)%next => null()
        end do

        fhashtxt = '/Users/yuanqingwu/research/SparseGrids/sg/hashTable.txt'
        open(unit=10, file=trim(adjustl(fhashtxt)), status='old', action='read', iostat=ierr)
        if(ierr /= 0) then
            print *, 'open file error. ', ierr
            stop
        end if

        do
            read(10, fmt="(i4)", iostat=ierr) pID%le(:)
            if(ierr == -1) then
                exit
            end if
            read(10, fmt="(i4)") pID%li(:)

            hIndex = hash(pID)
            pc => hashTable(hIndex)
            if(pc%myID%le(1) /= 0) then
                pb => hashTable(hIndex)
                pc => hashTable(hIndex)%next
                do while(associated(pc))
                    pb => pc
                    pc => pc%next
                end do
                allocate(pc)  ! will not deallocate
                pb%next => pc
            end if

            do i = 1, DIM
                pc%myID%le(i) = pID%le(i)
                pc%myID%li(i) = pID%li(i)
            end do
            read(10, fmt="(es15.8)") pc%value(:)
            read(10, fmt="(es15.8)") pc%surplus(:)
            read(10, fmt="(l2)") pc%stamp(:)
            read(10, fmt="(es15.8)") pc%lend(:)
            read(10, fmt="(es15.8)") pc%rend(:)
            read(10, fmt="(es15.8)") pc%coordinate(:)
            pc%next => null()
        end do

        close(10)

    end subroutine retrieveHashTable

end module RST_hashTable

