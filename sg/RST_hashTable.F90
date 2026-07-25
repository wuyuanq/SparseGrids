
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

    subroutine copyID(pID1, pID2)

        type(pointID), intent(inout) :: pID1
        type(pointID), intent(in) :: pID2
        integer :: i

        do i = 1, DIM
            pID1%le(i) = pID2%le(i)
            pID1%li(i) = pID2%li(i)
        end do

    end subroutine copyID

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

    function pointEqual(pID1, pID2) result(isEqual)

        type(pointID), intent(in) :: pID1, pID2
        logical :: isEqual
        integer :: i

        isEqual = .true.
        do i = 1, DIM
            if(pID1%le(i) /= pID2%le(i)) then
                isEqual = .false.
                exit
            end if
            if(pID1%li(i) /= pID2%li(i)) then
                isEqual = .false.
                exit
            end if
        end do

    end function pointEqual

    function inHash(pID) result(isInHash)

        type(pointID), intent(in) :: pID

        type(point), pointer :: pp
        integer :: hIndex
        logical :: isInHash

        hIndex = hash(pID)
        if(hashTable(hIndex)%myID%le(1) == 0) then
            isInHash = .false.
        else
            isInHash = .false.
            pp => hashTable(hIndex)
            do while(associated(pp))
                if(pointEqual(pp%myID, pID)) then
                    isInHash = .true.
                    exit
                end if
                pp => pp%next
            end do
        end if

    end function inHash

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
        type(point), pointer :: pp
        logical :: isInRange
        integer :: hIndex
        real(kind=8) :: ratio
        real(kind=8), dimension(1:SURPLUSSIZE) :: estArray
        integer :: i

        stdP = (P-PMIN)/(PMAX-PMIN)
        estArray(1:SURPLUSSIZE) = 0.D0
        hIndex = 1
        pp => hashTable(1)
        do while(hIndex <= HSIZE)
            if(pp%myID%le(1) /= 0) then
                if((stdP<pp%lend(1)).or.(stdP>pp%rend(1))) then
                    isInRange = .false.
                else
                    isInRange = .true.
                    ratio = ((pp%rend(1)-pp%lend(1))/2.D0 - abs(stdP-pp%coordinate(1)))/((pp%rend(1)-pp%lend(1))/2.D0)
                end if
                if(isInRange) then
                    do i = 2, DIM
                        if((z(i-1)<pp%lend(i)).or.(z(i-1)>pp%rend(i))) then
                            isInRange = .false.
                            exit
                        else
                            ratio = ratio * ((pp%rend(i)-pp%lend(i))/2.D0 - abs(z(i-1)-pp%coordinate(i)))/ &!
                                ((pp%rend(i)-pp%lend(i))/2.D0)
                        end if
                    end do
                end if
                if(isInRange) then
                    do i = 1, SURPLUSSIZE
                        estArray(i) = estArray(i) + pp%surplus(i)*ratio
                    end do
                end if
            end if
            if(associated(pp%next)) then
                pp => pp%next
            else
                hIndex = hIndex + 1
                if(hIndex <= HSIZE) then
                    pp => hashTable(hIndex)
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

    subroutine deleteHashTable()

        type(point), pointer :: pb, pc
        integer :: hIndex

        hIndex = 1
        do while(hIndex <= HSIZE)
            pb => hashTable(hIndex)%next
            do while(associated(pb))
                pc => pb%next
                deallocate(pb)
                pb => pc
            end do
            hIndex = hIndex + 1
        end do

    end subroutine deleteHashTable

    subroutine retrieveHashTable()

        type(pointID) :: pID
        type(point), pointer :: pb, pc
        character(len=70) :: fhashtxt
        integer :: hIndex, ierr, i

        do i = 1, HSIZE
            hashTable(i)%myID%le(1:DIM) = 0
            hashTable(i)%myID%li(1:DIM) = 0
            hashTable(i)%next => null()
        end do

        fhashtxt = HASHPREFIX//'hashTable.txt'
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

            call copyID(pc%myID, pID)
            read(10, fmt="(es15.8)") pc%value(:)
            read(10, fmt="(es15.8)") pc%surplus(:)
            read(10, fmt="(es15.8)") pc%lend(:)
            read(10, fmt="(es15.8)") pc%rend(:)
            read(10, fmt="(es15.8)") pc%coordinate(:)
            pc%next => null()
        end do

        close(10)

    end subroutine retrieveHashTable

end module RST_hashTable




