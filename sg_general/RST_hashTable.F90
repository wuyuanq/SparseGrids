
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

    subroutine sgInterpo(x, yEst)

        real(kind=8), dimension(:), pointer, intent(in) :: x
        real(kind=8), dimension(:), pointer, intent(in out) :: yEst
        type(point), pointer :: pp
        logical :: isInRange
        integer :: hIndex
        real(kind=8) :: ratio
        integer :: i

        yEst(1:SURPLUSSIZE) = 0.D0
        hIndex = 1
        pp => hashTable(1)
        do while(hIndex <= HSIZE)
            if(pp%myID%le(1) /= 0) then
                isInRange = .true.
                ratio = 1.D0
                do i = 1, DIM
                    if((x(i)<pp%lend(i)).or.(x(i)>pp%rend(i))) then
                        isInRange = .false.
                        exit
                    else
                        ratio = ratio * ((pp%rend(i)-pp%lend(i))/2.D0 - abs(x(i)-pp%coordinate(i)))/ &!
                            ((pp%rend(i)-pp%lend(i))/2.D0)
                    end if
                end do
                if(isInRange) then
                    do i = 1, SURPLUSSIZE
                        yEst(i) = yEst(i) + pp%surplus(i)*ratio
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




