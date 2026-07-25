
!!$ Author:
!!$   Yuanqing Wu, KAUST, Saudi Arabia
!!$
!!$ History:
!!$   2017-2-9 by Yuanqing Wu
!!$
!!$ Support:
!!$   wuyuanq@gmail.com

module RST_sg

    use RST_globalSgData
    use RST_hashTable
    use RST_flashcalculation
    implicit none

contains

    function getLayer(pID) result(layer)

        type(pointID), intent(in) :: pID
        integer :: layer
        integer :: i

        layer = 0
        do i = 1, DIM
            layer = layer + pID%le(i)
        end do
        layer = layer - DIM + 1

    end function getLayer

    function locatePoint(pID, dimN) result(coord)

        type(pointID), intent(in) :: pID
        integer, intent(in) :: dimN
        real(kind=8) :: coord

        coord = 2.D0**(-pID%le(dimN))*pID%li(dimN)

    end function locatePoint

    subroutine setPoint(pp, pq, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, &!
        xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)

        type(point), pointer, intent(inout) :: pp
        type(queueNode), pointer, intent(in) :: pq
        real(kind=8), dimension(:), pointer, intent(in) :: x
        real(kind=8), dimension(:), pointer, intent(in) :: y
        real(kind=8), intent(in) :: xiL
        real(kind=8), intent(in) :: xiG
        real(kind=8), intent(in) :: rhoL
        real(kind=8), intent(in) :: rhoG
        real(kind=8), intent(in) :: sL
        real(kind=8), dimension(:), pointer, intent(in) :: local_v
        real(kind=8), intent(in) :: local_Cf
        real(kind=8), dimension(:), pointer, intent(in) :: xEst
        real(kind=8), dimension(:), pointer, intent(in) :: yEst
        real(kind=8), intent(in) :: xiLEst
        real(kind=8), intent(in) :: xiGEst
        real(kind=8), intent(in) :: rhoLEst
        real(kind=8), intent(in) :: rhoGEst
        real(kind=8), intent(in) :: sLEst
        real(kind=8), dimension(:), pointer, intent(in) :: local_vEst
        real(kind=8), intent(in) :: local_CfEst
        integer :: i

        call copyID(pp%myID, pq%myID)

        pp%value(1:DIM) = x(1:DIM)
        pp%value(DIM+1:2*DIM) = y(1:DIM)
        pp%value(2*DIM+1) = xiL
        pp%value(2*DIM+2) = xiG
        pp%value(2*DIM+3) = rhoL
        pp%value(2*DIM+4) = rhoG
        pp%value(2*DIM+5) = sL
        pp%value(2*DIM+6:3*DIM+5) = local_v(1:DIM)
        pp%value(3*DIM+6) = local_Cf

        pp%surplus(1:DIM) = pp%value(1:DIM) - xEst(1:DIM)
        pp%surplus(DIM+1:2*DIM) = pp%value(DIM+1:2*DIM) - yEst(1:DIM)
        pp%surplus(2*DIM+1) = pp%value(2*DIM+1) - xiLEst
        pp%surplus(2*DIM+2) = pp%value(2*DIM+2) - xiGEst
        pp%surplus(2*DIM+3) = pp%value(2*DIM+3) - rhoLEst
        pp%surplus(2*DIM+4) = pp%value(2*DIM+4) - rhoGEst
        pp%surplus(2*DIM+5) = pp%value(2*DIM+5) - sLEst
        pp%surplus(2*DIM+6:3*DIM+5) = pp%value(2*DIM+6:3*DIM+5) - local_vEst(1:DIM)
        pp%surplus(3*DIM+6) = pp%value(3*DIM+6) - local_CfEst

        do i = 1, DIM
            pp%lend(i) = 2.D0**(-pq%myID%le(i))*(pq%myID%li(i)-1)
            pp%rend(i) = 2.D0**(-pq%myID%le(i))*(pq%myID%li(i)+1)
            pp%coordinate(i) = 2.D0**(-pq%myID%le(i))*pq%myID%li(i)
        end do

        pp%next => null()

    end subroutine setPoint

    subroutine addParentToQueue(childID)

        type(pointID), intent(in) :: childID

        type(pointID), dimension(DIM) :: parentID
        type(queueNode), pointer :: pq
        integer :: i

        do i = 1, DIM
            call copyID(parentID(i), childID)
            parentID(i)%le(i) = childID%le(i) - 1
            if(mod((childID%li(i)+1)/2,2) == 0) then ! not odd
                parentID(i)%li(i) = (childID%li(i)-1) / 2
            else
                parentID(i)%li(i) = (childID%li(i)+1) / 2
            end if

            if((parentID(i)%le(i)>0).and.(parentID(i)%li(i)>0)) then
                if(inHash(parentID(i))) then
                    if(isUpEmpty) then
                        allocate(upTail)
                        call copyID(upTail%myID, parentID(i))
                        upTail%next => null()
                        upHead => upTail
                        isUpEmpty = .false.
                    else
                        allocate(pq)
                        call copyID(pq%myID, parentID(i))
                        pq%next => null()
                        upTail%next => pq
                        upTail => pq
                    end if
                end if
            end if
        end do

    end subroutine addParentToQueue

    subroutine addAncestorToQueue()

        type(queueNode), pointer :: pq

        pq => upHead
        do while(associated(pq))
            call addParentToQueue(pq%myID)
            pq => pq%next
        end do

    end subroutine addAncestorToQueue

    function addPointToHash(pq) result(isRea)

        type(queueNode), pointer, intent(in) :: pq

        real(kind=8) :: P
        real(kind=8), dimension(:), pointer :: z, local_z
        real(kind=8), dimension(:), pointer :: x, xEst
        real(kind=8), dimension(:), pointer :: y, yEst
        real(kind=8) :: xiL, xiLEst
        real(kind=8) :: xiG, xiGEst
        real(kind=8) :: rhoL, rhoLEst
        real(kind=8) :: rhoG, rhoGEst
        real(kind=8) :: sL, sLEst
        real(kind=8), dimension(:), pointer :: local_v, local_vEst
        real(kind=8) :: local_Cf, local_CfEst
        logical :: isW, isN, isRea
        logical :: isInRange, isInHash
        integer :: hIndex
        type(point), pointer :: pb, pc
        real(kind=8) :: sum
        integer :: i

        allocate(z(1:DIM-1))
        allocate(local_z(1:DIM))
        allocate(x(1:DIM))
        allocate(y(1:DIM))
        allocate(local_v(1:DIM))
        allocate(xEst(1:DIM))
        allocate(yEst(1:DIM))
        allocate(local_vEst(1:DIM))

        P = PMIN + locatePoint(pq%myID, 1)*(PMAX-PMIN)
        local_z(DIM) = 1.D0
        sum = 0.D0
        do i = 1, DIM-1
            z(i) = locatePoint(pq%myID, i+1)
            local_z(i) = z(i)
            local_z(DIM) = local_z(DIM) - local_z(i)
            sum = sum + z(i)
        end do
        if(sum > 1.D0) then
            isInRange = .false.
        else
            isInRange = .true.
        end if

        isInHash = .true.
        if(isInRange) then
            isInHash = inHash(pq%myID)
        end if

        isRea = .false.
        if(.not.isInHash) then
            call flashcalculation(P, local_z, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, isW, isN, isRea)
            TNumF = TNumF + 1
        end if

        if(isRea) then
print *, getLayer(pq%myID), '**', pq%myID

            call sgInterpo(P, z, xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)
            hIndex = hash(pq%myID)
            if(hashTable(hIndex)%myID%le(1) == 0) then
                pc => hashTable(hIndex)
                call setPoint(pc, pq, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, &!
                    xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)
            else
                pb => hashTable(hIndex)
                pc => hashTable(hIndex)%next
                do while(associated(pc))
                    pb => pc
                    pc => pc%next
                end do
                allocate(pc)  ! will not deallocate
                pb%next => pc
                call setPoint(pc, pq, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, &!
                    xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)
            end if
        end if

        deallocate(z)
        deallocate(local_z)
        deallocate(x)
        deallocate(y)
        deallocate(local_v)
        deallocate(xEst)
        deallocate(yEst)
        deallocate(local_vEst)

    end function addPointToHash

    subroutine updatePoint(pID)

        type(pointID), intent(in) :: pID

        real(kind=8) :: P
        real(kind=8), dimension(:), pointer :: z
        integer :: hIndex
        type(point), pointer :: pp
        real(kind=8), dimension(:), pointer :: xEst
        real(kind=8), dimension(:), pointer :: yEst
        real(kind=8) :: xiLEst
        real(kind=8) :: xiGEst
        real(kind=8) :: rhoLEst
        real(kind=8) :: rhoGEst
        real(kind=8) :: sLEst
        real(kind=8), dimension(:), pointer :: local_vEst
        real(kind=8) :: local_CfEst
        integer :: i

        hIndex = hash(pID)
        pp => hashTable(hIndex)
        do while(associated(pp))
            if(pointEqual(pp%myID, pID)) then
                allocate(z(1:DIM-1))
                allocate(xEst(1:DIM))
                allocate(yEst(1:DIM))
                allocate(local_vEst(1:DIM))
                P = PMIN + locatePoint(pID, 1)*(PMAX-PMIN)
                do i = 1, DIM-1
                    z(i) = locatePoint(pID, i+1)
                end do
                call sgInterpo(P, z, xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)
                pp%surplus(1:DIM) = pp%value(1:DIM) - xEst(1:DIM)
                pp%surplus(DIM+1:2*DIM) = pp%value(DIM+1:2*DIM) - yEst(1:DIM)
                pp%surplus(2*DIM+1) = pp%value(2*DIM+1) - xiLEst
                pp%surplus(2*DIM+2) = pp%value(2*DIM+2) - xiGEst
                pp%surplus(2*DIM+3) = pp%value(2*DIM+3) - rhoLEst
                pp%surplus(2*DIM+4) = pp%value(2*DIM+4) - rhoGEst
                pp%surplus(2*DIM+5) = pp%value(2*DIM+5) - sLEst
                pp%surplus(2*DIM+6:3*DIM+5) = pp%value(2*DIM+6:3*DIM+5) - local_vEst(1:DIM)
                pp%surplus(3*DIM+6) = pp%value(3*DIM+6) - local_CfEst
                deallocate(z)
                deallocate(xEst)
                deallocate(yEst)
                deallocate(local_vEst)
                exit
            else
                pp => pp%next
            end if
        end do

    end subroutine updatePoint

    subroutine addAncestorToHash()

        type(queueNode), pointer :: pq, pql, pqm, pqr, pn, myHead, myTail, pt, pr
        type(point), pointer :: pp
        type(pointID) :: lChildID, rChildID
        logical :: isInHash
        integer :: hIndex, i

        ! reverse the queue
        if(associated(upHead%next)) then
            pql => upHead
            pqm => upHead%next
            pqr => pqm%next
            do while(associated(pqr))
                pqm%next => pql
                pql => pqm
                pqm => pqr
                pqr => pqr%next
            end do
            pqm%next => pql
            pq => upHead
            upHead => upTail
            upTail => pq
            upTail%next => null()
        end if

        pq => upHead
        do while(associated(pq))
            if(addPointToHash(pq)) then
                allocate(myHead)
                call copyID(myHead%myID, pq%myID)
                myHead%next => null()
                myTail => myHead
                pn => myHead
                do while(associated(pn))
                    if(.not.associated(pn,myHead)) then
                        if(inHash(pn%myID)) then
                            call updatePoint(pn%myID)
                        end if
                    end if

                    if(getLayer(pn%myID) < getLayer(downHead%myID)) then
                        do i = 1, DIM
                            call copyID(lChildID, pn%myID)
                            call copyID(rChildID, pn%myID)
                            lChildID%le(i) = pn%myID%le(i) + 1
                            lChildID%li(i) = 2*pn%myID%li(i) - 1
                            rChildID%le(i) = pn%myID%le(i) + 1
                            rChildID%li(i) = 2*pn%myID%li(i) + 1

                            allocate(pt)
                            call copyID(pt%myID, lChildID)
                            pt%next => null()
                            myTail%next => pt
                            myTail => pt
                            pt => null()
                            allocate(pt)
                            call copyID(pt%myID, rChildID)
                            pt%next => null()
                            myTail%next => pt
                            myTail => pt
                            pt => null()
                        end do
                    end if

                    pn => pn%next
                end do

                pn => myHead
                do while(associated(pn))
                    pr => pn
                    pn => pn%next
                    deallocate(pr)
                end do
            end if

            pq => pq%next
        end do

        pq => upHead
        do while(associated(pq))
            pr => pq
            pq => pq%next
            deallocate(pr)
        end do

    end subroutine addAncestorToHash

    subroutine sgHierarchy()

        real(kind=8) :: P
        real(kind=8), dimension(:), pointer :: z, local_z
        real(kind=8), dimension(:), pointer :: x, xEst
        real(kind=8), dimension(:), pointer :: y, yEst
        real(kind=8) :: xiL, xiLEst
        real(kind=8) :: xiG, xiGEst
        real(kind=8) :: rhoL, rhoLEst
        real(kind=8) :: rhoG, rhoGEst
        real(kind=8) :: sL, sLEst
        real(kind=8), dimension(:), pointer :: local_v, local_vEst
        real(kind=8) :: local_Cf, local_CfEst
        logical :: isW, isN, isRea
        logical, dimension(:), pointer :: stamptemp
        logical :: isInRange, isInHash, isInsert
        integer :: hIndex
        type(point), pointer :: pb, pc
        type(queueNode), pointer :: lChild, rChild, pr
        real(kind=8) :: sum
        integer :: i

        allocate(z(1:DIM-1))
        allocate(local_z(1:DIM))
        allocate(x(1:DIM))
        allocate(y(1:DIM))
        allocate(local_v(1:DIM))
        allocate(xEst(1:DIM))
        allocate(yEst(1:DIM))
        allocate(local_vEst(1:DIM))
        allocate(stamptemp(1:SURPLUSSIZE))

        P = PMIN + locatePoint(downHead%myID, 1)*(PMAX-PMIN)
        local_z(DIM) = 1.D0
        sum = 0.D0
        do i = 1, DIM-1
            z(i) = locatePoint(downHead%myID, i+1)
            local_z(i) = z(i)
            local_z(DIM) = local_z(DIM) - local_z(i)
            sum = sum + z(i)
        end do
        if(sum > 1.D0) then
            isInRange = .false.
        else
            isInRange = .true.
        end if

        isInHash = .true.
        if(isInRange) then
            isInHash = inHash(downHead%myID)
        end if

        isRea = .false.
        if(.not.isInHash) then
            call flashcalculation(P, local_z, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, isW, isN, isRea)
            TNumF = TNumF + 1
        end if

        isInsert = .false.
        if(isRea) then
            call sgInterpo(P, z, xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)

            stamptemp(1:SURPLUSSIZE) = .false.
            do i = 1, DIM
                if(abs(xEst(i)-x(i)) > XYPREC) then
                    stamptemp(i) = .true.
                end if
            end do
            do i = 1, DIM
                if(abs(yEst(i)-y(i)) > XYPREC) then
                    stamptemp(DIM+i) = .true.
                end if
            end do
            if(abs(xiLEst-xiL) > XILPREC) then
                stamptemp(2*DIM+1) = .true.
            end if
            if(abs(xiGEst-xiG) > XIGPREC) then
                stamptemp(2*DIM+2) = .true.
            end if
            if(abs(rhoLEst-rhoL) > RHOLPREC) then
                stamptemp(2*DIM+3) = .true.
            end if
            if(abs(rhoGEst-rhoG) > RHOGPREC) then
                stamptemp(2*DIM+4) = .true.
            end if
            if(abs(sLEst-sL) > SLPREC) then
                stamptemp(2*DIM+5) = .true.
            end if
            do i = 1, DIM
                if(abs(local_vEst(i)-local_v(i)) > VPREC) then
                    stamptemp(2*DIM+5+i) = .true.
                end if
            end do
            if(abs(local_CfEst-local_Cf) > CFPREC) then
                stamptemp(3*DIM+6) = .true.
            end if
            do i = 1, SURPLUSSIZE
                if(stamptemp(i)) then
                    isInsert = .true.
                    exit
                end if
            end do
        end if
print *, getLayer(downHead%myID), '**', downHead%myID
        if(isInsert) then
            ! add ancestors
            upHead => null()
            upTail => null()
            isUpEmpty = .true.
            call addParentToQueue(downHead%myID)
            if(.not.isUpEmpty) then
                call addAncestorToQueue()
                call addAncestorToHash()
            end if

            call sgInterpo(P, z, xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)
            
            hIndex = hash(downHead%myID)
            if(hashTable(hIndex)%myID%le(1) == 0) then
                pc => hashTable(hIndex)
                call setPoint(pc, downHead, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, &!
                    xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)
            else
                pb => hashTable(hIndex)
                pc => hashTable(hIndex)%next
                do while(associated(pc))
                    pb => pc
                    pc => pc%next
                end do
                allocate(pc)  ! will not deallocate
                pb%next => pc
                call setPoint(pc, downHead, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, &!
                    xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)
            end if
        end if

        if(.not.(.not.isInsert.and.isRea.and.isInRange)) then
            if(getLayer(downHead%myID) < MAXLAYER) then
                do i = 1, DIM
                    allocate(lChild)
                    allocate(rChild)
                    call copyID(lChild%myID, downHead%myID)
                    call copyID(rChild%myID, downHead%myID)
                    lChild%myID%le(i) = downHead%myID%le(i) + 1
                    lChild%myID%li(i) = 2*downHead%myID%li(i) - 1
                    rChild%myID%le(i) = downHead%myID%le(i) + 1
                    rChild%myID%li(i) = 2*downHead%myID%li(i) + 1
                    rChild%next => null()
                    ! add the children to the queue
                    downTail%next => lChild
                    lChild%next => rChild
                    downTail => rChild
                end do
            end if
        end if

        ! remove itself from the queue
        if(associated(downHead%next)) then
            pr => downHead
            downHead => downHead%next
            deallocate(pr)
        else
            deallocate(downHead)
            downHead => null()
            downTail => null()
        end if

        deallocate(z)
        deallocate(local_z)
        deallocate(x)
        deallocate(y)
        deallocate(local_v)
        deallocate(xEst)
        deallocate(yEst)
        deallocate(local_vEst)
        deallocate(stamptemp)

    end subroutine sgHierarchy

    subroutine sgOutput()

        type(point), pointer :: pp
        integer, dimension(1:HSIZE) :: nHash
        character(len=50) :: fnhashtxt, fhashtxt, funiontxt
        logical :: alive
        integer :: hIndex

        inquire(file = trim(adjustl(sgSoluDoc))//'/sg', exist = alive)
        if(.not.alive) then
            call system("mkdir "//trim(adjustl(sgSoluDoc))//'/sg')
        end if
        funiontxt = trim(adjustl(sgSoluDoc))//'/sg/union.txt'
        open(unit=10, file=trim(adjustl(funiontxt)), status='replace')

        fhashtxt = trim(adjustl(sgSoluDoc))//'/hashTable.txt'
        open(unit=11, file=trim(adjustl(fhashtxt)), status='replace')

        TNumSG = 0
        nHash(1:HSIZE) = 0
        hIndex = 1
        pp => hashTable(1)
        do while(hIndex <= HSIZE)
            if(pp%myID%le(1) /= 0) then
                write(10, fmt="(es15.8)") pp%coordinate

                nHash(hIndex) = nHash(hIndex) + 1
                TNumSG = TNumSG + 1

                write(11, fmt="(i4)") pp%myID%le
                write(11, fmt="(i4)") pp%myID%li
                write(11, fmt="(es15.8)") pp%value
                write(11, fmt="(es15.8)") pp%surplus
                write(11, fmt="(es15.8)") pp%lend
                write(11, fmt="(es15.8)") pp%rend
                write(11, fmt="(es15.8)") pp%coordinate
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

        fnhashtxt = trim(adjustl(sgSoluDoc))//'/numHash.txt'
        open(unit=12, file=trim(adjustl(fnhashtxt)), status='replace')
        write(12, fmt="(i4)") nHash(:)
        close(10)
        close(11)
        close(12)

        print *, 'The total number of points is ', TNumSG
        print *, 'The sparse degree is ', TNumSG*1.D2/(2**MAXLAYER-1)**DIM, '%'
        print *, 'The times to do flash calculations are ', TNumF

    end subroutine sgOutput

    recursive subroutine sgTest(dIndex)

        integer, intent(in) :: dIndex
        real(kind=8) :: P
        real(kind=8), dimension(:), pointer :: z, local_z
        real(kind=8), dimension(:), pointer :: x, xEst
        real(kind=8), dimension(:), pointer :: y, yEst
        real(kind=8) :: xiL, xiLEst
        real(kind=8) :: xiG, xiGEst
        real(kind=8) :: rhoL, rhoLEst
        real(kind=8) :: rhoG, rhoGEst
        real(kind=8) :: sL, sLEst
        real(kind=8), dimension(:), pointer :: local_v, local_vEst
        real(kind=8) :: local_Cf, local_CfEst
        logical :: isW, isN, isRea
        real(kind=8) :: sum
        logical :: isInRange
        integer :: i

        if(dIndex == DIM+1) then
            allocate(z(1:DIM-1))
            allocate(local_z(1:DIM))

            P = PMIN + (PMAX-PMIN)*sgTestArr(1)/(NUMSAMPLE+1)
            local_z(DIM) = 1.D0
            sum = 0.D0
            do i = 1, DIM-1
                z(i) = sgTestArr(i+1)*1.D0/(NUMSAMPLE+1)
                local_z(i) = z(i)
                local_z(DIM) = local_z(DIM) - local_z(i)
                sum = sum + z(i)
            end do
            if(sum > 1.D0) then
                isInRange = .false.
            else
                isInRange = .true.
            end if

            if(isInRange) then
                allocate(x(1:DIM))
                allocate(y(1:DIM))
                allocate(local_v(1:DIM))
                allocate(xEst(1:DIM))
                allocate(yEst(1:DIM))
                allocate(local_vEst(1:DIM))

                call flashcalculation(P, local_z, x, y, &!
                    xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, isW, isN, isRea)
                if(isRea) then
                    call sgInterpo(P, z, xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, &!
                        sLEst, local_vEst, local_CfEst)
                    do i = 1, DIM
                        if(abs(x(i)) < EQUALPREC) then
                            write(10+i, fmt="(es15.8)") 0.D0
                        else
                            write(10+i, fmt="(es15.8)") abs((xEst(i)-x(i))/x(i))
                        end if
                        if(abs(y(i)) < EQUALPREC) then
                            write(10+DIM+i, fmt="(es15.8)") 0.D0
                        else
                            write(10+DIM+i, fmt="(es15.8)") abs((yEst(i)-y(i))/y(i))
                        end if
                        if(abs(local_v(i)) < EQUALPREC) then
                            write(15+2*DIM+i, fmt="(es15.8)") 0.D0
                        else
                            write(15+2*DIM+i, fmt="(es15.8)") abs((local_vEst(i)-local_v(i))/local_v(i))
                        end if
                    end do
                    if(abs(xiL) < EQUALPREC) then
                        write(11+2*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(11+2*DIM, fmt="(es15.8)") abs((xiLEst-xiL)/xiL)
                    end if
                    if(abs(xiG) < EQUALPREC) then
                        write(12+2*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(12+2*DIM, fmt="(es15.8)") abs((xiGEst-xiG)/xiG)
                    end if
                    if(abs(rhoL) < EQUALPREC) then
                        write(13+2*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(13+2*DIM, fmt="(es15.8)") abs((rhoLEst-rhoL)/rhoL)
                    end if
                    if(abs(rhoG) < EQUALPREC) then
                        write(14+2*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(14+2*DIM, fmt="(es15.8)") abs((rhoGEst-rhoG)/rhoG)
                    end if
                    if(abs(sL) < EQUALPREC) then
                        write(15+2*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(15+2*DIM, fmt="(es15.8)") abs((sLEst-sL)/sL)
                    end if
                    if(abs(local_Cf) < EQUALPREC) then
                        write(16+3*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(16+3*DIM, fmt="(es15.8)") abs((local_CfEst-local_Cf)/local_Cf)
                    end if
                end if

                deallocate(x)
                deallocate(y)
                deallocate(local_v)
                deallocate(xEst)
                deallocate(yEst)
                deallocate(local_vEst)
            end if

            deallocate(z)
            deallocate(local_z)

            return
        end if

        do while(sgTestArr(dIndex) <= NUMSAMPLE)
            call sgTest(dIndex+1)
            sgTestArr(dIndex) = sgTestArr(dIndex) + 1
            if((dIndex+1) <= DIM) then
                sgTestArr(dIndex+1) = 1
            end if
        end do

    end subroutine sgTest

    subroutine sgTestDriver()

        character(len=2) :: charm
        character(len=50) :: fxiltxt, fxigtxt, frholtxt, frhogtxt, fsltxt, fcftxt
        character(len=50), dimension(:), pointer :: fxtxt, fytxt, fvtxt
        logical :: alive
        integer :: i

        inquire(file = trim(adjustl(sgSoluDoc))//'/error', exist = alive)
        if(.not.alive) then
            call system("mkdir "//trim(adjustl(sgSoluDoc))//'/error')
        end if

        fxiltxt = trim(adjustl(sgSoluDoc))//'/error/xiL.txt'
        fxigtxt = trim(adjustl(sgSoluDoc))//'/error/xiG.txt'
        frholtxt = trim(adjustl(sgSoluDoc))//'/error/rhoL.txt'
        frhogtxt = trim(adjustl(sgSoluDoc))//'/error/rhoG.txt'
        fsltxt = trim(adjustl(sgSoluDoc))//'/error/sL.txt'
        fcftxt = trim(adjustl(sgSoluDoc))//'/error/Cf.txt'
        allocate(fxtxt(1:DIM))
        allocate(fytxt(1:DIM))
        allocate(fvtxt(1:DIM))
        do i = 1, DIM
            write(charm,'(i2)') i
            fxtxt(i) = trim(adjustl(sgSoluDoc))//'/error/x'//trim(adjustl(charm))//'.txt'
            fytxt(i) = trim(adjustl(sgSoluDoc))//'/error/y'//trim(adjustl(charm))//'.txt'
            fvtxt(i) = trim(adjustl(sgSoluDoc))//'/error/v'//trim(adjustl(charm))//'.txt'
        end do

        do i = 1, DIM
            open(unit=10+i, file=trim(adjustl(fxtxt(i))), status='replace')
            open(unit=10+DIM+i, file=trim(adjustl(fytxt(i))), status='replace')
            open(unit=15+2*DIM+i, file=trim(adjustl(fvtxt(i))), status='replace')
        end do
        open(unit=11+2*DIM, file=trim(adjustl(fxiltxt)), status='replace')
        open(unit=12+2*DIM, file=trim(adjustl(fxigtxt)), status='replace')
        open(unit=13+2*DIM, file=trim(adjustl(frholtxt)), status='replace')
        open(unit=14+2*DIM, file=trim(adjustl(frhogtxt)), status='replace')
        open(unit=15+2*DIM, file=trim(adjustl(fsltxt)), status='replace')
        open(unit=16+3*DIM, file=trim(adjustl(fcftxt)), status='replace')

        sgTestArr(1:DIM) = 1
        call sgTest(1)

        do i = 1, DIM
            close(10+i)
            close(10+DIM+i)
            close(15+2*DIM+i)
        end do
        close(11+2*DIM)
        close(12+2*DIM)
        close(13+2*DIM)
        close(14+2*DIM)
        close(15+2*DIM)
        close(16+3*DIM)

        deallocate(fxtxt)
        deallocate(fytxt)
        deallocate(fvtxt)

    end subroutine sgTestDriver

    subroutine sgDriver()

        type(queueNode), pointer :: god
        character(len=50) :: fprimetxt
        logical :: alive
        integer :: ierr, i

        if(430*(DIM-1) > PRIMESIZE+1) then
            print *, 'The prime table is not big enough!'
            print *, 'At least ', 430*(DIM-1)-1, ' prime numbers are needed!'
            stop
        end if

        !call genPrime(PRIMESIZE)
        fprimetxt = 'prime.txt'
        open(unit=10, file=trim(adjustl(fprimetxt)), iostat=ierr)
        if(ierr /= 0) then
            print *, 'open file error. ', ierr
            stop
        end if
        read(10, fmt="(i6)") prime(:)
        close(10)

        do i = 1, HSIZE
            hashTable(i)%myID%le(1:DIM) = 0
            hashTable(i)%myID%li(1:DIM) = 0
            hashTable(i)%next => null()
        end do
        inquire(file = sgSoluDoc, exist = alive)
        if(.not.alive) then
            call system("mkdir "//trim(adjustl(sgSoluDoc)))
        end if
        TNumF = 0

        allocate(god)
        god%myID%le(1:DIM) = 1
        god%myID%li(1:DIM) = 1
        god%next => null()
        downHead => god
        downTail => god
        do while(associated(downHead))
            call sgHierarchy()
        end do
        call sgOutput()
        call sgTestDriver()
        call deleteHashTable()

    end subroutine sgDriver

end module RST_sg

