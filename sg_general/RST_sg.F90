
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
#ifdef FLASH
    use RST_flashcalculation
#elif NN
    use RST_getEquResidual
#endif

    implicit none

contains

    subroutine getTrueValue(x, y, isRea)

        real(kind=8), dimension(:), pointer, intent(in) :: x
        real(kind=8), dimension(:), pointer, intent(in out) :: y
        logical, intent(out) :: isRea

        real(kind=8) :: sum
        integer :: i

#ifdef FLASH
        real(kind=8) :: P
        real(kind=8), dimension(:), pointer :: local_z, xx, yy, local_v
        real(kind=8) :: xiL, xiG, rhoL, rhoG, sL, local_Cf
        logical :: isL, isG
#elif NN
        real(kind=8), dimension(:), pointer :: inputLayer
#endif

#ifdef FLASH
        
        allocate(local_z(1:DIM))
        allocate(xx(1:DIM))
        allocate(yy(1:DIM))
        allocate(local_v(1:DIM))

        P = PMIN + x(1)*(PMAX-PMIN)
        local_z(DIM) = 1.D0
        sum = 0.D0
        do i = 1, DIM-1
            local_z(i) = x(i+1)
            local_z(DIM) = local_z(DIM) - local_z(i)
            sum = sum + x(i+1)
        end do
        if(sum > 1.D0) then
            isRea = .false.
        else
            isRea = .true.
        end if

        if(isRea) then
            call flashcalculation(P, local_z, xx, yy, xiL, xiG, rhoL, rhoG, sL, &!
                local_v, local_Cf, isL, isG, isRea)
            TNumF = TNumF + 1
        end if

        if(isRea) then
            y(1:DIM) = xx(1:DIM)
            y(DIM+1:DIM*2) = yy(1:DIM)
            y(DIM*2+1) = xiL
            y(DIM*2+2) = xiG
            y(DIM*2+3) = rhoL
            y(DIM*2+4) = rhoG
            y(DIM*2+5) = sL
            y(DIM*2+6:DIM*3+5) = local_v(1:DIM)
            y(DIM*3+6) = local_Cf
        end if

        deallocate(local_z)
        deallocate(xx)
        deallocate(yy)
        deallocate(local_v)

#elif NN

        if(isFirstNN) then
            isFirstNN = .false.
            call retrieveNN()
        end if

        allocate(inputLayer(DIM+2))

        inputLayer(1) =  x(1)
        inputLayer(2) =  Temp

        sum = 0.D0
        do i = 2, DIM
            inputLayer(i+1) = x(i)
            sum = sum + x(i)
        end do
        inputLayer(DIM+2) = 1.D0 - sum
        if(inputLayer(DIM+2) > 0) then
            isRea = .true.
        else
            isRea = .false.
        end if
        
        if(isRea) then
            call getEquResidual(inputLayer, y)
        end if

        deallocate(inputLayer)

#endif

    end subroutine getTrueValue

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

    subroutine setPoint(pp, pq, y, yEst)

        type(point), pointer, intent(inout) :: pp
        type(queueNode), pointer, intent(in) :: pq
        real(kind=8), dimension(:), pointer, intent(in) :: y
        real(kind=8), dimension(:), pointer, intent(in) :: yEst
        integer :: i

        call copyID(pp%myID, pq%myID)

        pp%value(1:SURPLUSSIZE) = y(1:SURPLUSSIZE)
        pp%surplus(1:SURPLUSSIZE) = pp%value(1:SURPLUSSIZE) - yEst(1:SURPLUSSIZE)

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
        logical :: isRea, isInHash
        real(kind=8), dimension(:), pointer :: x, y, yEst
        type(point), pointer :: pb, pc
        integer :: hIndex, i
        
        allocate(x(1:DIM))
        allocate(y(1:SURPLUSSIZE))
        allocate(yEst(1:SURPLUSSIZE))

        isInHash = inHash(pq%myID)
        isRea = .false.
        if(.not.isInHash) then
            do i = 1, DIM
                x(i) = locatePoint(pq%myID, i)
            end do
            call getTrueValue(x, y, isRea)
        end if

        if(isRea) then
!print *, getLayer(pq%myID), '**', pq%myID

            call sgInterpo(x, yEst)
            hIndex = hash(pq%myID)
            if(hashTable(hIndex)%myID%le(1) == 0) then
                pc => hashTable(hIndex)
                call setPoint(pc, pq, y, yEst)
            else
                pb => hashTable(hIndex)
                pc => hashTable(hIndex)%next
                do while(associated(pc))
                    pb => pc
                    pc => pc%next
                end do
                allocate(pc)  ! will not deallocate
                pb%next => pc
                call setPoint(pc, pq, y, yEst)
            end if
        end if

        deallocate(x)
        deallocate(y)
        deallocate(yEst)

    end function addPointToHash

    subroutine updatePoint(pID)

        type(pointID), intent(in) :: pID
        type(point), pointer :: pp
        real(kind=8), dimension(:), pointer :: x, yEst
        integer :: hIndex, i

        hIndex = hash(pID)
        pp => hashTable(hIndex)
        do while(associated(pp))
            if(pointEqual(pp%myID, pID)) then
                allocate(x(1:DIM))
                allocate(yEst(1:SURPLUSSIZE))
                do i = 1, DIM
                    x(i) = locatePoint(pID, i)
                end do
                call sgInterpo(x, yEst)
                pp%surplus(1:SURPLUSSIZE) = pp%value(1:SURPLUSSIZE) - yEst(1:SURPLUSSIZE)
                deallocate(x)
                deallocate(yEst)
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

        real(kind=8), dimension(:), pointer :: x, y, yEst
        logical, dimension(:), pointer :: stamptemp
        logical :: isRea, isInHash, isInsert
        type(point), pointer :: pb, pc
        type(queueNode), pointer :: lChild, rChild, pr
        integer :: hIndex, i

        allocate(stamptemp(1:SURPLUSSIZE))
        allocate(x(1:DIM))
        allocate(y(1:SURPLUSSIZE))
        allocate(yEst(1:SURPLUSSIZE))
       
        isInHash = inHash(downHead%myID)
        isRea = .false.
        if(.not.isInHash) then
            do i = 1, DIM
                x(i) = locatePoint(downHead%myID, i)
            end do
            call getTrueValue(x, y, isRea)
        end if

        isInsert = .false.
        if(isRea) then
            call sgInterpo(x, yEst)
            stamptemp(1:SURPLUSSIZE) = .false.
            do i = 1, SURPLUSSIZE
                if(abs(yEst(i)-y(i)) > yprec(i)) then
                    stamptemp(i) = .true.
                end if
            end do
            do i = 1, SURPLUSSIZE
                if(stamptemp(i)) then
                    isInsert = .true.
                    exit
                end if
            end do
        end if

!print *, getLayer(downHead%myID), '**', downHead%myID
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

            call sgInterpo(x, yEst)
            
            hIndex = hash(downHead%myID)
            if(hashTable(hIndex)%myID%le(1) == 0) then
                pc => hashTable(hIndex)
                call setPoint(pc, downHead, y, yEst)
            else
                pb => hashTable(hIndex)
                pc => hashTable(hIndex)%next
                do while(associated(pc))
                    pb => pc
                    pc => pc%next
                end do
                allocate(pc)  ! will not deallocate
                pb%next => pc
                call setPoint(pc, downHead, y, yEst)
            end if
        end if

        if(.not.(.not.isInsert.and.isRea)) then
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

        deallocate(stamptemp)
        deallocate(x)
        deallocate(y)
        deallocate(yEst)

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
#ifdef FLASH
        print *, 'The times to do flash calculations are ', TNumF
#endif

    end subroutine sgOutput

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine sgHierOutput()

        type(point), pointer :: pp
        integer, dimension(1:HSIZE) :: nHash
        character(len=50) :: fnhashtxt, fhashtxt, funiontxt
        logical :: alive
        integer :: hIndex, i

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

        do i = 1, MAXLAYER
            hIndex = 1
            pp => hashTable(1)
            do while(hIndex <= HSIZE)
                if((pp%myID%le(1) /= 0).and.(getLayer(pp%myID) == i)) then
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
        end do

        fnhashtxt = trim(adjustl(sgSoluDoc))//'/numHash.txt'
        open(unit=12, file=trim(adjustl(fnhashtxt)), status='replace')
        write(12, fmt="(i4)") nHash(:)
        close(10)
        close(11)
        close(12)

        print *, 'The total number of points is ', TNumSG
        print *, 'The sparse degree is ', TNumSG*1.D2/(2**MAXLAYER-1)**DIM, '%'
#ifdef FLASH
        print *, 'The times to do flash calculations are ', TNumF
#endif

    end subroutine sgHierOutput
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    recursive subroutine sgTest(dIndex)

        integer, intent(in) :: dIndex
        real(kind=8), dimension(:), pointer :: x, y, yEst
        logical :: isRea
        integer :: i

        if(dIndex == DIM+1) then
            allocate(x(1:DIM))
            allocate(y(1:SURPLUSSIZE))
            allocate(yEst(1:SURPLUSSIZE))

            do i = 1, DIM
                x(i) = sgTestArr(i)*1.D0/(NUMSAMPLE+1)
            end do
           
            call getTrueValue(x, y, isRea)
            if(isRea) then
                call sgInterpo(x, yEst)
                do i = 1, SURPLUSSIZE
                    if(abs(y(i)) < EQUALPREC) then
                        write(10+i, fmt="(es15.8)") 0.D0
                    else
                        write(10+i, fmt="(es15.8)") abs((yEst(i)-y(i))/y(i))
                    end if
                end do
            end if

            deallocate(x)
            deallocate(y)
            deallocate(yEst)

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
        character(len=50), dimension(:), pointer :: ftxt
        logical :: alive
        integer :: i

        inquire(file = trim(adjustl(sgSoluDoc))//'/error', exist = alive)
        if(.not.alive) then
            call system("mkdir "//trim(adjustl(sgSoluDoc))//'/error')
        end if

        allocate(ftxt(1:SURPLUSSIZE))
        do i = 1, SURPLUSSIZE
            write(charm,'(i2)') i
            ftxt(i) = trim(adjustl(sgSoluDoc))//'/error/y'//trim(adjustl(charm))//'.txt'
        end do

        do i = 1, SURPLUSSIZE
            open(unit=10+i, file=trim(adjustl(ftxt(i))), status='replace')
        end do
       
        sgTestArr(1:DIM) = 1
        call sgTest(1)

        do i = 1, SURPLUSSIZE
            close(10+i)
        end do

        do i = 1, DIM
            write(charm,'(i2)') i
            call rename(ftxt(i), trim(adjustl(sgSoluDoc))//'/error/x'//trim(adjustl(charm))//'.txt')
        end do
        do i = 1, DIM
            write(charm,'(i2)') i
            call rename(ftxt(DIM+i), trim(adjustl(sgSoluDoc))//'/error/y'//trim(adjustl(charm))//'.txt')
        end do
        call rename(ftxt(DIM*2+1), trim(adjustl(sgSoluDoc))//'/error/xiL.txt')
        call rename(ftxt(DIM*2+2), trim(adjustl(sgSoluDoc))//'/error/xiG.txt')
        call rename(ftxt(DIM*2+3), trim(adjustl(sgSoluDoc))//'/error/rhoL.txt')
        call rename(ftxt(DIM*2+4), trim(adjustl(sgSoluDoc))//'/error/rhoG.txt')
        call rename(ftxt(DIM*2+5), trim(adjustl(sgSoluDoc))//'/error/sL.txt')
        do i = 1, DIM
            write(charm,'(i2)') i
            call rename(ftxt(DIM*2+5+i), trim(adjustl(sgSoluDoc))//'/error/v'//trim(adjustl(charm))//'.txt')
        end do
        call rename(ftxt(DIM*3+6), trim(adjustl(sgSoluDoc))//'/error/Cf.txt')

        deallocate(ftxt)

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
#ifdef NN
        isFirstNN = .true.
#endif

#ifdef FLASH
        yprec(1:DIM*2) = XYPREC
        yprec(DIM*2+1) = XILPREC
        yprec(DIM*2+2) = XIGPREC
        yprec(DIM*2+3) = RHOLPREC
        yprec(DIM*2+4) = RHOGPREC
        yprec(DIM*2+5) = SLPREC
        yprec(DIM*2+6:DIM*3+5) = VPREC
        yprec(DIM*3+6) = CFPREC
#elif NN
        yprec(1:SURPLUSSIZE) = RESIDUALPREC
#endif

        allocate(god)
        god%myID%le(1:DIM) = 1
        god%myID%li(1:DIM) = 1
        god%next => null()
        downHead => god
        downTail => god
        do while(associated(downHead))
            call sgHierarchy()
        end do
        !call sgHierOutput()
        call sgOutput()
        !call sgTestDriver()
        call deleteHashTable()

    end subroutine sgDriver

end module RST_sg

