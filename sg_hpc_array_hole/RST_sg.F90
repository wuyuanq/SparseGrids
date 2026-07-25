
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
    use RST_flashcalculation
    implicit none

    interface assignment (=)
        module procedure pointAssign
    end interface

contains

    function hash(pID) result(hIndex)

        type(pointID), intent(in) :: pID
        integer(kind=8) :: sum
        integer :: hIndex, i

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

    subroutine hashTableClean()

        type(hashNode), pointer :: phn1, phn2
        integer :: i

        do i = 1, HSIZE
            phn1 => hashTable(i)%next
            do while(associated(phn1))
                phn2 => phn1
                phn1 => phn1%next
                deallocate(phn2)
            end do
        end do

        do i = 1, HSIZE
            hashTable(i)%myID%le(1:DIM) = 0
            hashTable(i)%myID%li(1:DIM) = 0
            hashTable(i)%next => null()
        end do

    end subroutine hashTableClean

    function pointLayer(pID) result(layer)

        type(pointID), intent(in) :: pID
        integer :: layer
        integer :: i

        layer = 0
        do i = 1, DIM
            layer = layer + pID%le(i)
        end do
        layer = layer - DIM + 1

    end function pointLayer

    subroutine pointAssign(pID1, pID2)

        type(pointID), intent(in out) :: pID1
        type(pointID), intent(in) :: pID2
        integer :: i

        do i = 1, DIM
            pID1%le(i) = pID2%le(i)
            pID1%li(i) = pID2%li(i)
        end do

    end subroutine pointAssign

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

    function pointLocate(pID, dimN) result(coord)

        type(pointID), intent(in) :: pID
        integer, intent(in) :: dimN
        real(kind=8) :: coord

        coord = 2.D0**(-pID%le(dimN))*pID%li(dimN)

    end function pointLocate

    subroutine pointSet(thisNode, myPos, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, stamptemp, &!
        xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)

        type(queueNode), pointer, intent(in) :: thisNode
        integer, intent(in) :: myPos
        real(kind=8), dimension(:), pointer, intent(in) :: x
        real(kind=8), dimension(:), pointer, intent(in) :: y
        real(kind=8), intent(in) :: xiL
        real(kind=8), intent(in) :: xiG
        real(kind=8), intent(in) :: rhoL
        real(kind=8), intent(in) :: rhoG
        real(kind=8), intent(in) :: sL
        real(kind=8), dimension(:), pointer, intent(in) :: local_v
        real(kind=8), intent(in) :: local_Cf
        logical, dimension(:), pointer, intent(in) :: stamptemp
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

        sgTable(myPos)%myID = thisNode%myID

        sgTable(myPos)%value(1:DIM) = x(1:DIM)
        sgTable(myPos)%value(DIM+1:2*DIM) = y(1:DIM)
        sgTable(myPos)%value(2*DIM+1) = xiL
        sgTable(myPos)%value(2*DIM+2) = xiG
        sgTable(myPos)%value(2*DIM+3) = rhoL
        sgTable(myPos)%value(2*DIM+4) = rhoG
        sgTable(myPos)%value(2*DIM+5) = sL
        sgTable(myPos)%value(2*DIM+6:3*DIM+5) = local_v(1:DIM)
        sgTable(myPos)%value(3*DIM+6) = local_Cf

        sgTable(myPos)%surplus(1:DIM) = sgTable(myPos)%value(1:DIM) - xEst(1:DIM)
        sgTable(myPos)%surplus(DIM+1:2*DIM) = sgTable(myPos)%value(DIM+1:2*DIM) - yEst(1:DIM)
        sgTable(myPos)%surplus(2*DIM+1) = sgTable(myPos)%value(2*DIM+1) - xiLEst
        sgTable(myPos)%surplus(2*DIM+2) = sgTable(myPos)%value(2*DIM+2) - xiGEst
        sgTable(myPos)%surplus(2*DIM+3) = sgTable(myPos)%value(2*DIM+3) - rhoLEst
        sgTable(myPos)%surplus(2*DIM+4) = sgTable(myPos)%value(2*DIM+4) - rhoGEst
        sgTable(myPos)%surplus(2*DIM+5) = sgTable(myPos)%value(2*DIM+5) - sLEst
        sgTable(myPos)%surplus(2*DIM+6:3*DIM+5) = sgTable(myPos)%value(2*DIM+6:3*DIM+5) - local_vEst(1:DIM)
        sgTable(myPos)%surplus(3*DIM+6) = sgTable(myPos)%value(3*DIM+6) - local_CfEst

        sgTable(myPos)%stamp(1:SURPLUSSIZE) = stamptemp(1:SURPLUSSIZE)

        do i = 1, DIM
            sgTable(myPos)%lend(i) = 2.D0**(-thisNode%myID%le(i))*(thisNode%myID%li(i)-1)
            sgTable(myPos)%rend(i) = 2.D0**(-thisNode%myID%le(i))*(thisNode%myID%li(i)+1)
            sgTable(myPos)%coordinate(i) = 2.D0**(-thisNode%myID%le(i))*thisNode%myID%li(i)
        end do

    end subroutine pointSet

    function pointDomain(pID) result(isInRange)

        type(pointID), intent(in) :: pID
        logical :: isInRange
        real(kind=8) :: sum
        integer :: i

        sum = 0.D0
        do i = 2, DIM
            sum = sum + 2.D0**(-pID%le(i))*(pID%li(i)-1)
        end do
        if(sum <= 1.D0) then
            isInRange = .true.
        else
            isInRange = .false.
        end if

    end function pointDomain

    subroutine sgInterpo(P, z, xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)

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
        logical :: isInRange
        real(kind=8) :: ratio
        real(kind=8), dimension(1:SURPLUSSIZE) :: estArray
        integer :: i, j

        stdP = (P-PMIN)/(PMAX-PMIN)
        estArray(1:SURPLUSSIZE) = 0.D0

        do i = 1, tLayerTail
            isInRange = .true.
            if((stdP<sgTable(i)%lend(1)).or.(stdP>sgTable(i)%rend(1))) then
                isInRange = .false.
            else
                ratio = ((sgTable(i)%rend(1)-sgTable(i)%lend(1))/2.D0 - abs(stdP-sgTable(i)%coordinate(1))) &!
                    /((sgTable(i)%rend(1)-sgTable(i)%lend(1))/2.D0)
            end if
            if(isInRange) then
                do j = 2, DIM
                    if((z(j-1)<sgTable(i)%lend(j)).or.(z(j-1)>sgTable(i)%rend(j))) then
                        isInRange = .false.
                        exit
                    else
                        ratio = ratio * ((sgTable(i)%rend(j)-sgTable(i)%lend(j))/2.D0 - abs(z(j-1)-sgTable(i)%coordinate(j)))/ &!
                            ((sgTable(i)%rend(j)-sgTable(i)%lend(j))/2.D0)
                    end if
                end do
            end if
            if(isInRange) then
                do j = 1, SURPLUSSIZE
                    if(sgTable(i)%stamp(j)) then
                        estArray(j) = estArray(j) + sgTable(i)%surplus(j)*ratio
                    end if
                end do
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

    subroutine sgHierarchy(thisNode)

        type(queueNode), pointer, intent(inout) :: thisNode
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
        logical :: isInRange, isInsert
        integer :: myPos, hIndex
        type(queueNode), pointer :: childNode
        type(hashNode), pointer :: phn, newHashNode
        logical :: isCDInRange, isFindC
        real(kind=8) :: sum
        integer :: i, j

        allocate(z(1:DIM-1))
        allocate(local_z(1:DIM))
        allocate(x(1:DIM))
        allocate(y(1:DIM))
        allocate(local_v(1:DIM))
        allocate(xEst(1:DIM))
        allocate(yEst(1:DIM))
        allocate(local_vEst(1:DIM))
        allocate(stamptemp(1:SURPLUSSIZE))

        P = PMIN + pointLocate(thisNode%myID, 1)*(PMAX-PMIN)
        local_z(DIM) = 1.D0
        sum = 0.D0
        do i = 1, DIM-1
            z(i) = pointLocate(thisNode%myID, i+1)
            local_z(i) = z(i)
            local_z(DIM) = local_z(DIM) - local_z(i)
            sum = sum + z(i)
        end do
        if(sum > 1.D0) then
            isInRange = .false.
        else
            isInRange = .true.
        end if

        isRea = .false.
        if(isInRange) then
            call flashcalculation(P, local_z, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, isW, isN, isRea)
            !$OMP CRITICAL (TNUMF)
            TNumF = TNumF + 1
            !$OMP END CRITICAL (TNUMF)
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

        if(isInsert) then
            !$OMP CRITICAL (SG)
            myPos = pos
            if(myPos > TSIZE) then
                print *, 'The table size is not enough!'
                stop
            end if
            pos = pos + 1
            !$OMP END CRITICAL (SG)
            call pointSet(thisNode, myPos, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, stamptemp, &!
                xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)
        end if

        if(.not.(.not.isInsert.and.isRea.and.isInRange)) then
            if(pointLayer(thisNode%myID) < MAXLAYER) then
                do i = 1, DIM
                    do j = 1, 2
                        allocate(childNode)
                        childNode%myID = thisNode%myID
                        childNode%myID%le(i) = thisNode%myID%le(i) + 1
                        if(j == 1) then
                            childNode%myID%li(i) = 2*thisNode%myID%li(i) - 1
                        else
                            childNode%myID%li(i) = 2*thisNode%myID%li(i) + 1
                        end if
                        childNode%next => null()

                        isCDInRange = pointDomain(childNode%myID)
                        isFindC = .false.
                        if(isCDInRange) then
                            hIndex = hash(childNode%myID)
                            phn => hashTable(hIndex)
                            call OMP_set_lock(lock(hIndex))
                            do while(associated(phn))
                                if(pointEqual(phn%myID,childNode%myID)) then
                                    isFindC = .true.
                                    exit
                                elseif(associated(phn%next)) then
                                    phn => phn%next
                                elseif(phn%myID%le(1) == 0) then
                                    phn%myID = childNode%myID
                                    exit
                                else
                                    allocate(newHashNode)
                                    newHashNode%myID = childNode%myID
                                    newHashNode%next => null()
                                    phn%next => newHashNode
                                    exit
                                end if
                            end do
                            call OMP_unset_lock(lock(hIndex))
                        end if

                        if(isCDInRange.and..not.isFindC) then
                            !$OMP CRITICAL (QUEUE)
                            if(.not.associated(qHead)) then
                                qHead => childNode
                                qTail => childNode
                            else
                                qTail%next => childNode
                                qTail => childNode
                            end if
                            !$OMP END CRITICAL (QUEUE)
                        else
                            deallocate(childNode)
                        end if
                    end do
                end do
            end if
        end if

        deallocate(thisNode)

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

        character(len=2) :: charm
        character(len=50) :: funiontxt, fxiltxt, fxigtxt, frholtxt, frhogtxt, fsltxt, fcftxt
        character(len=50), dimension(:), pointer :: fxtxt, fytxt, fvtxt
        logical :: alive
        integer :: i, j, k

        inquire(file = trim(adjustl(soluDoc))//'/sg', exist = alive)
        if(.not.alive) then
            call system("mkdir "//trim(adjustl(soluDoc))//'/sg')
        end if

        funiontxt = trim(adjustl(soluDoc))//'/sg/union.txt'
        fxiltxt = trim(adjustl(soluDoc))//'/sg/xiL.txt'
        fxigtxt = trim(adjustl(soluDoc))//'/sg/xiG.txt'
        frholtxt = trim(adjustl(soluDoc))//'/sg/rhoL.txt'
        frhogtxt = trim(adjustl(soluDoc))//'/sg/rhoG.txt'
        fsltxt = trim(adjustl(soluDoc))//'/sg/sL.txt'
        fcftxt = trim(adjustl(soluDoc))//'/sg/Cf.txt'
        allocate(fxtxt(1:DIM))
        allocate(fytxt(1:DIM))
        allocate(fvtxt(1:DIM))
        do i = 1, DIM
            write(charm,'(i2)') i
            fxtxt(i) = trim(adjustl(soluDoc))//'/sg/x'//trim(adjustl(charm))//'.txt'
            fytxt(i) = trim(adjustl(soluDoc))//'/sg/y'//trim(adjustl(charm))//'.txt'
            fvtxt(i) = trim(adjustl(soluDoc))//'/sg/v'//trim(adjustl(charm))//'.txt'
        end do

        do i = 1, DIM
            open(unit=10+i, file=trim(adjustl(fxtxt(i))), status='replace')
            open(unit=10+DIM+i, file=trim(adjustl(fytxt(i))), status='replace')
            open(unit=15+2*DIM+i, file=trim(adjustl(fvtxt(i))), status='replace')
        end do
        open(unit=10, file=trim(adjustl(funiontxt)), status='replace')
        open(unit=11+2*DIM, file=trim(adjustl(fxiltxt)), status='replace')
        open(unit=12+2*DIM, file=trim(adjustl(fxigtxt)), status='replace')
        open(unit=13+2*DIM, file=trim(adjustl(frholtxt)), status='replace')
        open(unit=14+2*DIM, file=trim(adjustl(frhogtxt)), status='replace')
        open(unit=15+2*DIM, file=trim(adjustl(fsltxt)), status='replace')
        open(unit=16+3*DIM, file=trim(adjustl(fcftxt)), status='replace')

        do i = 1, pos-1
            do j = 1, DIM
                write(10, fmt="(es15.8)") sgTable(i)%coordinate(j)
            end do
            do j = 1, SURPLUSSIZE
                if(sgTable(i)%stamp(j)) then
                    do k = 1, DIM
                        write(10+j, fmt="(es15.8)") sgTable(i)%coordinate(k)
                    end do
                end if
            end do
        end do

        deallocate(fxtxt)
        deallocate(fytxt)
        deallocate(fvtxt)

        do i = 1, DIM
            close(10+i)
            close(10+DIM+i)
            close(15+2*DIM+i)
        end do
        close(10)
        close(11+2*DIM)
        close(12+2*DIM)
        close(13+2*DIM)
        close(14+2*DIM)
        close(15+2*DIM)
        close(16+3*DIM)

        print *, 'The total number of points is ', pos-1
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
        real(kind=8) :: fStartTime, fEndTime, iStartTime, iEndTime
        integer :: i

        if(dIndex == DIM+1) then
            allocate(z(1:DIM-1))
            allocate(local_z(1:DIM))

            P = PMIN + (PMAX-PMIN)*sgTestArray(1)/(NUMSAMPLE+1)
            local_z(DIM) = 1.D0
            sum = 0.D0
            do i = 1, DIM-1
                z(i) = sgTestArray(i+1)*1.D0/(NUMSAMPLE+1)
                local_z(i) = z(i)
                local_z(DIM) = local_z(DIM) - local_z(i)
                sum = sum + z(i)
            end do
            if(sum > 1) then
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

                fStartTime = OMP_get_wtime()
                call flashcalculation(P, local_z, x, y, &!
                    xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, isW, isN, isRea)
                fendTime = OMP_get_wtime()
                if(isRea) then
                    iStartTime = OMP_get_wtime()
                    call sgInterpo(P, z, xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, &!
                        sLEst, local_vEst, local_CfEst)
                    iEndTime = OMP_get_wtime()
                    fTimeSum = fTimeSum + fEndTime - fStartTime
                    iTimeSum = iTimeSum + iEndTime - iStartTime
                    TNumC = TNumC + 1

write(188, fmt="(es15.8)") sL
write(189, fmt="(es15.8)") sLEst

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

        do while(sgTestArray(dIndex) <= NUMSAMPLE)
            call sgTest(dIndex+1)
            sgTestArray(dIndex) = sgTestArray(dIndex) + 1
            if((dIndex+1) <= DIM) then
                sgTestArray(dIndex+1) = 1
            end if
        end do

    end subroutine sgTest

    subroutine sgTestDriver()

        character(len=2) :: charm
        character(len=50) :: fxiltxt, fxigtxt, frholtxt, frhogtxt, fsltxt, fcftxt
character(len=50) :: slf, sls
        character(len=50), dimension(:), pointer :: fxtxt, fytxt, fvtxt
        logical :: alive
        integer :: i

        inquire(file = trim(adjustl(soluDoc))//'/error', exist = alive)
        if(.not.alive) then
            call system("mkdir "//trim(adjustl(soluDoc))//'/error')
        end if

        fxiltxt = trim(adjustl(soluDoc))//'/error/xiL.txt'
        fxigtxt = trim(adjustl(soluDoc))//'/error/xiG.txt'
        frholtxt = trim(adjustl(soluDoc))//'/error/rhoL.txt'
        frhogtxt = trim(adjustl(soluDoc))//'/error/rhoG.txt'
        fsltxt = trim(adjustl(soluDoc))//'/error/sL.txt'
        fcftxt = trim(adjustl(soluDoc))//'/error/Cf.txt'
slf = trim(adjustl(soluDoc))//'/slf.txt'
sls = trim(adjustl(soluDoc))//'/sls.txt'
        allocate(fxtxt(1:DIM))
        allocate(fytxt(1:DIM))
        allocate(fvtxt(1:DIM))
        do i = 1, DIM
            write(charm,'(i2)') i
            fxtxt(i) = trim(adjustl(soluDoc))//'/error/x'//trim(adjustl(charm))//'.txt'
            fytxt(i) = trim(adjustl(soluDoc))//'/error/y'//trim(adjustl(charm))//'.txt'
            fvtxt(i) = trim(adjustl(soluDoc))//'/error/v'//trim(adjustl(charm))//'.txt'
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
open(unit=188, file=trim(adjustl(slf)), status='replace')
open(unit=189, file=trim(adjustl(sls)), status='replace')

        fTimeSum = 0.D0
        iTimeSum = 0.D0
        TNumC = 0
        sgTestArray(1:DIM) = 1
        call sgTest(1)
        print *, 'There are ', TNumC, ' comparisons totally.'
        print *, 'Average flash time = ', fTimeSum/TNumC, ' seconds.'
        print *, 'Average sparse grid interpolation time = ', iTimeSum/TNumC, ' seconds.'

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
close(188)
close(189)

        deallocate(fxtxt)
        deallocate(fytxt)
        deallocate(fvtxt)

    end subroutine sgTestDriver

    subroutine sgDriver()

        character(len=50) :: fprimetxt
        type(queueNode), pointer :: godNode, thisNode
        logical :: alive, isWait, isContin
        real(kind=8) :: startTime, endTime
        integer :: curLayer, ierr, i

        inquire(file = soluDoc, exist = alive)
        if(.not.alive) then
            call system("mkdir "//trim(adjustl(soluDoc)))
        end if

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
            call OMP_init_lock(lock(i))
        end do

        TNumF = 0
        do i = 1, TSIZE
            sgTable(i)%myID%le(1:DIM) = 0
            sgTable(i)%myID%li(1:DIM) = 0
        end do
        pos = 1
        tLayerTail = 0

        allocate(godNode)
        godNode%myID%le(1:DIM) = 1
        godNode%myID%li(1:DIM) = 1
        godNode%next => null()
        qHead => godNode
        qTail => godNode

        curLayer = 1
        isContin = .true.
        call OMP_set_num_threads(NUMTHREADS)
        startTime = OMP_get_wtime()
        !$OMP PARALLEL PRIVATE(thisNode, isWait)
        do while(isContin)
            !$OMP CRITICAL (QUEUE)
            if((associated(qHead)).and.(pointLayer(qHead%myID)/=curLayer)) then
                isWait = .true.
            elseif((associated(qHead)).and.(pointLayer(qHead%myID)==curLayer)) then
                isWait = .false.
                thisNode => qHead
                if(associated(qHead%next)) then
                    qHead => qHead%next
                else
                    qHead => null()
                end if
            elseif(.not.associated(qHead)) then
                isWait = .true.
            end if
            !$OMP END CRITICAL (QUEUE)
            if(isWait) then
                !$OMP BARRIER
                if(.not.associated(qHead)) then
                    isContin = .false.
                else
                    !$OMP SINGLE
                    tLayerTail = pos - 1
                    curLayer = curLayer + 1
                    print *, 'Current Layer = ', curLayer
                    call hashTableClean()
                    !$OMP END SINGLE
                end if
            else
                call sgHierarchy(thisNode)
            end if
        end do
        !$OMP END PARALLEL
        endTime = OMP_get_wtime()
        print *, 'The time to construct sparse grids is ', endTime-startTime, ' seconds.'
        call sgOutput()
        call sgTestDriver()

        do i = 1, HSIZE
            call OMP_destroy_lock(lock(i))
        end do

    end subroutine sgDriver

end module RST_sg
