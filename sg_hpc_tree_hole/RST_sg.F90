
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

    subroutine sgInitialize()

        character(len=2) :: charm
        character(len=50) :: funiontxt, fxiltxt, fxigtxt, frholtxt, frhogtxt, fsltxt, fcftxt
        character(len=50), dimension(:), pointer :: fxtxt, fytxt, fvtxt
        logical :: alive
        integer :: i

        inquire(file = soluDoc, exist = alive)
        if(.not.alive) then
            call system("mkdir "//trim(adjustl(soluDoc)))
        end if

        ! files to store sparse grid points
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

        ! files to store errors
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
            open(unit=60+i, file=trim(adjustl(fxtxt(i))), status='replace')
            open(unit=60+DIM+i, file=trim(adjustl(fytxt(i))), status='replace')
            open(unit=65+2*DIM+i, file=trim(adjustl(fvtxt(i))), status='replace')
        end do
        open(unit=61+2*DIM, file=trim(adjustl(fxiltxt)), status='replace')
        open(unit=62+2*DIM, file=trim(adjustl(fxigtxt)), status='replace')
        open(unit=63+2*DIM, file=trim(adjustl(frholtxt)), status='replace')
        open(unit=64+2*DIM, file=trim(adjustl(frhogtxt)), status='replace')
        open(unit=65+2*DIM, file=trim(adjustl(fsltxt)), status='replace')
        open(unit=66+3*DIM, file=trim(adjustl(fcftxt)), status='replace')

        deallocate(fxtxt)
        deallocate(fytxt)
        deallocate(fvtxt)

    end subroutine sgInitialize

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

    subroutine pointSet(thisNode, newPoint, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, &!
        stamptemp, xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)

        type(queueNode), pointer, intent(in) :: thisNode
        type(point), pointer, intent(in out) :: newPoint
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
        integer :: i, j

        newPoint%myID = thisNode%myID

        newPoint%value(1:DIM) = x(1:DIM)
        newPoint%value(DIM+1:2*DIM) = y(1:DIM)
        newPoint%value(2*DIM+1) = xiL
        newPoint%value(2*DIM+2) = xiG
        newPoint%value(2*DIM+3) = rhoL
        newPoint%value(2*DIM+4) = rhoG
        newPoint%value(2*DIM+5) = sL
        newPoint%value(2*DIM+6:3*DIM+5) = local_v(1:DIM)
        newPoint%value(3*DIM+6) = local_Cf

        newPoint%surplus(1:DIM) = newPoint%value(1:DIM) - xEst(1:DIM)
        newPoint%surplus(DIM+1:2*DIM) = newPoint%value(DIM+1:2*DIM) - yEst(1:DIM)
        newPoint%surplus(2*DIM+1) = newPoint%value(2*DIM+1) - xiLEst
        newPoint%surplus(2*DIM+2) = newPoint%value(2*DIM+2) - xiGEst
        newPoint%surplus(2*DIM+3) = newPoint%value(2*DIM+3) - rhoLEst
        newPoint%surplus(2*DIM+4) = newPoint%value(2*DIM+4) - rhoGEst
        newPoint%surplus(2*DIM+5) = newPoint%value(2*DIM+5) - sLEst
        newPoint%surplus(2*DIM+6:3*DIM+5) = newPoint%value(2*DIM+6:3*DIM+5) - local_vEst(1:DIM)
        newPoint%surplus(3*DIM+6) = newPoint%value(3*DIM+6) - local_CfEst

        newPoint%stamp(1:SURPLUSSIZE) = stamptemp(1:SURPLUSSIZE)

        do i = 1, DIM
            newPoint%lend(i) = 2.D0**(-thisNode%myID%le(i))*(thisNode%myID%li(i)-1)
            newPoint%rend(i) = 2.D0**(-thisNode%myID%le(i))*(thisNode%myID%li(i)+1)
            newPoint%coordinate(i) = 2.D0**(-thisNode%myID%le(i))*thisNode%myID%li(i)
        end do

        do i = 1, 2*DIM
            newPoint%pChildArray(i)%pp => null()
        end do

        newPoint%isInSG = .true.

        do i = 1, DIM
            write(10, fmt="(es15.8)") newPoint%coordinate(i)
        end do
        do i = 1, SURPLUSSIZE
            if(newPoint%stamp(i)) then
                do j = 1, DIM
                    write(10+i, fmt="(es15.8)") newPoint%coordinate(j)
                end do
            end if
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

    ! Link the points on the initial layer together
    recursive subroutine iniLayerLink(pPoint)

        type(point), pointer, intent(in out) :: pPoint
        type(iniLayerPoint), pointer :: pilp
        integer :: myLayer, i

        myLayer = pointLayer(pPoint%myID)
        if((myLayer==iniLayer).and.(.not.pPoint%isInSG)) then
            allocate(pilp)
            pilp%pp => pPoint
            pilp%next => null()
            !$OMP CRITICAL (INITIALLAYER)
            ilqTail%next => pilp
            ilqTail => pilp
            !$OMP END CRITICAL (INITIALLAYER)
        elseif(myLayer < iniLayer) then
            do i = 1, 2*DIM
                if(associated(pPoint%pChildArray(i)%pp)) then
                    call iniLayerLink(pPoint%pChildArray(i)%pp)
                end if
            end do
            deallocate(pPoint) ! Remove the ghost points above the initial layer
        end if

    end subroutine iniLayerLink

    recursive subroutine sgSubInterpo(P, z, pPoint, estArray)

        real(kind=8), intent(in) :: P
        real(kind=8), dimension(:), pointer, intent(in) :: z
        type(point), pointer, intent(in) :: pPoint
        real(kind=8), dimension(:), pointer, intent(in out) :: estArray
        real(kind=8) :: stdP
        logical :: isInRange
        real(kind=8) :: ratio
        type(point), pointer :: childPoint
        integer :: i

        stdP = (P-PMIN)/(PMAX-PMIN)
        if(pPoint%isInSG) then
            isInRange = .true.
            if((stdP<pPoint%lend(1)).or.(stdP>pPoint%rend(1))) then
                isInRange = .false.
            else
                ratio = ((pPoint%rend(1)-pPoint%lend(1))/2.D0 - abs(stdP-pPoint%coordinate(1))) &!
                    /((pPoint%rend(1)-pPoint%lend(1))/2.D0)
            end if
            if(isInRange) then
                do i = 2, DIM
                    if((z(i-1)<pPoint%lend(i)).or.(z(i-1)>pPoint%rend(i))) then
                        isInRange = .false.
                        exit
                    else
                        ratio = ratio * ((pPoint%rend(i)-pPoint%lend(i))/2.D0 - &!
                            abs(z(i-1)-pPoint%coordinate(i)))/ ((pPoint%rend(i)-pPoint%lend(i))/2.D0)
                    end if
                end do
            end if
            if(isInRange) then
                do i = 1, SURPLUSSIZE
                    if(pPoint%stamp(i)) then
                        estArray(i) = estArray(i) + pPoint%surplus(i)*ratio
                    end if
                end do
            end if
        end if

        childPoint => pPoint%pChildArray(1)%pp
        if(associated(childPoint).and.((childPoint%isInSG.and.(stdP>=childPoint%lend(1)).and. &!
            (stdP<=childPoint%rend(1))).or.(.not.childPoint%isInSG))) then
            call sgSubInterpo(P, z, childPoint, estArray)
        else
            childPoint => pPoint%pChildArray(2)%pp
            if(associated(childPoint).and.((childPoint%isInSG.and.(stdP>=childPoint%lend(1)).and. &!
                (stdP<=childPoint%rend(1))).or.(.not.childPoint%isInSG))) then
                call sgSubInterpo(P, z, childPoint, estArray)
            end if
        end if
        do i = 2, DIM
            childPoint => pPoint%pChildArray(2*i-1)%pp
            if(associated(childPoint).and.((childPoint%isInSG.and.(z(i-1)>=childPoint%lend(i)).and. &!
                (z(i-1)<=childPoint%rend(i))).or.(.not.childPoint%isInSG))) then
                call sgSubInterpo(P, z, childPoint, estArray)
            else
                childPoint => pPoint%pChildArray(2*i)%pp
                if(associated(childPoint).and.((childPoint%isInSG.and.(z(i-1)>=childPoint%lend(i)).and. &!
                    (z(i-1)<=childPoint%rend(i))).or.(.not.childPoint%isInSG))) then
                    call sgSubInterpo(P, z, childPoint, estArray)
                end if
            end if
        end do

    end subroutine sgSubInterpo

    subroutine sgInterpo(P, z, estArray)

        real(kind=8), intent(in) :: P
        real(kind=8), dimension(:), pointer, intent(in) :: z
        real(kind=8), dimension(:), pointer, intent(in out) :: estArray
        type(iniLayerPoint), pointer :: pilp

        estArray(1:SURPLUSSIZE) = 0.D0
        pilp => ilqHead
        do while(associated(pilp))
            call sgSubInterpo(P, z, pilp%pp, estArray)
            pilp => pilp%next
        end do

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
        real(kind=8), dimension(:), pointer :: estArray
        logical, dimension(:), pointer :: stamptemp
        logical :: isInRange, isInsert
        type(point), pointer :: newPoint
        type(iniLayerPoint), pointer :: newILPoint
        type(queueNode), pointer :: childNode
        type(hashNode), pointer :: phn, newHashNode
        logical :: isCDInRange, isFindC
        integer :: hIndex
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
            call flashcalculation(P, local_z, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, &!
                local_Cf, isW, isN, isRea)
        end if

        isInsert = .false.
        if(isRea) then
            if(associated(ilqHead).and.(curLayer>iniLayer)) then
                allocate(estArray(1:SURPLUSSIZE))
                call sgInterpo(P, z, estArray)
                xEst(1:DIM) = estArray(1:DIM)
                yEst(1:DIM) = estArray(DIM+1:2*DIM)
                xiLEst = estArray(2*DIM+1)
                xiGEst = estArray(2*DIM+2)
                rhoLEst = estArray(2*DIM+3)
                rhoGEst = estArray(2*DIM+4)
                sLEst = estArray(2*DIM+5)
                local_vEst(1:DIM) = estArray(2*DIM+6:3*DIM+5)
                local_CfEst = estArray(3*DIM+6)
                deallocate(estArray)
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
            else
                xEst(1:DIM) = 0.D0
                yEst(1:DIM) = 0.D0
                xiLEst = 0.D0
                xiGEst = 0.D0
                rhoLEst = 0.D0
                rhoGEst = 0.D0
                sLEst = 0.D0
                local_vEst(1:DIM) = 0.D0
                local_CfEst = 0.D0
                stamptemp(1:SURPLUSSIZE) = .true.
                isInsert = .true.
            end if
        end if

        if(.not.(.not.isInsert.and.isRea.and.isInRange)) then ! not leaf
            allocate(newPoint)
            if(curLayer == 1) then
                godPoint => newPoint
            end if
            if(isInsert) then ! sparse grid point
                call pointSet(thisNode, newPoint, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, &!
                    stamptemp, xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)
                !$OMP CRITICAL (INITIALLAYER)
                tNumP = tNumP + 1
                if(.not.associated(ilqHead)) then
                    allocate(newILPoint)
                    newILPoint%pp => newPoint
                    newILPoint%next => null()
                    ilqHead => newILPoint
                    ilqTail => newILPoint
                    iniLayer = pointLayer(newPoint%myID)
                elseif(iniLayer == pointLayer(newPoint%myID)) then
                    allocate(newILPoint)
                    newILPoint%pp => newPoint
                    newILPoint%next => null()
                    ilqTail%next => newILPoint
                    ilqTail => newILPoint
                end if
                !$OMP END CRITICAL (INITIALLAYER)
            else ! ghost point
                newPoint%myID = thisNode%myID
                do i = 1, 2*DIM
                    newPoint%pChildArray(i)%pp => null()
                end do
                newPoint%isInSG = .false.
            end if
            if(associated(thisNode%parent)) then
                thisNode%parent%pChildArray(thisNode%pCAIndex)%pp => newPoint
            end if

            if(pointLayer(newPoint%myID) < MAXLAYER) then
                do i = 1, DIM
                    do j = 1, 2
                        allocate(childNode)
                        childNode%myID = newPoint%myID
                        childNode%myID%le(i) = newPoint%myID%le(i) + 1
                        if(j == 1) then
                            childNode%myID%li(i) = 2*newPoint%myID%li(i) - 1
                        else
                            childNode%myID%li(i) = 2*newPoint%myID%li(i) + 1
                        end if
                        childNode%parent => newPoint
                        if(j == 1) then
                            childNode%pCAIndex = 2*i - 1
                        else
                            childNode%pCAIndex = 2*i
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

    recursive subroutine sgClean(pPoint)

        type(point), pointer, intent(in out) :: pPoint
        integer :: i

        do i = 1, 2*DIM
            if(associated(pPoint%pChildArray(i)%pp)) then
                call sgClean(pPoint%pChildArray(i)%pp)
            end if
        end do

        deallocate(pPoint)

    end subroutine sgClean

    subroutine sgHierarchyDriver()

        character(len=50) :: fprimetxt
        type(queueNode), pointer :: godNode, thisNode
        logical :: isWait, isContin
        real(kind=8) :: hStartTime, hEndTime
        integer :: ierr, i

        if(430*(DIM-1) > PRIMESIZE+1) then
            print *, 'The prime table is not big enough!'
            print *, 'At least ', 430*(DIM-1)-1, ' prime numbers are needed!'
            stop
        end if

        !call genPrime(PRIMESIZE)
        fprimetxt = 'prime.txt'
        open(unit=210, file=trim(adjustl(fprimetxt)), iostat=ierr)
        if(ierr /= 0) then
            print *, 'open file error. ', ierr
            stop
        end if
        read(210, fmt="(i6)") prime(:)
        close(210)

        do i = 1, HSIZE
            hashTable(i)%myID%le(1:DIM) = 0
            hashTable(i)%myID%li(1:DIM) = 0
            hashTable(i)%next => null()
            call OMP_init_lock(lock(i))
        end do

        ilqHead => null()

        allocate(godNode)
        godNode%myID%le(1:DIM) = 1
        godNode%myID%li(1:DIM) = 1
        godNode%next => null()
        godNode%parent => null()
        godNode%pCAIndex = 0
        qHead => godNode
        qTail => godNode

        tNumP = 0
        curLayer = 1
        isContin = .true.

        call OMP_set_num_threads(NUMTHREADS)
        hStartTime = OMP_get_wtime()
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
                    curLayer = curLayer + 1
                    print *, 'Current Layer = ', curLayer
                    call hashTableClean()
                    if(curLayer == iniLayer+1) then
                        call iniLayerLink(godPoint)
                    end if
                    !$OMP END SINGLE
                end if
            else
                call sgHierarchy(thisNode)
            end if
        end do

        !$OMP END PARALLEL
        hEndTime = OMP_get_wtime()
        print *, 'The time to construct sparse grids is ', hEndTime-hStartTime, ' seconds.'
        print *, 'The total number of points is ', tNumP

        do i = 1, HSIZE
            call OMP_destroy_lock(lock(i))
        end do

    end subroutine sgHierarchyDriver

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
        real(kind=8), dimension(:), pointer :: estArray
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
                fEndTime = OMP_get_wtime()

                if(isRea) then
                    allocate(estArray(1:SURPLUSSIZE))
                    iStartTime = OMP_get_wtime()
                    call sgInterpo(P, z, estArray)
                    iEndTime = OMP_get_wtime()
                    fTimeSum = fTimeSum + fEndTime - fStartTime
                    iTimeSum = iTimeSum + iEndTime - iStartTime
                    tNumC = tNumC + 1

                    xEst(1:DIM) = estArray(1:DIM)
                    yEst(1:DIM) = estArray(DIM+1:2*DIM)
                    xiLEst = estArray(2*DIM+1)
                    xiGEst = estArray(2*DIM+2)
                    rhoLEst = estArray(2*DIM+3)
                    rhoGEst = estArray(2*DIM+4)
                    sLEst = estArray(2*DIM+5)
                    local_vEst(1:DIM) = estArray(2*DIM+6:3*DIM+5)
                    local_CfEst = estArray(3*DIM+6)
                    deallocate(estArray)

                    do i = 1, DIM
                        if(abs(x(i)) < EQUALPREC) then
                            write(60+i, fmt="(es15.8)") 0.D0
                        else
                            write(60+i, fmt="(es15.8)") abs((xEst(i)-x(i))/x(i))
                        end if
                        if(abs(y(i)) < EQUALPREC) then
                            write(60+DIM+i, fmt="(es15.8)") 0.D0
                        else
                            write(60+DIM+i, fmt="(es15.8)") abs((yEst(i)-y(i))/y(i))
                        end if
                        if(abs(local_v(i)) < EQUALPREC) then
                            write(65+2*DIM+i, fmt="(es15.8)") 0.D0
                        else
                            write(65+2*DIM+i, fmt="(es15.8)") abs((local_vEst(i)-local_v(i))/local_v(i))
                        end if
                    end do
                    if(abs(xiL) < EQUALPREC) then
                        write(61+2*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(61+2*DIM, fmt="(es15.8)") abs((xiLEst-xiL)/xiL)
                    end if
                    if(abs(xiG) < EQUALPREC) then
                        write(62+2*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(62+2*DIM, fmt="(es15.8)") abs((xiGEst-xiG)/xiG)
                    end if
                    if(abs(rhoL) < EQUALPREC) then
                        write(63+2*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(63+2*DIM, fmt="(es15.8)") abs((rhoLEst-rhoL)/rhoL)
                    end if
                    if(abs(rhoG) < EQUALPREC) then
                        write(64+2*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(64+2*DIM, fmt="(es15.8)") abs((rhoGEst-rhoG)/rhoG)
                    end if
                    if(abs(sL) < EQUALPREC) then
                        write(65+2*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(65+2*DIM, fmt="(es15.8)") abs((sLEst-sL)/sL)
                    end if
                    if(abs(local_Cf) < EQUALPREC) then
                        write(66+3*DIM, fmt="(es15.8)") 0.D0
                    else
                        write(66+3*DIM, fmt="(es15.8)") abs((local_CfEst-local_Cf)/local_Cf)
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

        fTimeSum = 0.D0
        iTimeSum = 0.D0
        tNumC = 0
        sgTestArray(1:DIM) = 1
        call sgTest(1)
        print *, 'There are ', tNumC, ' comparisons totally.'
        print *, 'Average flash time = ', fTimeSum/tNumC, ' seconds.'
        print *, 'Average sparse grid interpolation time = ', iTimeSum/tNumC, ' seconds.'

    end subroutine sgTestDriver

    subroutine sgFinalize()

        type(iniLayerPoint), pointer :: pilp1, pilp2
        integer :: i

        pilp1 => ilqHead
        do while(associated(pilp1))
            call sgClean(pilp1%pp)
            pilp1 => pilp1%next
        end do

        pilp1 => ilqHead
        do while(associated(pilp1))
            pilp2 => pilp1
            pilp1 => pilp1%next
            deallocate(pilp2)
        end do

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

        do i = 1, DIM
            close(60+i)
            close(60+DIM+i)
            close(65+2*DIM+i)
        end do
        close(61+2*DIM)
        close(62+2*DIM)
        close(63+2*DIM)
        close(64+2*DIM)
        close(65+2*DIM)
        close(66+3*DIM)

    end subroutine sgFinalize

    subroutine sgDriver()

        call sgInitialize()

        call sgHierarchyDriver()

        call sgTestDriver()

        call sgFinalize()

    end subroutine sgDriver

end module RST_sg
