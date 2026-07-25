
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

    function locate(pID, dimN) result(coord)

        type(pointID), intent(in) :: pID
        integer, intent(in) :: dimN
        real(kind=8) :: coord

        coord = 2.D0**(-pID%le(dimN))*pID%li(dimN)

    end function locate

    subroutine setPoint(pc, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, stamptemp, &!
        xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)

        type(point), pointer, intent(inout) :: pc
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

        do i = 1, DIM
            pc%myID%le(i) = head%myID%le(i)
            pc%myID%li(i) = head%myID%li(i)
        end do

        pc%value(1:DIM) = x(1:DIM)
        pc%value(DIM+1:2*DIM) = y(1:DIM)
        pc%value(2*DIM+1) = xiL
        pc%value(2*DIM+2) = xiG
        pc%value(2*DIM+3) = rhoL
        pc%value(2*DIM+4) = rhoG
        pc%value(2*DIM+5) = sL
        pc%value(2*DIM+6:3*DIM+5) = local_v(1:DIM)
        pc%value(3*DIM+6) = local_Cf

        pc%surplus(1:DIM) = pc%value(1:DIM) - xEst(1:DIM)
        pc%surplus(DIM+1:2*DIM) = pc%value(DIM+1:2*DIM) - yEst(1:DIM)
        pc%surplus(2*DIM+1) = pc%value(2*DIM+1) - xiLEst
        pc%surplus(2*DIM+2) = pc%value(2*DIM+2) - xiGEst
        pc%surplus(2*DIM+3) = pc%value(2*DIM+3) - rhoLEst
        pc%surplus(2*DIM+4) = pc%value(2*DIM+4) - rhoGEst
        pc%surplus(2*DIM+5) = pc%value(2*DIM+5) - sLEst
        pc%surplus(2*DIM+6:3*DIM+5) = pc%value(2*DIM+6:3*DIM+5) - local_vEst(1:DIM)
        pc%surplus(3*DIM+6) = pc%value(3*DIM+6) - local_CfEst

        pc%stamp(1:SURPLUSSIZE) = stamptemp(1:SURPLUSSIZE)

        do i = 1, DIM
            pc%lend(i) = 2.D0**(-head%myID%le(i))*(head%myID%li(i)-1)
            pc%rend(i) = 2.D0**(-head%myID%le(i))*(head%myID%li(i)+1)
            pc%coordinate(i) = 2.D0**(-head%myID%le(i))*head%myID%li(i)
        end do

        pc%next => null()

    end subroutine setPoint

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
        logical :: isInRange, isInSG, isInsert
        integer :: hIndex
        type(point), pointer :: pb, pc
        type(queueNode), pointer :: lChild, rChild, premove
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

        P = PMIN + locate(head%myID, 1)*(PMAX-PMIN)
        local_z(DIM) = 1.D0
        sum = 0.D0
        do i = 1, DIM-1
            z(i) = locate(head%myID, i+1)
            local_z(i) = z(i)
            local_z(DIM) = local_z(DIM) - local_z(i)
            sum = sum + z(i)
        end do
        if(sum > 1.D0) then
            isInRange = .false.
        else
            isInRange = .true.
        end if

        isInSG = .true.
        if(isInRange) then
            hIndex = hash(head%myID)
            pc => hashTable(hIndex)
            do while(.true.)
                if(.not.(pointEqual(pc%myID, head%myID))) then
                    if(associated(pc%next)) then
                        pc => pc%next
                    else
                        isInSG = .false.
                        exit
                    end if
                else
                    exit
                end if
            end do
        end if

        isRea = .false.
        if(.not.isInSG) then
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

        if(isInsert) then
            hIndex = hash(head%myID)
            if(hashTable(hIndex)%myID%le(1) == 0) then
                pc => hashTable(hIndex)
                call setPoint(pc, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, stamptemp, &!
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
                call setPoint(pc, x, y, xiL, xiG, rhoL, rhoG, sL, local_v, local_Cf, stamptemp, &!
                    xEst, yEst, xiLEst, xiGEst, rhoLEst, rhoGEst, sLEst, local_vEst, local_CfEst)
            end if
        end if

        if(.not.(.not.isInsert.and.isRea.and.isInRange)) then
            if(getLayer(head%myID) < MAXLAYER) then
                do i = 1, DIM
                    allocate(lChild)
                    allocate(rChild)
                    do j = 1, DIM
                        lChild%myID%le(j) = head%myID%le(j)
                        lChild%myID%li(j) = head%myID%li(j)
                        rChild%myID%le(j) = head%myID%le(j)
                        rChild%myID%li(j) = head%myID%li(j)
                    end do
                    lChild%myID%le(i) = head%myID%le(i) + 1
                    lChild%myID%li(i) = 2*head%myID%li(i) - 1
                    rChild%myID%le(i) = head%myID%le(i) + 1
                    rChild%myID%li(i) = 2*head%myID%li(i) + 1
                    rChild%next => null()
                    ! add the children to the queue
                    tail%next => lChild
                    lChild%next => rChild
                    tail => rChild
                end do
            end if
        end if

        ! remove itself from the queue
        if(associated(head%next)) then
            premove => head
            head => head%next
            deallocate(premove)
        else
            deallocate(head)
            head => null()
            tail => null()
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

        type(point), pointer :: pc
        integer :: hIndex
        character(len=2) :: charm
        character(len=50) :: funiontxt, fxiltxt, fxigtxt, frholtxt, frhogtxt, fsltxt, fcftxt
        character(len=50), dimension(:), pointer :: fxtxt, fytxt, fvtxt
        logical :: alive
        integer :: i, j

        inquire(file = trim(adjustl(sgSoluDoc))//'/sg', exist = alive)
        if(.not.alive) then
            call system("mkdir "//trim(adjustl(sgSoluDoc))//'/sg')
        end if

        funiontxt = trim(adjustl(sgSoluDoc))//'/sg/union.txt'
        fxiltxt = trim(adjustl(sgSoluDoc))//'/sg/xiL.txt'
        fxigtxt = trim(adjustl(sgSoluDoc))//'/sg/xiG.txt'
        frholtxt = trim(adjustl(sgSoluDoc))//'/sg/rhoL.txt'
        frhogtxt = trim(adjustl(sgSoluDoc))//'/sg/rhoG.txt'
        fsltxt = trim(adjustl(sgSoluDoc))//'/sg/sL.txt'
        fcftxt = trim(adjustl(sgSoluDoc))//'/sg/Cf.txt'
        allocate(fxtxt(1:DIM))
        allocate(fytxt(1:DIM))
        allocate(fvtxt(1:DIM))
        do i = 1, DIM
            write(charm,'(i2)') i
            fxtxt(i) = trim(adjustl(sgSoluDoc))//'/sg/x'//trim(adjustl(charm))//'.txt'
            fytxt(i) = trim(adjustl(sgSoluDoc))//'/sg/y'//trim(adjustl(charm))//'.txt'
            fvtxt(i) = trim(adjustl(sgSoluDoc))//'/sg/v'//trim(adjustl(charm))//'.txt'
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

        hIndex = 1
        pc => hashTable(1)
        do while(hIndex <= HSIZE)
            if(pc%myID%le(1) /= 0) then
                do i = 1, DIM
                    write(10, fmt="(es15.8)") pc%coordinate(i)
                end do
                do i = 1, SURPLUSSIZE
                    if(pc%stamp(i)) then
                        do j = 1, DIM
                            write(10+i, fmt="(es15.8)") pc%coordinate(j)
                        end do
                    end if
                end do
            end if
            if(associated(pc%next)) then
                pc => pc%next
            else
                hIndex = hIndex + 1
                if(hIndex <= HSIZE) then
                    pc => hashTable(hIndex)
                end if
            end if
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

        print *, 'The times to do flash calculations are ', TNumF

    end subroutine sgOutput

    subroutine sgTraverse()

        type(point), pointer :: pc
        integer :: hIndex
        integer, dimension(1:HSIZE) :: nHash
        character(len=50) :: fnhashtxt, fhashtxt

        fhashtxt = trim(adjustl(sgSoluDoc))//'/hashTable.txt'
        open(unit=11, file=trim(adjustl(fhashtxt)), status='replace')

        TNumSG = 0
        nHash(1:HSIZE) = 0
        hIndex = 1
        pc => hashTable(1)
        do while(hIndex <= HSIZE)
            if(pc%myID%le(1) /= 0) then
                nHash(hIndex) = nHash(hIndex) + 1
                TNumSG = TNumSG + 1

                write(11, fmt="(i4)") pc%myID%le
                write(11, fmt="(i4)") pc%myID%li
                write(11, fmt="(es15.8)") pc%value
                write(11, fmt="(es15.8)") pc%surplus
                write(11, fmt="(l2)") pc%stamp
                write(11, fmt="(es15.8)") pc%lend
                write(11, fmt="(es15.8)") pc%rend
                write(11, fmt="(es15.8)") pc%coordinate
            end if
            if(associated(pc%next)) then
                pc => pc%next
            else
                hIndex = hIndex + 1
                if(hIndex <= HSIZE) then
                    pc => hashTable(hIndex)
                end if
            end if
        end do

        fnhashtxt = trim(adjustl(sgSoluDoc))//'/numHash.txt'
        open(unit=10, file=trim(adjustl(fnhashtxt)), status='replace')
        write(10, fmt="(i4)") nHash(:)
        close(10)
        close(11)

        print *, 'The total number of points is ', TNumSG
        print *, 'The sparse degree is ', TNumSG*1.D2/(2**MAXLAYER-1)**DIM, '%'

    end subroutine sgTraverse

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
        head => god
        tail => god
        do while(associated(head))
            call sgHierarchy()
        end do
        call sgOutput()
        call sgTraverse()
        call sgTestDriver()

    end subroutine sgDriver

end module RST_sg

