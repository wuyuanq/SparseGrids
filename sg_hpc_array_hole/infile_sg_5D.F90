
!!$ Author:
!!$   Yuanqing Wu, KAUST, Saudi Arabia
!!$
!!$ History:
!!$   2017-4-9 by Yuanqing Wu
!!$
!!$ Support:
!!$   wuyuanq@gmail.com

program infile_sg

    use RST_globalSgData
    use RST_sg
    implicit none

    ! 1=CO2, 2=CH4, 3=C3H8, 4=C5H12, 5=C6H14
    Nc = DIM
    Temp = 2.2D2
    allocate(ct(Nc))
    ct(1) = 3.044D2
    ct(2) = 1.9D2
    ct(3) = 3.7D2
    ct(4) = 4.698D2
    ct(5) = 5.075D2
    allocate(cp(Nc))
    cp(1) = 7.4D6
    cp(2) = 4.6D6
    cp(3) = 4.2D6
    cp(4) = 3.36D6
    cp(5) = 3.03D6
    allocate(af(Nc))
    af(1) = 2.3D-1
    af(2) = 1.D-2
    af(3) = 1.5D-1
    af(4) = 2.5D-1
    af(5) = 2.99D-1
    allocate(mw(Nc))
    mw(1) = 4.4D-2
    mw(2) = 1.6D-2
    mw(3) = 4.4D-2
    mw(4) = 7.2D-2
    mw(5) = 8.6D-2
    allocate(cv(Nc))
    cv(1) = 2.1D-3
    cv(2) = 6.2D-3
    cv(3) = 4.5D-3
    cv(4) = 3.1D-4
    cv(5) = 3.7D-4
    allocate(psatA(Nc))
    psatA(1) = 6.81228D0
    psatA(2) = 6.69561D0
    psatA(3) = 6.82973D0
    psatA(4) = 6.85296D0
    psatA(5) = 6.91058D0
    allocate(psatB(Nc))
    psatB(1) = 1.301679D3
    psatB(2) = 4.0542D2
    psatB(3) = 8.132D2
    psatB(4) = 1.06484D3
    psatB(5) = 1.18964D3
    allocate(psatC(Nc))
    psatC(1) = 2.69506D2
    psatC(2) = 2.67777D2
    psatC(3) = 2.48D2
    psatC(4) = 2.32012D2
    psatC(5) = 2.2628D2
    allocate(delta(Nc,Nc))
    delta(:,:) = 0.D0
    delta(1,2) = 9.19D-2
    delta(2,1) = delta(1,2)
    delta(1,3) = 1.241D-1
    delta(3,1) = delta(1,3)
    delta(1,4) = 1.222D-1
    delta(4,1) = delta(1,4)
    delta(1,5) = 1.1D-1
    delta(5,1) = delta(1,5)
    delta(2,3) = 1.4D-2
    delta(3,2) = delta(2,3)
    delta(2,4) = 2.3D-2
    delta(4,2) = delta(2,4)
    delta(2,5) = 4.22D-2
    delta(5,2) = delta(2,5)
    delta(3,4) = 2.67D-2
    delta(4,3) = delta(3,4)
    delta(3,5) = 7.D-4
    delta(5,3) = delta(3,5)
    delta(4,5) = 2.D-4
    delta(5,4) = delta(4,5)
    soludoc = 'Case5'

    call sgDriver()

    deallocate(ct)
    deallocate(cp)
    deallocate(af)
    deallocate(mw)
    deallocate(cv)
    deallocate(psatA)
    deallocate(psatB)
    deallocate(psatC)
    deallocate(delta)

end program infile_sg
