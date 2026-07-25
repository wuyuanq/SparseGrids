
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

    ! 1=CH4, 2=C3H8
    Nc = DIM
    Temp = 2.2D2
    allocate(ct(Nc))
    ct(1) = 1.9D2
    ct(2) = 3.7D2
    allocate(cp(Nc))
    cp(1) = 4.6*1.D6
    cp(2) = 4.2*1.D6
    allocate(af(Nc))
    af(1) = 1.D-2
    af(2) = 1.5D-1
    allocate(mw(Nc))
    mw(1) = 1.6D-2
    mw(2) = 4.4D-2
    allocate(cv(Nc))
    cv(1) = 6.2D-3
    cv(2) = 4.5D-3
    allocate(psatA(Nc))
    psatA(1) = 6.69561D0
    psatA(2) = 6.82973D0
    allocate(psatB(Nc))
    psatB(1) = 4.0542D2
    psatB(2) = 8.132D2
    allocate(psatC(Nc))
    psatC(1) = 2.67777D2
    psatC(2) = 2.48D2
    allocate(delta(Nc,Nc))
    delta(:,:) = 0.D0
    delta(1,2) = 3.6D-2
    delta(2,1) = delta(1,2)
    sgSoluDoc = 'Case1'

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
