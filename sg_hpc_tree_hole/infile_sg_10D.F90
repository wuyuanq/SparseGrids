
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

    ! 1=Methane, 2=Ethane, 3=Propane, 4=n-Butane, 5=i-Butane
    ! 6=n-Pentane, 7=n-Hexane, 8=n-Heptane, 9=n-Octane, 10=n-Nonane
    Nc = DIM
    Temp = 2.53D2
    allocate(ct(Nc))
    ct(1) = 1.9056D2
    ct(2) = 3.0532D2
    ct(3) = 3.6983D2
    ct(4) = 4.2512D2
    ct(5) = 4.0785D2
    ct(6) = 4.697D2
    ct(7) = 5.076D2
    ct(8) = 5.402D2
    ct(9) = 5.687D2
    ct(10) = 5.946D2
    allocate(cp(Nc))
    cp(1) = 4.599D6
    cp(2) = 4.872D6
    cp(3) = 4.248D6
    cp(4) = 3.796D6
    cp(5) = 3.64D6
    cp(6) = 3.37D6
    cp(7) = 3.025D6
    cp(8) = 2.74D6
    cp(9) = 2.49D6
    cp(10) = 2.29D6
    allocate(af(Nc))
    af(1) = 1.1D-2
    af(2) = 9.9D-2
    af(3) = 1.52D-1
    af(4) = 1.99D-1
    af(5) = 1.86D-1
    af(6) = 2.51D-1
    af(7) = 2.97D-1
    af(8) = 3.5D-1
    af(9) = 3.97D-1
    af(10) = 4.43D-1
    allocate(mw(Nc))
    mw(1) = 1.6D-2
    mw(2) = 3.0D-2
    mw(3) = 4.4D-2
    mw(4) = 5.8D-2
    mw(5) = 5.8D-2
    mw(6) = 7.2D-2
    mw(7) = 8.6D-2
    mw(8) = 1.0D-1
    mw(9) = 1.14D-1
    mw(10) = 1.28D-1
    allocate(cv(Nc)) ! m^3/kg
    cv(1) = 6.2D-3
    cv(2) = 4.7D-3
    cv(3) = 4.5D-3
    cv(4) = 4.3D-3
    cv(5) = 4.47D-3
    cv(6) = 4.3D-3
    cv(7) = 4.3D-3
    cv(8) = 4.28D-3
    cv(9) = 4.32D-3
    cv(10) = 4.34D-3
    allocate(psatA(Nc))
    psatA(1) = 6.69561D0
    psatA(2) = 6.83452D0
    psatA(3) = 6.80398D0
    psatA(4) = 6.80896D0
    psatA(5) = 6.91048D0
    psatA(6) = 6.87632D0
    psatA(7) = 6.87024D0
    psatA(8) = 6.89385D0
    psatA(9) = 6.90940D0
    psatA(10) = 6.93440D0
    allocate(psatB(Nc))
    psatB(1) = 4.0542D2
    psatB(2) = 6.6370D2
    psatB(3) = 8.03810D2
    psatB(4) = 9.35860D2
    psatB(5) = 9.46350D2
    psatB(6) = 1.075780D3
    psatB(7) = 1.168720D3
    psatB(8) = 1.264370D3
    psatB(9) = 1.349820D3
    psatB(10) = 1.429460D3
    allocate(psatC(Nc))
    psatC(1) = 2.67777D2
    psatC(2) = 2.56470D2
    psatC(3) = 2.46990D2
    psatC(4) = 2.38730D2
    psatC(5) = 2.46680D2
    psatC(6) = 2.33205D2
    psatC(7) = 2.24210D2
    psatC(8) = 2.16636D2
    psatC(9) = 2.09385D2
    psatC(10) = 2.01820D2
    allocate(delta(Nc,Nc))
    delta(:,:) = 0.D0
    delta(1,2) = -2.6D-3
    delta(2,1) = delta(1,2)
    delta(1,3) = 1.4D-2
    delta(3,1) = delta(1,3)
    delta(1,4) = 1.33D-2
    delta(4,1) = delta(1,4)
    delta(1,5) = 2.56D-2
    delta(5,1) = delta(1,5)
    delta(1,6) = 2.3D-2
    delta(6,1) = delta(1,6)
    delta(1,7) = 4.22D-2
    delta(7,1) = delta(1,7)
    delta(1,8) = 3.52D-2
    delta(8,1) = delta(1,8)
    delta(1,9) = 4.96D-2
    delta(9,1) = delta(1,9)
    delta(1,10) = 4.74D-2
    delta(10,1) = delta(1,10)
    delta(2,3) = 1.1D-3
    delta(3,2) = delta(2,3)
    delta(2,4) = 9.6D-3
    delta(4,2) = delta(2,4)
    delta(2,5) = -6.7D-3
    delta(5,2) = delta(2,5)
    delta(2,6) = 7.8D-3
    delta(6,2) = delta(2,6)
    delta(2,7) = -1.D-2
    delta(7,2) = delta(2,7)
    delta(2,8) = 6.7D-3
    delta(8,2) = delta(2,8)
    delta(2,9) = 1.85D-2
    delta(9,2) = delta(2,9)
    delta(2,10) = 0.D0
    delta(10,2) = delta(2,10)
    delta(3,4) = 3.3D-3
    delta(4,3) = delta(3,4)
    delta(3,5) = -7.8D-3
    delta(5,3) = delta(3,5)
    delta(3,6) = 2.67D-2
    delta(6,3) = delta(3,6)
    delta(3,7) = 7.D-4
    delta(7,3) = delta(3,7)
    delta(3,8) = 5.6D-3
    delta(8,3) = delta(3,8)
    delta(3,9) = 0.D0
    delta(9,3) = delta(3,9)
    delta(3,10) = 0.D0
    delta(10,3) = delta(3,10)
    delta(4,5) = -4.D-4
    delta(5,4) = delta(4,5)
    delta(4,6) = 1.74D-2
    delta(6,4) = delta(4,6)
    delta(4,7) = -5.6D-3
    delta(7,4) = delta(4,7)
    delta(4,8) = 3.3D-3
    delta(8,4) = delta(4,8)
    delta(4,9) = 7.4D-3
    delta(9,4) = delta(4,9)
    delta(4,10) = 0.D0
    delta(10,4) = delta(4,10)
    delta(5,6) = 0.D0
    delta(6,5) = delta(5,6)
    delta(5,7) = 0.D0
    delta(7,5) = delta(5,7)
    delta(5,8) = 0.D0
    delta(8,5) = delta(5,8)
    delta(5,9) = 0.D0
    delta(9,5) = delta(5,9)
    delta(5,10) = 0.D0
    delta(10,5) = delta(5,10)
    delta(6,7) = 0.D0
    delta(7,6) = delta(6,7)
    delta(6,8) = 7.4D-3
    delta(8,6) = delta(6,8)
    delta(6,9) = 0.D0
    delta(9,6) = delta(6,9)
    delta(6,10) = 0.D0
    delta(10,6) = delta(6,10)
    delta(7,8) = -7.8D-3
    delta(8,7) = delta(7,8)
    delta(7,9) = 0.D0
    delta(9,7) = delta(7,9)
    delta(7,10) = 0.D0
    delta(10,7) = delta(7,10)
    delta(8,9) = 0.D0
    delta(9,8) = delta(8,9)
    delta(8,10) = 0.D0
    delta(10,8) = delta(8,10)
    delta(9,10) = 0.D0
    delta(10,9) = delta(9,10)
    soludoc = 'Case4'

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
