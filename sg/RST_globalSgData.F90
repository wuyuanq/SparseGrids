
!!$ Author:
!!$   Yuanqing Wu, KAUST, Saudi Arabia
!!$
!!$ History:
!!$   2017-4-9 by Yuanqing Wu
!!$
!!$ Support:
!!$   wuyuanq@gmail.com

module RST_globalSgData

    implicit none

    character(len=*), parameter :: HASHPREFIX = '/Users/yuanqingwu/research/SparseGrids/sg/Case1/'

    ! the model parameters
    integer, parameter :: DIM = 2 !!!
    integer, parameter :: HSIZE = 10**4-1 ! must be odd
    integer, parameter :: PRIMESIZE = 10**4
    integer, parameter :: SURPLUSSIZE = 3*DIM+6
    integer, parameter :: MAXLAYER = 10 !!
    real(kind=8), parameter :: PMIN = 1.9D6 !1.98D6 !
    real(kind=8), parameter :: PMAX = 2.1D6 !2.12D6 !
    real(kind=8), parameter :: XYPREC = 1.D-5
    real(kind=8), parameter :: XILPREC = 1.D-1
    real(kind=8), parameter :: XIGPREC = 1.D-1
    real(kind=8), parameter :: RHOLPREC = 1.D-1
    real(kind=8), parameter :: RHOGPREC = 1.D-2
    real(kind=8), parameter :: SLPREC = 1.D-5
    real(kind=8), parameter :: VPREC = 1.D-6
    real(kind=8), parameter :: CFPREC = 1.D-8
    real(kind=8), parameter :: EQUALPREC = 1.D-7
    integer, parameter :: NUMSAMPLE = 10

    ! the global variables
    type pointID
        integer, dimension(1:DIM) :: le  ! need to be initialized to 0
        integer, dimension(1:DIM) :: li
    endtype pointID
    type point
        type(pointID) :: myID
        real(kind=8), dimension(1:SURPLUSSIZE) :: value
        real(kind=8), dimension(1:SURPLUSSIZE) :: surplus
        real(kind=8), dimension(1:DIM) :: lend
        real(kind=8), dimension(1:DIM) :: rend
        real(kind=8), dimension(1:DIM) :: coordinate
        type(point), pointer :: next
    endtype point
    type(point), dimension(1:HSIZE), target :: hashTable
    type queueNode
        type(pointID) :: myID
        type(queueNode), pointer :: next
    endtype queueNode
    type(queueNode), pointer :: upHead, upTail, downHead, downTail
    logical :: isUpEmpty
    integer, dimension(1:PRIMESIZE) :: prime
    integer, dimension(1:DIM) :: sgTestArr
    integer :: TNumF, TNumSG
    character(len=10) :: sgSoluDoc

end module RST_globalSgData

