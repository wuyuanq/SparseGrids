
!!$ Author:
!!$   Yuanqing Wu, KAUST, Saudi Arabia
!!$
!!$ History:
!!$   2017-4-9 by Yuanqing Wu
!!$
!!$ Support:
!!$   wuyuanq@gmail.com

module RST_globalSgData

    use omp_lib
    implicit none

    ! the model parameters
    integer, parameter :: DIM = 5 !!
    integer, parameter :: HSIZE = 10**4-1 ! must be odd
    integer, parameter :: PRIMESIZE = 10**4
    integer, parameter :: SURPLUSSIZE = 3*DIM+6
    integer, parameter :: MAXLAYER = 11 !!
    real(kind=8), parameter :: PMIN = 3.D6 !1.98D6 !
    real(kind=8), parameter :: PMAX = 4.D6 !2.12D6 !
    real(kind=8), parameter :: XYPREC = 1.D-5
    real(kind=8), parameter :: XILPREC = 1.D-5
    real(kind=8), parameter :: XIGPREC = 1.D-1
    real(kind=8), parameter :: RHOLPREC = 1.D-1
    real(kind=8), parameter :: RHOGPREC = 1.D-2
    real(kind=8), parameter :: SLPREC = 1.D-5
    real(kind=8), parameter :: VPREC = 1.D-6
    real(kind=8), parameter :: CFPREC = 1.D-8
    real(kind=8), parameter :: EQUALPREC = 1.D-10
    integer, parameter :: NUMSAMPLE = 10

    ! the program parameters
    integer, parameter :: NUMTHREADS = 2 !!!!

    ! the global variables
    type pointID
        integer, dimension(1:DIM) :: le  ! need to be initialized to 0
        integer, dimension(1:DIM) :: li
    endtype pointID
    type pPoint
        type(point), pointer :: pp
    endtype pPoint
    type point
        type(pointID) :: myID
        real(kind=8), dimension(1:SURPLUSSIZE) :: value
        real(kind=8), dimension(1:SURPLUSSIZE) :: surplus
        logical, dimension(1:SURPLUSSIZE) :: stamp
        real(kind=8), dimension(1:DIM) :: lend
        real(kind=8), dimension(1:DIM) :: rend
        real(kind=8), dimension(1:DIM) :: coordinate
        type(pPoint), dimension(1:2*DIM) :: pChildArray
        logical :: isInSG
    endtype point
    type(point), pointer :: godPoint
    type iniLayerPoint
        type(point), pointer :: pp
        type(iniLayerPoint), pointer :: next
    endtype iniLayerPoint
    type(iniLayerPoint), pointer :: ilqHead, ilqTail
    integer :: iniLayer, curLayer
    type queueNode
        type(pointID) :: myID
        type(point), pointer :: parent
        integer :: pCAIndex
        type(queueNode), pointer :: next
    endtype queueNode
    type(queueNode), pointer :: qHead, qTail
    type hashNode
        type(pointID) :: myID
        type(hashNode), pointer :: next
    endtype hashNode
    type(hashNode), dimension(1:HSIZE), target :: hashTable
    integer(kind=OMP_lock_kind), dimension(1:HSIZE) :: lock
    integer, dimension(1:PRIMESIZE) :: prime
    integer, dimension(1:DIM) :: sgTestArray
    integer :: tNumP, tNumC
    real(kind=8) :: fTimeSum, iTimeSum
    character(len=10) :: soluDoc

end module RST_globalSgData

