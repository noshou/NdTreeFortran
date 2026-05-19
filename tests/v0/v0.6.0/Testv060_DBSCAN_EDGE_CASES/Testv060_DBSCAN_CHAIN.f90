program Testv060_DBSCAN_CHAIN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanChain()
    contains
        !> 10 points in a chain: (0,0),(0.1,0),...,(0.9,0).
        !! radius=0.15, minPts=2. End points have 1 real neighbour + self = 2 >= minPts
        !! so all are core. Transitive expansion connects the whole chain -> 1 cluster.
        subroutine dbscanChain()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 10)
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: i, nClusters, noiseSize, total

            do i = 1, 10
                coords(1, i) = real(i - 1, real64) * 0.1_real64
                coords(2, i) = 0.0_real64
            end do

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.15_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)
            total     = 0
            do i = 1, size(res)
                total = total + size(res(i)%nodes)
            end do

            if (nClusters .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CHAIN ---'
                write(*, '(A,I0)') 'expected 1 cluster (chain), got: ', nClusters
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 10) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CHAIN ---'
                write(*, '(A,I0)') 'expected cluster size==10, got: ', size(res(1)%nodes)
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CHAIN ---'
                write(*, '(A,I0)') 'expected 0 noise, got: ', noiseSize
                stop 1
            end if
            if (total .ne. 10) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CHAIN ---'
                write(*, '(A,I0)') 'population invariant failed: total=', total
                stop 1
            end if
        end subroutine dbscanChain
end program Testv060_DBSCAN_CHAIN
