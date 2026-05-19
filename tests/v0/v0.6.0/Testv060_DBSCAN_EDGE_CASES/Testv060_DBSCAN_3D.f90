program Testv060_DBSCAN_3D
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscan3D()
    contains
        !> 3D tree with two well-separated clusters of 3 points each.
        subroutine dbscan3D()
            type(KdTree)                    :: t
            real(real64)                    :: coords(3, 6) = reshape( &
                [0.0_real64, 0.0_real64, 0.0_real64, &
                 0.1_real64, 0.0_real64, 0.0_real64, &
                 0.0_real64, 0.1_real64, 0.0_real64, &
                 10.0_real64, 10.0_real64, 10.0_real64, &
                 10.1_real64, 10.0_real64, 10.0_real64, &
                 10.0_real64, 10.1_real64, 10.0_real64], [3, 6])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.5_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_3D ---'
                write(*, '(A,I0)') 'expected 2 clusters in 3D, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_3D ---'
                write(*, '(A,I0)') 'expected 0 noise, got: ', noiseSize
                stop 1
            end if
        end subroutine dbscan3D
end program Testv060_DBSCAN_3D
