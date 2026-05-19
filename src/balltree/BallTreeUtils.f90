submodule(NdTreeFortran) BallTreeUtils
    implicit none
    contains
        
        module procedure setMetricBLT
            logical :: isInit
            call this%getInitState(isInit)
            if (isInit) then  
                error stop "setMetric: tree is already initialized!"
            else 
                if (present(metric)) then ! comes preloaded with default, no need to double set 
                    select case (metric)
                        case ('euclidean')
                        case ('manhattan')
                        case ('chebyshev')
                        case default
                            error stop "setMetric: unknown metric"
                    end select
                    this%metric = metric
                end if
            end if
        end procedure setMetricBLT

        module procedure getMetricBLT
            logical :: isInit

        end procedure getMetricBLT

end submodule BallTreeUtils