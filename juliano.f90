program juliano

    implicit none

    ! General parameters for the setting
    integer :: i, n
    integer, parameter :: grid_size = 360 ! Longitude resolution [°]
    integer, parameter :: num_time_steps = 1080 ! Simulation length [hour] -> 45 days
    real, parameter :: dt = 1.0 ! Time step [day]
    real, parameter :: dx = 1.0 ! Grid spacing [°]
    real, parameter :: c = 0.333  ! Background flow speed [°/hour] -> one cycle every 45 days
    real :: h(grid_size), dh(grid_size)

    ! Gaussian parameters of the wave
    ! MJO has a positive (convective) and negative phase (subsidence)
    ! NOTE: The wave is assumed to be Gaussian
    integer, parameter :: up_center = 180
    integer, parameter :: down_center = 90
    real, parameter :: decay = 0.001

    ! Checking some conditions
    if (grid_size <= 0) stop 'grid size must be > 0'
    if (dt <= 0) stop 'time step must be > 0'
    if (dx <= 0) stop 'grid spacing dx must be > 0'
    if (c <= 0) stop 'background flow speed c must be > 0'

    ! Initializing water height
    do concurrent (i = 1:grid_size)
        h(i) = exp(-decay * (i - up_center) ** 2) + -1 * exp(-decay * (i - down_center) ** 2)
    end do

    print *, 0, h

    ! Iterate over time
    time_loop: do n = 1, num_time_steps
        dh(1) = h(1) - h(grid_size)

        do concurrent (i = 2:grid_size)
            dh(i) = h(i) - h(i-1)
        end do

        do concurrent (i = 1:grid_size)
            h(i) = h(i) - c * dh(i) / dx * dt
        end do

        if (mod(n, 10) == 0) print *, n, h
    end do time_loop

end program juliano
