SUBROUTINE UEXTERNALDB(LOP, LRESTART, TIME, DTIME, KSTEP, KINC)  ! Handles external database operations
    IMPLICIT NONE  ! Make sure all variables are declared

    INTEGER :: LOP, LRESTART  ! Control flags for loops and restarts
    REAL :: TIME(2), DTIME  ! Current and previous time; time step size
    INTEGER :: KSTEP, KINC  ! Current step and increment numbers

    ! Define material properties for Ti6Al4V
    REAL :: E_MODULUS, NU, SIGMA_YIELD  ! Material property variables
    E_MODULUS = 113.8E9  ! This is the elastic modulus in Pascals
    NU = 0.342  ! Poisson's ratio tells us about volume change under stress
    SIGMA_YIELD = 880E6  ! Yield strength in Pascals; the point of no return!

    ! Output current simulation state
    PRINT *, 'Current Time:', TIME(1)  ! Shows the time at the moment
    PRINT *, 'Time Step:', DTIME  ! The size of the time increment
    PRINT *, 'Current Step:', KSTEP  ! Which step are we on?
    PRINT *, 'Increment:', KINC  ! Count of how many increments we've had

    ! Check if we're restarting the simulation
    IF (LRESTART .EQ. 1) THEN  ! Is it a restart?
        PRINT *, 'Simulation is restarting...'  ! Just letting you know
    ENDIF

    RETURN  ! Exit this subroutine
END SUBROUTINE UEXTERNALDB  ! Finished with UEXTERNALDB


SUBROUTINE UMAT(STRESS, STATEV, DSTRAN, TIME, DTIME,  ! Main routine for calculating stress
     1         CMODUL, ENTHAL, T, TEMP, TLOAD, LSTATE,  ! Various material parameters we might need
     2         H, LAMDA, LMECH, J, M,  ! Additional parameters for mechanical properties
     3         E, NDI, NSTATV, PROPS,  ! More details about dimensions and state variables
     4         RDMASS, RDXFR, RDRHO, RDK, RDMU)  ! Transport properties for materials
    IMPLICIT NONE  ! Enforce strict variable declarations

    ! Declare variables
    REAL :: STRESS(6)  ! An array to hold the stress components
    REAL :: STATEV(*)  ! State variables that might change
    REAL :: DSTRAN(6)  ! Incremental strain values
    REAL :: TIME(2), DTIME  ! Time management for the simulation
    REAL :: CMODUL, ENTHAL, T, TEMP, TLOAD  ! More material parameters
    INTEGER :: LSTATE, H, LAMDA, LMECH, J, M  ! Indices and state variables
    INTEGER :: E, NDI, NSTATV  ! For dimensions and number of state variables
    REAL :: PROPS(10)  ! Holds different material properties
    REAL :: RDMASS, RDXFR, RDRHO, RDK, RDMU  ! Variables for mass and transport calculations

    ! Ti6Al4V material properties
    REAL :: E_MODULUS, NU, SIGMA_YIELD, SIGMA_HARDENING  ! Define properties here
    E_MODULUS = 113.8E9  ! Elastic modulus in Pascals
    NU = 0.342  ! Poisson's ratio for the material
    SIGMA_YIELD = 880E6  ! Yield strength, the breaking point
    SIGMA_HARDENING = 0.01E6  ! A bit of hardening strength for our calculations

    ! Calculate stress based on strain
    STRESS(1) = E_MODULUS / (1.0 - NU**2) * (DSTRAN(1) + NU * (DSTRAN(2) + DSTRAN(3)))  ! Calculate sigma_x
    STRESS(2) = E_MODULUS / (1.0 - NU**2) * (DSTRAN(2) + NU * (DSTRAN(1) + DSTRAN(3)))  ! Calculate sigma_y
    STRESS(3) = E_MODULUS / (1.0 - NU**2) * (DSTRAN(3) + NU * (DSTRAN(1) + DSTRAN(2)))  ! Calculate sigma_z
    STRESS(4) = E_MODULUS * DSTRAN(4)  ! Shear stress calculation
    STRESS(5) = E_MODULUS * DSTRAN(5)  ! Another shear stress
    STRESS(6) = E_MODULUS * DSTRAN(6)  ! Last shear stress component

    ! Check for plastic deformation
    IF (MAXVAL(STRESS(1:3)) > SIGMA_YIELD) THEN  ! Is any stress above yield?
        DO J = 1, 3  ! Check each of the stress components
            IF (STRESS(J) > SIGMA_YIELD) THEN  ! Found a plastic deformation case
                STRESS(J) = SIGMA_YIELD + SIGMA_HARDENING * (STRESS(J) - SIGMA_YIELD)  ! Apply hardening here
            ENDIF
        ENDDO
        PRINT *, 'Plastic deformation initiated!'  ! Letting you know that it's happening
    ENDIF  ! End of plastic deformation check

    RETURN  ! Exit the subroutine
END SUBROUTINE UMAT  ! Finished with UMAT
