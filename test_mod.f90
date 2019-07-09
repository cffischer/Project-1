!======================================================================
 Module Nucleus_m
!======================================================================
!  RRMS values for the periodic table
!I. Angeli and K.P. Marinova, Atomic Data and Nuclear Data Tables 99 (2013) 69–95,
! When values are not available the defaut formula is taken to be
!    rrms = 0.836*a**(1/3) + 0.570
!  shown to be a good approximation for Z < 91 by
!W.R. Johnson, G.Soff, Atomic Data and Nuclear Data Tables {\bf 33}, p.405 (1985)
!----------------------------------------------------------------------
    Implicit none
    Integer, parameter :: n_atoms = 3, n_range = 35

    Integer  :: a_min(n_atoms), a_max(n_atoms)
    Real(8)  :: rr(n_atoms, n_range)

    Data a_min /  1, 3,  6  /
    Data a_max /  3, 8, 11  /

    Data rr(1, 1: 3) / 0.8783,2.1421,1.7591 /
    Data rr(2, 1: 6) / 1.9661,1.6755,0.0000,2.0660,0.0000,1.9239 /
    Data rr(3, 1: 6) / 2.5890,2.4440,2.3390,2.2450,0.0000,2.4820 /

!  Note the range rr(1:n) where rr(i) == rrms(z, a_min + i-1)
CONTAINS
!======================================================================
   Real(8) Function RRMS_value(iz,a)
!======================================================================
!  RRMS values for the given Z and A
!----------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER  i,a, iz, n

     rrms_value  = 0.d0
     i = a - a_min(iz) +1
     n = a_max(iz) - a_min(iz) +1

     If ( i >= 1  .and. i <= n ) then
         rrms_value = rr(iz, i)
     End if
     If  ( rrms_value == 0.d0 ) then
         rrms_value = 0.836**(1/3) + 0.570
     End if
   END FUNCTION RRMS_value

 END Module Nucleus_m


    PROGRAM  test_nucleus
     Use Nucleus_m https://github.com/cffischer/Project-1.git

     IMPLICIT NONE
     INTEGER  a, iz
     REAL(8) :: rrms

     print *, 'ENter the atomic number and mass nubmer'
     Read  *, iz, a
     If (iz >= n_atoms ) Print *, ' Atomic number out of range'

     Print *, ' Atomic number           = ', iz
     Print *, ' Nucleur Mass number     = ', a

     Print *, ' RRMS =', rrms_value(iz,a)

    End program test_nucleus


