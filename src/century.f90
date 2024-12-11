!======================================================================!
program century
!----------------------------------------------------------------------!
implicit none
!----------------------------------------------------------------------!
integer, parameter :: nyr = 10000
!----------------------------------------------------------------------!
! Surface litter N (fraction of DM)
!----------------------------------------------------------------------!
real, parameter :: fol_N = 0.02
!----------------------------------------------------------------------!
! Root litter N (fraction of DM)
!----------------------------------------------------------------------!
real, parameter :: fro_N = 0.02
!----------------------------------------------------------------------!
! Surface litter lignin (fraction of DM)
!----------------------------------------------------------------------!
real, parameter :: fol_lig = 0.2
!----------------------------------------------------------------------!
! Root litter lignin (fraction of DM)
!----------------------------------------------------------------------!
real, parameter :: fro_lig = 0.16
!----------------------------------------------------------------------!
! Decay constant for surface structural litter pool (/yr)
!----------------------------------------------------------------------!
real, parameter :: k1 = 3.9
!----------------------------------------------------------------------!
! Decay constant for soil structural litter pool (/yr)
!----------------------------------------------------------------------!
real, parameter :: k2 = 4.8
!----------------------------------------------------------------------!
! Decay constant for soil active pool (/yr)
!----------------------------------------------------------------------!
real, parameter :: k3 = 7.3
!----------------------------------------------------------------------!
! Decay constant for surface active pool (/yr)
!----------------------------------------------------------------------!
real, parameter :: k4 = 6.0
!----------------------------------------------------------------------!
! Decay constant for surface metabolic litter pool (/yr)
!----------------------------------------------------------------------!
real, parameter :: k5 = 14.8
!----------------------------------------------------------------------!
! Decay constant for soil metabolic litter pool (/yr)
!----------------------------------------------------------------------!
real, parameter :: k6 = 18.5
!----------------------------------------------------------------------!
! Decay constant for slow pool (/yr)
!----------------------------------------------------------------------!
real, parameter :: k7 = 0.2
!----------------------------------------------------------------------!
! Decay constant for passive poot (/yr)
!----------------------------------------------------------------------!
real, parameter :: k8 = 0.0045
!----------------------------------------------------------------------!
! Fraction of surface structural lignin pool respired on decay to slow
! (fraction)
!----------------------------------------------------------------------!
real, parameter :: f_surf_str_lig_slow = 0.3
!----------------------------------------------------------------------!
! Fraction of surface structural cellulose pool respired on decay to
! active (fraction)
!----------------------------------------------------------------------!
real, parameter :: f_surf_str_cel_act = 0.6
!----------------------------------------------------------------------!
! Fraction of surface active pool respired on decay to slow pool
! (fraction).
!----------------------------------------------------------------------!
real, parameter :: f_surf_act_slow = 0.6
!----------------------------------------------------------------------!
! Fraction of root litter structural lignin pool respired on decay to
! slow pool.
!----------------------------------------------------------------------!
real, parameter :: f_soil_str_lig_slow = 0.3
!----------------------------------------------------------------------!
! Fraction of root litter structural cellulose pool respired on decay
! to soil active pool.
!----------------------------------------------------------------------!
real, parameter :: f_soil_str_cel_act = 0.55
!----------------------------------------------------------------------!
! Fraction of surface metabolic pool respired on decay (fraction)
!----------------------------------------------------------------------!
real, parameter :: f_surf_met = 0.6
!----------------------------------------------------------------------!
! Fraction of soil metabolic pool respired on decay (fraction)
!----------------------------------------------------------------------!
real, parameter :: f_soil_met = 0.55
!----------------------------------------------------------------------!
! Fraction of slow pool respired on decay to passive pool (fraction
!----------------------------------------------------------------------!
real, parameter :: f_slow_passive = 0.55
!----------------------------------------------------------------------!
! Fraction of passive pool respired on decay to soil active pool
! (fraction
!----------------------------------------------------------------------!
real, parameter :: f_passive_soil_active = 0.55
!----------------------------------------------------------------------!
! Clay content (fraction)
!----------------------------------------------------------------------!
real, parameter :: TC = 0.3
!----------------------------------------------------------------------!
! Silt plus clay (fraction)
!----------------------------------------------------------------------!
real, parameter :: T = 0.7
!----------------------------------------------------------------------!
real :: Surf_met ! Surface metabolic
real :: Surf_str_lig ! Surface structural lignin
real :: Surf_str_cel ! Surface structural cellulose
real :: Soil_met ! Soil metabolic
real :: Soil_str_lig ! Soil structural lignin
real :: Soil_str_cel ! Soil structural cellulose
real :: Surf_act ! Surface active (microbe)
real :: Soil_act ! Soil active (microbe)
real :: Slow     ! Slow
real :: Passive  ! Passive
real :: L_fol
real :: L_fro
real :: fol_LN
real :: fro_LN
real :: Fm
real :: Surf_met_lose
real :: Surf_str_lig_lose
real :: Surf_str_cel_lose
real :: Surf_act_lose
real :: Soil_met_lose
real :: Soil_str_lig_lose
real :: Soil_str_cel_lose
real :: Soil_act_lose
real :: Slow_lose
real :: Passive_lose
real :: CSP
real :: CSA
real :: CAS
real :: CAL
real :: CAP
real :: Ft
real :: A
real :: LC
real :: Tm
!----------------------------------------------------------------------!
integer :: kyr
integer :: kday
!----------------------------------------------------------------------!
write (*,*) "Century has started..."
!----------------------------------------------------------------------!
! Initial soil organic matter values (kg[C]/m2).
!----------------------------------------------------------------------!
Surf_met = 0.0  ! Surface metabolic
Surf_str_lig = fol_lig * 0.0!0.5         ! Surface structural lignin
Surf_str_cel = (1.0 - fol_lig) * 0.0!0.5 ! Surface structural cellulose
Soil_met = 0.0  ! Soil metabolic
Soil_str_lig = fro_lig * 0.0!0.5         ! Soil structural lignin
Soil_str_cel = (1.0 - fro_lig) * 0.0!0.5 ! Soil structural cellulose
Surf_act = 0.0!0.04 ! Surface active (microbe)
Soil_act = 0.0!0.3  ! Soil active (microbe)
Slow     = 0.0!5.0  ! Slow
Passive  = 0.0!8.0  ! Passive
!----------------------------------------------------------------------!
do kyr = 1, nyr
  do kday = 1, 365
    !------------------------------------------------------------------!
    ! Abiotic decomposition factor.
    !------------------------------------------------------------------!
    A = 0.25
    !------------------------------------------------------------------!
    ! Surface input (kg[C]/m2/day)
    !------------------------------------------------------------------!
    L_fol = 0.8 / 365.0
    !------------------------------------------------------------------!
    ! Surface litter lignin/N (ratio)
    !------------------------------------------------------------------!
    fol_LN = fol_lig / fol_N
    !------------------------------------------------------------------!
    ! Metabolic fraction of surface litter.
    !------------------------------------------------------------------!
    Fm = 0.99 - (fol_LN) * 0.018
    !------------------------------------------------------------------!
    ! Add litter to surface structural lignin litter pool.
    !------------------------------------------------------------------!
    Surf_str_lig = Surf_str_lig + fol_lig * (1.0 - Fm) * L_fol
    !------------------------------------------------------------------!
    ! Add litter to surface structural cellulose litter pool.
    !------------------------------------------------------------------!
    Surf_str_cel = Surf_str_cel + (1.0 - fol_lig) * (1.0 - Fm) * L_fol
    !------------------------------------------------------------------!
    ! Add foliage litter to surface metabolic litter pool.
    !------------------------------------------------------------------!
    Surf_met = Surf_met + Fm * L_fol
    !------------------------------------------------------------------!
    ! Root litter input (kg[C]/m2/day)
    !------------------------------------------------------------------!
    L_fro = 0.3 / 365.0
    !------------------------------------------------------------------!
    ! Root lignin/N (ratio)
    !------------------------------------------------------------------!
    fro_LN = fro_lig / fro_N
    !------------------------------------------------------------------!
    ! Metabolic fraction of root litter.
    !------------------------------------------------------------------!
    Fm = 0.99 - (fro_LN) * 0.018
    !------------------------------------------------------------------!
    ! Add root litter to soil structural lignin litter pool.
    !------------------------------------------------------------------!
    Soil_str_lig = Soil_str_lig + fro_lig * (1.0 - Fm) * L_fro
    !------------------------------------------------------------------!
    ! Add root litter to soil structural cellulose litter pool.
    !------------------------------------------------------------------!
    Soil_str_cel = Soil_str_cel + (1.0 - fro_lig) * (1.0 - Fm) * L_fro
    !------------------------------------------------------------------!
    ! Add root litter to soil metabolic litter pool.
    !------------------------------------------------------------------!
    Soil_met = Soil_met + Fm * L_fro
    !------------------------------------------------------------------!
    ! Lignin function.
    !------------------------------------------------------------------!
    LC = exp (-3.0 * fol_lig)
    !------------------------------------------------------------------!
    ! Decay of surface structural lignin litter pool (kg[C]/m2/yr)
    !------------------------------------------------------------------!
    Surf_str_lig_lose = (k1 / 365.0) * LC * A * Surf_str_lig
    !------------------------------------------------------------------!
    ! Decay of surface structural cellulose litter pool (kg[C]/m2/yr)
    !------------------------------------------------------------------!
    Surf_str_cel_lose = (k1 / 365.0) * LC * A * Surf_str_cel
    !------------------------------------------------------------------!
    ! Decay of surface metabolic litter pool (kg[C]/m2/yr)
    !------------------------------------------------------------------!
    Surf_met_lose = (k5 / 365.0) * A * Surf_met
    !------------------------------------------------------------------!
    ! Remove decay from surface structural lignin litter pool
    !------------------------------------------------------------------!
    Surf_str_lig = Surf_str_lig - Surf_str_lig_lose
    !------------------------------------------------------------------!
    ! Remove decay from surface structural cellulose litter pool
    !------------------------------------------------------------------!
    Surf_str_cel = Surf_str_cel - Surf_str_cel_lose
    !------------------------------------------------------------------!
    ! Remove decay from surface metabolic pool
    !------------------------------------------------------------------!
    Surf_met = Surf_met - Surf_met_lose
    !------------------------------------------------------------------!
    ! Lignin function.
    !------------------------------------------------------------------!
    LC = exp (-3.0 * fro_lig)
    !------------------------------------------------------------------!
    ! Decay of soil structural lignin litter pool (kg[C]/m2/yr)
    !------------------------------------------------------------------!
    Soil_str_lig_lose = (k2 / 365.0) * LC * A * Soil_str_lig
    !------------------------------------------------------------------!
    ! Decay of soil structural cellulose litter pool (kg[C]/m2/yr)
    !------------------------------------------------------------------!
    Soil_str_cel_lose = (k2 / 365.0) * LC * A * Soil_str_cel
    !------------------------------------------------------------------!
    ! Decay of surface metabolic litter pool (kg[C]/m2/yr)
    !------------------------------------------------------------------!
    Soil_met_lose = (k6 / 365.0) * A * Soil_met
    !------------------------------------------------------------------!
    ! Remove decay from surface metabolic pool
    !------------------------------------------------------------------!
    Surf_met = Surf_met - Surf_met_lose
    !------------------------------------------------------------------!
    ! Remove decay from soil structural lignin litter pool
    !------------------------------------------------------------------!
    Soil_str_lig = Soil_str_lig - Soil_str_lig_lose
    !------------------------------------------------------------------!
    ! Remove decay from soil structural cellulose litter pool
    !------------------------------------------------------------------!
    Soil_str_cel = Soil_str_cel - Soil_str_cel_lose
    !------------------------------------------------------------------!
    ! Remove decay from soil metabolic litter pool
    !------------------------------------------------------------------!
    Soil_met = Soil_met - Soil_met_lose
    !------------------------------------------------------------------!
    ! Add flux from surface structure lignin to slow pool.
    !------------------------------------------------------------------!
    Slow = Slow + (1.0 - f_surf_str_lig_slow) * Surf_str_lig_lose
    !------------------------------------------------------------------!
    ! Add flux from surface structural cellulose to surface microbe pool.
    !------------------------------------------------------------------!
    Surf_act = Surf_act + (1.0 - f_surf_str_cel_act) * Surf_str_cel_lose
    !------------------------------------------------------------------!
    ! Add flux from surface metabolic to surface microbe pool.
    !------------------------------------------------------------------!
    Surf_act = Surf_act + (1.0 - f_surf_met) * Surf_met_lose
    !------------------------------------------------------------------!
    ! Add flux from soil structural litter lignin to slow.
    !------------------------------------------------------------------!
    Slow = Slow + (1.0 - f_soil_str_lig_slow) * Soil_str_lig_lose
    !------------------------------------------------------------------!
    ! Add flux from soil structural litter cellulose to soil active.
    !------------------------------------------------------------------!
    Soil_act = Soil_act + (1.0 - f_soil_str_cel_act) * Soil_str_cel_lose
    !------------------------------------------------------------------!
    ! Add flux from soil metabolic litter to soil active pool.
    !------------------------------------------------------------------!
    Soil_act = Soil_act + (1.0 - f_soil_met) * Soil_met_lose
    !------------------------------------------------------------------!
    ! Decay of surface active pool.
    !------------------------------------------------------------------!
    Surf_act_lose = (k4 / 365.0) * A * Surf_act
    !------------------------------------------------------------------!
    ! Remove decay from surface active pool.
    !------------------------------------------------------------------!
    Surf_act = Surf_act - Surf_act_lose
    !------------------------------------------------------------------!
    ! Add flux from surface active to slow pool.
    !------------------------------------------------------------------!
    Slow = Slow + (1.0 - f_surf_act_slow) * Surf_act_lose
    !------------------------------------------------------------------!
    ! Decay of slow pool.
    !------------------------------------------------------------------!
    Slow_lose = (k7 / 365.0) * A * Slow
    !------------------------------------------------------------------!
    ! Remove decay from slow pool.
    !------------------------------------------------------------------!
    Slow = Slow - Slow_lose
    !------------------------------------------------------------------!
    ! Fraction of slow decay to passive pool
    !------------------------------------------------------------------!
    CSP = 0.003 - 0.009 * TC
    !------------------------------------------------------------------!
    ! Add flux from slow to passive pool
    !------------------------------------------------------------------!
    Passive = Passive + CSP * (1.0 - f_slow_passive) * Slow_lose
    !------------------------------------------------------------------!
    ! Fraction of slow decay to soil active pool
    !------------------------------------------------------------------!
    CSA = 1.0 - CSP - f_slow_passive
    !------------------------------------------------------------------!
    ! Add flux from slow to soil active pool
    !------------------------------------------------------------------!
    Soil_act = Soil_act + CSA * Slow_lose
    !------------------------------------------------------------------!
    ! Texture control on soil active pool decay.
    !------------------------------------------------------------------!
    Tm = (1.0 - 0.75) * T
    !------------------------------------------------------------------!
    ! Decay of soil active pool.
    !------------------------------------------------------------------!
    Soil_act_lose = (k3 / 365.0) * A * Tm * Soil_act
    !------------------------------------------------------------------!
    ! Remove decay from soil active pool.
    !------------------------------------------------------------------!
    Soil_act = Soil_act - Soil_act_lose
    !------------------------------------------------------------------!
    ! Decay of passive pool.
    !------------------------------------------------------------------!
    Passive_lose = (k8 / 365.0) * A * passive
    !------------------------------------------------------------------!
    ! Remove decay from passive pool.
    !------------------------------------------------------------------!
    Passive = Passive - Passive_lose
    !------------------------------------------------------------------!
    ! Fraction of soil active decay to passive pool.
    !------------------------------------------------------------------!
    CAP= 0.003 + 0.032 * TC
    !------------------------------------------------------------------!
    ! Fraction of soil active decay leached.
    !------------------------------------------------------------------!
    CAL = 0.0
    !------------------------------------------------------------------!
    ! Fraction of soil active decay respired.
    !------------------------------------------------------------------!
    Ft = 0.85 - 0.68 * T
    !------------------------------------------------------------------!
    ! Fraction of soil active decay to active pool.
    !------------------------------------------------------------------!
    CAS = 1.0 - CAP - CAL - Ft
    !------------------------------------------------------------------!
    ! Add flux from soil active to slow pool.
    !------------------------------------------------------------------!
    Slow = Slow + CAS * Soil_act_lose
    !------------------------------------------------------------------!
    ! Add flux from soil active to passive pool.
    !------------------------------------------------------------------!
    Passive = Passive + (1.0 - CAS - CAL - Ft) * Soil_act_lose
    !------------------------------------------------------------------!
    ! Add flux from passive to soil active pool.
    !------------------------------------------------------------------!
    Soil_act = Soil_act + (1.0 - f_passive_soil_active) * Passive_lose
    !------------------------------------------------------------------!
  end do ! kday = 1, 365
  !--------------------------------------------------------------------!
  write (*,'(i5,8f12.4)') kyr, Surf_str_lig+Surf_str_cel, &
                          Soil_str_lig+Soil_str_cel, &
                          Soil_act, Surf_act, Surf_met, Soil_met, &
			  Slow, Passive
  !--------------------------------------------------------------------!
end do ! kyr = 1, nyr
write (*,'(a101)') '  kyr    Surf_str    Soil_str    Soil_act    Surf_act&
&    Surf_met    Soil_met        Slow     Passive'
write (*,'(a101)') '  kyr           1           2           3           4&
&           5           6           7           8'
!----------------------------------------------------------------------!
write (*,*) fol_LN, Fm
write (*,*) Surf_met + Surf_str_lig + Surf_str_cel + Soil_met + &
            Soil_str_lig + Soil_str_cel + Surf_act + Soil_act + Slow + &
	    Passive
write (*,*) "Century has finished..."
!----------------------------------------------------------------------!
end program century
!======================================================================!
