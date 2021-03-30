program BSPE
   implicit none
   ! BS Variables & Input
   real :: S_0, K, T, sigma, r
   real :: BS_CALCULATE, PCparity, Phi
   real :: C_0, P_0

   print*, "Input order: stock price ($), strike price ($), time to maturity (years), voltality (rate), interest rate."

   ! S_0: stock price, K: strike price, T: time to maturity, sigma: volatility, r: risk-free interest rate
   read*, S_0, K, T, sigma, r

   ! Output European call option price
   C_0 = BS_CALCULATE(S_0, K, T, sigma, r)
   print*, "Price of European call option: ", C_0
   ! Output European put option price
   P_0 = PCparity(C_0, K, r, T, S_0)
   print*, "Price of European put option: ", P_0

end program BSPE

! Calculate the value of the non-dividend paying call option
! https://en.wikipedia.org/wiki/Black-Scholes_model#Black–Scholes_formula
function BS_CALCULATE(S_0, K, T, sigma, r)
   implicit none
   real :: S_0, K, T, sigma, r, omega
   real :: BS_CALCULATE, Phi

   omega = ( r*T + (sigma**2 * T / 2) - log(K/S_0) ) / ( sigma * sqrt(T) )
   BS_CALCULATE = S_0 * Phi(omega) - K * exp(-r*T) * Phi(omega - sigma*sqrt(T))
end function BS_CALCULATE

! Calculate normal cdf
! https://en.wikipedia.org/wiki/Normal_distribution#Cumulative_distribution_function
function Phi(z)
   implicit none
   real, parameter :: pi = 4.D0*DATAN(1.D0)
   real :: Phi, z
   integer :: k

   ! Taylor series normal approximation
   Phi = 0.5
   do k = 0, 20
      Phi = Phi + ( ((-1.0)**k) * (z** (2.0*k+1.0)) ) / ( (2.0**k) * gamma(real(k+1)) * (2.0*k+1.0) ) * (1.0/sqrt(2*pi))
   end do
end function Phi

function PCparity(C_0, K, r, T, S_0)
   implicit none
   real :: PCparity, C_0, K, r, T, S_0

   PCparity = C_0 + K * exp(-r*T) - S_0
end function PCparity
