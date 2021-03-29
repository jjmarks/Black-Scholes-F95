program BSPE
   implicit none
   ! BS Variables & Input
   real :: S_0, K, T, sigma, r
   real :: BS_CALCULATE

   print*, "Input order: stock price, strike price, time to maturity, voltality, interest rate."

   ! S_0: stock price, K: strike price, T: time to maturity, sigma: volatility, r: risk-free interest rate
   read*, S_0, K, T, sigma, r

   print*, BS_CALCULATE(S_0, K, T, sigma, r)

end program BSPE

function BS_CALCULATE(S_0, K, T, sigma, r)
   implicit none
   real :: S_0, K, T, sigma, r, omega
   real :: BS_CALCULATE, ANORDF

   omega = ( r*T + (sigma**2 * T / 2) - log(K/S_0) ) / ( sigma * sqrt(T) )
   
   !BS_CALCULATE = S_0 * Phi(omega) - K * exp(-r*T) * Phi(omega - sigma*sqrt(T))
end function BS_CALCULATE
