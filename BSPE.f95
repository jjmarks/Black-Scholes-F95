program BSPE
   implicit none
   real :: S_0, K, T, sigma, r

   print*, "Input order: stock price, strike price, time to maturity, voltality, interest rate."

   ! S_0: stock price, K: strike price, T: time to maturity, sigma: volatility, r: risk-free interest rate
   read*, S_0, K, T, sigma, r

end program BSPE
