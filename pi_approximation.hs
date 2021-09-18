{--
Function to approximate pi

params
------
Tolerance for the returned value : Float

returns
-------
Approximated value and number of recursive steps : Tuple(Float, Int)
--}

approx_pi :: Float -> (Float, Int)
approx_pi tol = calc_pi 1.0 tol 1 True
calc_pi val tol n sgn
 | 4*abs(val - new_val) < tol = (4*val, n)
 | otherwise = calc_pi new_val tol (n+1) (not sgn)
 where
 new_val | sgn = val - (1.0/fromIntegral(2*n+1))
 | otherwise = val + (1.0/fromIntegral(2*n+1))