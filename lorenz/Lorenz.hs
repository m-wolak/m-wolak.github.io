type Vec3 = (Double,Double,Double)

(.*) :: Double -> Vec3 -> Vec3
(.*) s (a,b,c) = (s*a,s*b,s*c)

(.+) :: Vec3 -> Vec3 -> Vec3
(.+)  (a,b,c) (d,e,f) = (a + d,b + e, c+f)

rk4step3 :: (Vec3 -> Vec3) -> Double -> Vec3 -> Vec3
rk4step3 f delta old =  old .+ ((delta/6.0) .* (k1 .+ (2.*k2) .+ (2 .* k3) .+ k4))
    where
    k1 = f old
    k2 = f (old .+ ((0.5*delta) .* k1))
    k3 = f (old .+ ((0.5*delta) .* k2))
    k4 = f (old .+ (delta .* k3))


lorenzDerivs :: Double -> Double -> Double -> Vec3 -> Vec3
lorenzDerivs sigma r b (x,y,z) = (sigma*(y-x),r*x - y - x*z,x*y - b*z)

showVec3 :: Vec3 -> String
showVec3 (a,b,c) = (show a) ++ ", " ++ (show b) ++ ", " ++ (show c)

printPairs :: (Show a) => [a] -> IO ()
printPairs [] = return ()
printPairs [_] = return ()
printPairs (a:b:rest) = (putStr (show a)) >> (putStr ", ") >> (putStrLn (show b)) >> (printPairs (b:rest))

getLocMaxima :: [Double] -> [Double]
getLocMaxima [] = []
getLocMaxima [_] = []
getLocMaxima [_,_] = []
getLocMaxima (a:b:c:rest) = if (b > a) && (b > c) then b:(getLocMaxima (c:rest)) else getLocMaxima (b:c:rest) 
    

main = do
    let lorenzStep = rk4step3 (lorenzDerivs 10 28 (8/3)) 0.001
    let iterates = iterate lorenzStep (1,1,1)
    let zs = getLocMaxima (map (\(_,_,z) -> z) iterates)
    
    --printPairs (take 10000 zs)
    mapM_ (\x -> putStrLn $ showVec3 x) (take 100000 iterates)

