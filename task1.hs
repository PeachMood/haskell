findTheRoots 0 b c = error "This is not a quadratic equation"
findTheRoots a b c
                  | d > 0 = (x1,x2)
                  | d == 0 = (x1,x1)
                  | otherwise = error "No real roots"
                 where
                      d = b*b-4*a*c
                      x1=((-b)+ sqrt(d))/(2*a)
                      x2=((-b)- sqrt(d))/(2*a)
