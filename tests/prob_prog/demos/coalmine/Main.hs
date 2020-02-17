module CoalMine where
import           Probability

fatalities =
    [ 4
    , 5
    , 4
    , 1
    , 0
    , 4
    , 3
    , 4
    , 0
    , 6
    , 3
    , 3
    , 4
    , 0
    , 2
    , 6
    , 3
    , 3
    , 5
    , 4
    , 5
    , 3
    , 1
    , 4
    , 4
    , 1
    , 5
    , 5
    , 3
    , 4
    , 2
    , 5
    , 2
    , 2
    , 3
    , 4
    , 2
    , 1
    , 3
    , 2
    , 2
    , 1
    , 1
    , 1
    , 1
    , 3
    , 0
    , 0
    , 1
    , 0
    , 1
    , 1
    , 0
    , 0
    , 3
    , 1
    , 0
    , 3
    , 2
    , 2
    , 0
    , 1
    , 1
    , 1
    , 0
    , 1
    , 0
    , 1
    , 0
    , 0
    , 0
    , 2
    , 1
    , 0
    , 0
    , 0
    , 1
    , 1
    , 0
    , 2
    , 3
    , 3
    , 1
    , 1
    , 2
    , 1
    , 1
    , 1
    , 1
    , 2
    , 3
    , 3
    , 0
    , 0
    , 0
    , 1
    , 4
    , 0
    , 0
    , 0
    , 1
    , 0
    , 0
    , 0
    , 0
    , 0
    , 1
    , 0
    , 0
    , 1
    , 0
    , 1
    ]
years = [1851 .. 1962]

model = do
    eta    <- gamma 10.0 20.0

    lambda <- gamma 2.0 eta

    gamm   <- gamma 2.0 eta

    theta  <- uniform 1852.0 1962.0

    return (eta, lambda, gamm, theta)

main = do

    (eta, lambda, gamm, theta) <- random $ model

    let mean year = if (theta > intToDouble year) then lambda else gamm

    observe (independent [ poisson (mean year) | year <- years ]) fatalities

    return ["eta" %=% eta, "lambda" %=% lambda, "gamma" %=% gamm, "theta" %=% theta]
