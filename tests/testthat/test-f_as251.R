test_that("mvnprd and sadmvn are equal", {
    A <- rep(2, 3)
    B <- rep(-3, 3)
    BPD <- sqrt(c(0.3, 0.4, 0.5))
    eps <- 1e-06
    INF <- rep(2, 3)
    ierc <- 0
    hinc <- 0
    x1 <- mvnprd(A = A, B = B, BPD = BPD, EPS = eps, INF = INF, IERC = ierc, HINC = hinc)

    sigma <- BPD %*% t(BPD)
    diag(sigma) <- 1

    x2 <- mnormt::sadmvn(lower = B, upper = A, mean = 0, varcov = sigma)
    expect_equal(x1[1], x2[1], tolerance = eps)
})

test_that("mvstud and sadmvn are equal", {
    NDF <- 4
    A <- rep(2, 3)
    B <- rep(-3, 3)
    BPD <- sqrt(c(0.3, 0.4, 0.5))
    eps <- 1e-06
    INF <- rep(2, 3)
    ierc <- 0
    hinc <- 0
    D <- rep(0, 3)
    x1 <- mvstud(NDF = NDF, A = A, B = B, BPD = BPD, D = D, EPS = eps, INF = INF, IERC = ierc, HINC = hinc)
    
    sigma <- BPD %*% t(BPD)
    diag(sigma) <- 1
    
    x2 <- mnormt::sadmvt(lower = B, upper = A, mean = 0, S = sigma, df = NDF)
    expect_equal(x1[1], x2[1], tolerance = eps)
})

test_that("mvstud, sadmvn, and as251StudentT are equal", {
    NDF <- 4
    A <- rep(2, 3)
    B <- rep(-3, 3)
    BPD <- sqrt(c(0.3, 0.4, 0.5))
    eps <- 1e-06
    INF <- rep(2, 3)
    ierc <- 0
    hinc <- 0
    D <- rep(0, 3)
    x1 <- mvstud(NDF = NDF, A = A, B = B, BPD = BPD, D = D, EPS = eps, INF = INF, IERC = ierc, HINC = hinc)
    
    sigma <- BPD %*% t(BPD)
    diag(sigma) <- 1
    
    x2 <- mnormt::sadmvt(lower = B, upper = A, mean = 0, S = sigma, df = NDF)
    
    x3 <- as251StudentT(lower = -3, upper = 2, sigma = sigma, df = NDF, inf = INF, eps = eps, ierc = ierc, hinc = 0)
    
    expect_equal(x1[1], x2[1], tolerance = eps)
    expect_equal(x1[1], x3[1], tolerance = eps)
})

test_that("binary case: sadmvn and as251Normal are equal", {
    frac <- rep(0.7,2)
    sigma <- sqrt(frac) %*% sqrt(t(frac))
    diag(sigma) <- 1
    as251StudentT(lower = -Inf, upper = 2, sigma = sigma, df = 500000) 
        
    eps <- 1e-06
    frac <- rep(0.7, 2)
    sigma <- sqrt(frac) %*% sqrt(t(frac))
    diag(sigma) <- 1
    x1 <- as251Normal(lower = -Inf, upper = 2, sigma = sigma, eps = eps)[1]
    x2 <- mnormt::sadmvn(lower = -Inf, upper = 2, mean = 0, varcov = sigma, abseps = eps)
    expect_equal(x1, x2, tolerance = eps)
})

test_that("binary case: sadmvn and as251StudentT are equal", {
    eps <- 1e-06
    frac <- rep(0.7, 2)
    sigma <- sqrt(frac) %*% sqrt(t(frac))
    diag(sigma) <- 1
    x1 <- mnormt::sadmvt(lower = -Inf, upper = 2, mean = 0, S = sigma, df = 22, abseps = eps)
    
    #as251StudentT(lower = -Inf, upper = 2, sigma = sigma, df = 100, eps = 1e-06)
    
    x2 <- as251StudentT(lower = -Inf, upper = 2, sigma = sigma, df = 22, eps = eps)[1]
    expect_equal(x1, x2, tolerance = eps)
})

test_that("mvnprd, sadmvn, and as251Normal are equal", {
    A <- rep(2, 3)
    B <- rep(-3, 3)
    BPD <- sqrt(c(0.3, 0.4, 0.5))
    eps <- 1e-06
    INF <- rep(2, 3)
    ierc <- 0
    hinc <- 0
    x1 <- mvnprd(A = A, B = B, BPD = BPD, EPS = eps, INF = INF, IERC = ierc, HINC = hinc)

    sigma <- BPD %*% t(BPD)
    diag(sigma) <- 1

    x2 <- mnormt::sadmvn(lower = B, upper = A, mean = 0, varcov = sigma)
    
    x3 <- as251Normal(lower = -3, upper = 2, sigma = sigma, inf = INF, eps = eps, ierc = ierc, hinc = 0)
    
    expect_equal(x1[1], x2[1], tolerance = eps)
    expect_equal(x1[1], x3[1], tolerance = eps)
})

test_that("mvnprd, sadmvn, and as251Normal are equal for -Inf lower bound", {
    A <- rep(2, 3)
    B <- rep(-Inf, 3)
    BPD <- sqrt(c(0.3, 0.4, 0.6))
    eps <- 1e-06
    INF <- rep(1, 3)
    ierc <- 0
    hinc <- 0
    x1 <- mvnprd(A = A, B = B, BPD = BPD, EPS = eps, INF = INF, IERC = ierc, HINC = hinc)

    sigma <- BPD %*% t(BPD)
    diag(sigma) <- 1

    x2 <- mnormt::sadmvn(lower = B, upper = A, mean = 0, varcov = sigma)
    
    x3 <- as251Normal(lower = -Inf, upper = 2, sigma = sigma)
    
    expect_equal(x1[1], x2[1], tolerance = eps)
    expect_equal(x1[1], x3[1], tolerance = eps)
})

test_that(".sigmaToBPD works correctly", {
    bpd <- c(0.3, 0.4, 0.5) 
    sigma <- bpd %*% t(bpd)
    diag(sigma) <- 1
    for (i in 1:1000) {
        n <- 9
        bpd <- runif(n, 0, 5)
        sigma <- bpd %*% t(bpd)
        diag(sigma) <- 1  
        expect_equal(.sigmaToBPD(sigma), bpd, tolerance = 1e-12)
    }
})
