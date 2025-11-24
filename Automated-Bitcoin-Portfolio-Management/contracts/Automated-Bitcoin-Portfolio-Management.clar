
;; title: Automated-Bitcoin-Portfolio-Management
;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-asset-id (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-asset-exists (err u104))
(define-constant err-asset-not-exists (err u105))
(define-constant err-invalid-threshold (err u106))
(define-constant err-invalid-allocation (err u107))
(define-constant err-invalid-risk-level (err u108))
(define-constant err-rebalance-in-progress (err u109))
(define-constant err-no-liquidity (err u110))


;; Define risk levels (conservative, moderate, aggressive)
(define-constant risk-conservative u1)
(define-constant risk-moderate u2)
(define-constant risk-aggressive u3)

;; Data maps and variables
(define-data-var rebalancing-in-progress bool false)
(define-data-var rebalance-frequency uint u30) ;; Default 30 days
(define-data-var last-rebalance-block uint u0)
(define-data-var default-risk-level uint risk-moderate)
(define-data-var max-slippage-percentage uint u2) ;; Default 2%
(define-data-var minimum-rebalance-threshold uint u5) ;; Only rebalance if drift > 5%
(define-data-var performance-fee-percentage uint u2) ;; 2% fee on profits

;; Keep track of all registered assets
(define-map assets 
  { asset-id: uint } 
  { 
    name: (string-ascii 32),
    token-contract: principal,
    token-id: (optional uint),
    is-yield-bearing: bool,
    yield-source: (optional principal),
    current-yield-percentage: uint,
    last-yield-claim-block: uint
  }
)

;; Keep track of portfolio composition/allocations by risk level
(define-map risk-allocations
  { risk-level: uint, asset-id: uint }
  { target-percentage: uint }
)

;; User portfolios
(define-map user-portfolios
  { user: principal }
  {
    risk-level: uint,
    total-btc-value: uint,
    last-rebalance-block: uint,
    custom-allocations: bool,
    active: bool
  }
)

;; User asset balances
(define-map user-asset-balances
  { user: principal, asset-id: uint }
  { amount: uint }
)

;; Historical performance tracking
(define-map portfolio-performance
  { user: principal, timestamp: uint }
  { 
    btc-value: uint,
    percentage-change: int
  }
)

;; Public functions

;; Initialize or update a user's portfolio
(define-public (initialize-portfolio (risk-level uint))
  (begin
    (asserts! (or (is-eq risk-level risk-conservative) 
                (is-eq risk-level risk-moderate) 
                (is-eq risk-level risk-aggressive)) 
            err-invalid-risk-level)
    (map-set user-portfolios 
      { user: tx-sender }
      {
        risk-level: risk-level,
        total-btc-value: u0,
        last-rebalance-block: stacks-block-height,
        custom-allocations: false,
        active: true
      })
    (ok true)))

;; Deposit an asset into the portfolio
(define-public (deposit-asset (asset-id uint) (amount uint))
  (let (
    (asset-info (unwrap! (map-get? assets { asset-id: asset-id }) err-asset-not-exists))
    (portfolio (unwrap! (map-get? user-portfolios { user: tx-sender }) err-invalid-risk-level))
    (token-contract (get token-contract asset-info))
    (token-id (get token-id asset-info))
    (current-balance (default-to u0 (get amount (map-get? user-asset-balances { user: tx-sender, asset-id: asset-id }))))
  )
    (asserts! (> amount u0) err-invalid-amount)
    
    
    ;; Update user's balance
    (map-set user-asset-balances 
      { user: tx-sender, asset-id: asset-id }
      { amount: (+ current-balance amount) })
    
    ;; Update portfolio total value - in a real implementation, you'd calculate BTC value
    (map-set user-portfolios
      { user: tx-sender }
      (merge portfolio { total-btc-value: (+ (get total-btc-value portfolio) amount) }))
    
    (ok true)))

;; Withdraw an asset from the portfolio
(define-public (withdraw-asset (asset-id uint) (amount uint))
  (let (
    (asset-info (unwrap! (map-get? assets { asset-id: asset-id }) err-asset-not-exists))
    (portfolio (unwrap! (map-get? user-portfolios { user: tx-sender }) err-invalid-risk-level))
    (token-contract (get token-contract asset-info))
    (token-id (get token-id asset-info))
    (current-balance (default-to u0 (get amount (map-get? user-asset-balances { user: tx-sender, asset-id: asset-id }))))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= current-balance amount) err-insufficient-balance)
    
    ;; Update user's balance
    (map-set user-asset-balances 
      { user: tx-sender, asset-id: asset-id }
      { amount: (- current-balance amount) })
    
    ;; Update portfolio total value
    (map-set user-portfolios
      { user: tx-sender }
      (merge portfolio { total-btc-value: (- (get total-btc-value portfolio) amount) }))
    
    (ok true)))
