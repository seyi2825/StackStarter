;; Fundraising & Contribution Logic Smart Contract
;; Manages contributions, tracks backers, and handles dual-token logic (STX + BTC via PoX)

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_CAMPAIGN_NOT_FOUND (err u101))
(define-constant ERR_CAMPAIGN_ENDED (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_CAMPAIGN_ALREADY_EXISTS (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_POX_INTEGRATION_FAILED (err u106))

;; Data Variables
(define-data-var campaign-counter uint u0)

;; Campaign structure
(define-map campaigns
  { campaign-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    target-amount-stx: uint,
    target-amount-btc: uint,
    raised-stx: uint,
    raised-btc: uint,
    end-block: uint,
    is-active: bool,
    created-at: uint
  }
)

;; Backer contributions tracking
(define-map backer-contributions
  { campaign-id: uint, backer: principal }
  {
    stx-amount: uint,
    btc-amount: uint,
    contribution-count: uint,
    first-contribution-block: uint,
    last-contribution-block: uint
  }
)

;; Campaign backers list
(define-map campaign-backers
  { campaign-id: uint }
  { backer-count: uint }
)

;; Individual contribution records
(define-map contribution-records
  { campaign-id: uint, backer: principal, contribution-id: uint }
  {
    amount: uint,
    token-type: (string-ascii 10), ;; "STX" or "BTC"
    block-height: uint,
    timestamp: uint
  }
)

;; PoX integration data
(define-map pox-commitments
  { campaign-id: uint, cycle: uint }
  {
    committed-amount: uint,
    reward-address: (buff 20),
    lock-period: uint
  }
)

;; Create a new fundraising campaign
(define-public (create-campaign 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (target-stx uint)
  (target-btc uint)
  (duration-blocks uint))
  (let
    (
      (campaign-id (+ (var-get campaign-counter) u1))
      (end-block (+ stacks-block-height duration-blocks))
    )
    (asserts! (> target-stx u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration-blocks u0) ERR_INVALID_AMOUNT)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        target-amount-stx: target-stx,
        target-amount-btc: target-btc,
        raised-stx: u0,
        raised-btc: u0,
        end-block: end-block,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    
    (map-set campaign-backers
      { campaign-id: campaign-id }
      { backer-count: u0 }
    )
    
    (var-set campaign-counter campaign-id)
    (ok campaign-id)
  )
)

;; Contribute STX to a campaign
(define-public (contribute (campaign-id uint) (amount uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
      (current-backer-info (default-to 
        { stx-amount: u0, btc-amount: u0, contribution-count: u0, first-contribution-block: u0, last-contribution-block: u0 }
        (map-get? backer-contributions { campaign-id: campaign-id, backer: tx-sender })
      ))
      (is-new-backer (is-eq (get contribution-count current-backer-info) u0))
      (current-backers (default-to { backer-count: u0 } (map-get? campaign-backers { campaign-id: campaign-id })))
    )
    
    ;; Validations
    (asserts! (get is-active campaign) ERR_CAMPAIGN_ENDED)
    (asserts! (< stacks-block-height (get end-block campaign)) ERR_CAMPAIGN_ENDED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    ;; Transfer STX from contributor to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update campaign totals
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { raised-stx: (+ (get raised-stx campaign) amount) })
    )
    
    ;; Update backer information
    (map-set backer-contributions
      { campaign-id: campaign-id, backer: tx-sender }
      {
        stx-amount: (+ (get stx-amount current-backer-info) amount),
        btc-amount: (get btc-amount current-backer-info),
        contribution-count: (+ (get contribution-count current-backer-info) u1),
        first-contribution-block: (if is-new-backer stacks-block-height (get first-contribution-block current-backer-info)),
        last-contribution-block: stacks-block-height
      }
    )
    
    ;; Update backer count if new backer
    (if is-new-backer
      (map-set campaign-backers
        { campaign-id: campaign-id }
        { backer-count: (+ (get backer-count current-backers) u1) }
      )
      true
    )
    
    ;; Record individual contribution
    (map-set contribution-records
      { campaign-id: campaign-id, backer: tx-sender, contribution-id: (get contribution-count current-backer-info) }
      {
        amount: amount,
        token-type: "STX",
        block-height: stacks-block-height,
        timestamp: stacks-block-height ;; Using stacks-block-height as timestamp proxy
      }
    )
    
    (ok amount)
  )
)

;; Contribute BTC via PoX integration
(define-public (contribute-btc 
  (campaign-id uint) 
  (amount uint)
  (reward-address (buff 20))
  (lock-period uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
      (current-backer-info (default-to 
        { stx-amount: u0, btc-amount: u0, contribution-count: u0, first-contribution-block: u0, last-contribution-block: u0 }
        (map-get? backer-contributions { campaign-id: campaign-id, backer: tx-sender })
      ))
      (is-new-backer (is-eq (get contribution-count current-backer-info) u0))
      (current-backers (default-to { backer-count: u0 } (map-get? campaign-backers { campaign-id: campaign-id })))
      (pox-cycle (/ stacks-block-height u2100)) ;; Approximate PoX cycle calculation
    )
    
    ;; Validations
    (asserts! (get is-active campaign) ERR_CAMPAIGN_ENDED)
    (asserts! (< stacks-block-height (get end-block campaign)) ERR_CAMPAIGN_ENDED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= lock-period u1) ERR_INVALID_AMOUNT)
    
    ;; Note: In a real implementation, this would integrate with actual PoX stacking
    ;; For now, we simulate the BTC contribution tracking
    
    ;; Update campaign BTC totals
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { raised-btc: (+ (get raised-btc campaign) amount) })
    )
    
    ;; Update backer information
    (map-set backer-contributions
      { campaign-id: campaign-id, backer: tx-sender }
      {
        stx-amount: (get stx-amount current-backer-info),
        btc-amount: (+ (get btc-amount current-backer-info) amount),
        contribution-count: (+ (get contribution-count current-backer-info) u1),
        first-contribution-block: (if is-new-backer stacks-block-height (get first-contribution-block current-backer-info)),
        last-contribution-block: stacks-block-height
      }
    )
    
    ;; Update backer count if new backer
    (if is-new-backer
      (map-set campaign-backers
        { campaign-id: campaign-id }
        { backer-count: (+ (get backer-count current-backers) u1) }
      )
      true
    )
    
    ;; Record PoX commitment
    (map-set pox-commitments
      { campaign-id: campaign-id, cycle: pox-cycle }
      {
        committed-amount: amount,
        reward-address: reward-address,
        lock-period: lock-period
      }
    )
    
    ;; Record individual contribution
    (map-set contribution-records
      { campaign-id: campaign-id, backer: tx-sender, contribution-id: (get contribution-count current-backer-info) }
      {
        amount: amount,
        token-type: "BTC",
        block-height: stacks-block-height,
        timestamp: stacks-block-height
      }
    )
    
    (ok amount)
  )
)

;; Get total funds raised for a campaign by token type
(define-read-only (get-total-raised (campaign-id uint) (token-type (string-ascii 10)))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign
    (if (is-eq token-type "STX")
      (ok (get raised-stx campaign))
      (if (is-eq token-type "BTC")
        (ok (get raised-btc campaign))
        (ok u0)
      )
    )
    ERR_CAMPAIGN_NOT_FOUND
  )
)

;; Get total funds raised for a campaign (both tokens)
(define-read-only (get-campaign-totals (campaign-id uint))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign
    (ok {
      raised-stx: (get raised-stx campaign),
      raised-btc: (get raised-btc campaign),
      target-stx: (get target-amount-stx campaign),
      target-btc: (get target-amount-btc campaign)
    })
    ERR_CAMPAIGN_NOT_FOUND
  )
)

;; Get backer information for a specific user and campaign
(define-read-only (get-backer-info (campaign-id uint) (backer principal))
  (match (map-get? backer-contributions { campaign-id: campaign-id, backer: backer })
    backer-info
    (ok backer-info)
    (ok { stx-amount: u0, btc-amount: u0, contribution-count: u0, first-contribution-block: u0, last-contribution-block: u0 })
  )
)

;; Get campaign details
(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id })
)

;; Get campaign backer count
(define-read-only (get-backer-count (campaign-id uint))
  (match (map-get? campaign-backers { campaign-id: campaign-id })
    backers (ok (get backer-count backers))
    (ok u0)
  )
)

;; Get contribution record
(define-read-only (get-contribution-record (campaign-id uint) (backer principal) (contribution-id uint))
  (map-get? contribution-records { campaign-id: campaign-id, backer: backer, contribution-id: contribution-id })
)

;; Get PoX commitment info
(define-read-only (get-pox-commitment (campaign-id uint) (cycle uint))
  (map-get? pox-commitments { campaign-id: campaign-id, cycle: cycle })
)

;; Admin function to end a campaign early
(define-public (end-campaign (campaign-id uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator campaign)) ERR_UNAUTHORIZED)
    (asserts! (get is-active campaign) ERR_CAMPAIGN_ENDED)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { is-active: false })
    )
    
    (ok true)
  )
)

;; Withdraw funds (only campaign creator)
(define-public (withdraw-funds (campaign-id uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
      (raised-amount (get raised-stx campaign))
    )
    (asserts! (is-eq tx-sender (get creator campaign)) ERR_UNAUTHORIZED)
    (asserts! (or (not (get is-active campaign)) (>= stacks-block-height (get end-block campaign))) ERR_CAMPAIGN_ENDED)
    (asserts! (> raised-amount u0) ERR_INSUFFICIENT_FUNDS)
    
    ;; Transfer STX to campaign creator
    (try! (as-contract (stx-transfer? raised-amount tx-sender (get creator campaign))))
    
    ;; Reset raised amount
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { raised-stx: u0, is-active: false })
    )
    
    (ok raised-amount)
  )
)

;; Get current campaign counter
(define-read-only (get-campaign-counter)
  (var-get campaign-counter)
)