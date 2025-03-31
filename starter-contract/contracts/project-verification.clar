;; Project Verification and Governance Contract
;; Enables decentralized community voting to verify and list projects

;; Define constants
(define-constant contract-owner tx-sender)

;; Error codes
(define-constant err-not-authorized (err u100))
(define-constant err-project-exists (err u101))
(define-constant err-project-not-found (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-already-voted (err u104))
(define-constant err-voting-closed (err u105))
(define-constant err-insufficient-stake (err u106))
(define-constant err-invalid-vote (err u107))
(define-constant err-not-member (err u108))
(define-constant err-already-verified (err u109))
(define-constant err-voting-period-active (err u110))
(define-constant err-invalid-threshold (err u111))
(define-constant err-invalid-period (err u112))
(define-constant err-already-member (err u113))
(define-constant err-zero-amount (err u114))

;; Project status values
(define-constant status-pending "pending")
(define-constant status-voting "voting")
(define-constant status-verified "verified")
(define-constant status-rejected "rejected")

;; Vote types
(define-constant vote-approve u1)
(define-constant vote-reject u2)
(define-constant vote-abstain u3)

;; Define data maps
(define-map projects
  { id: uint }
  {
    creator: principal,
    name: (string-ascii 64),
    description: (string-ascii 256),
    website: (string-ascii 128),
    repository: (string-ascii 128),
    status: (string-ascii 16),
    votes-for: uint,
    votes-against: uint,
    total-votes: uint,
    voting-start-block: uint,
    voting-end-block: uint,
    created-at: uint,
    verified-at: (optional uint)
  }
)

(define-map project-votes
  { project-id: uint, voter: principal }
  {
    vote-type: uint,
    weight: uint,
    block-height: uint
  }
)

(define-map governance-members
  { member: principal }
  {
    stake: uint,
    reputation: uint,
    joined-at: uint,
    vote-count: uint
  }
)

;; Define variables
(define-data-var project-counter uint u0)
(define-data-var verification-threshold uint u6000) ;; 60% approval needed (basis points)
(define-data-var minimum-votes uint u5) ;; Minimum votes required for verification
(define-data-var voting-period uint u1440) ;; Default voting period in blocks (approx. 10 days at 10 min blocks)
(define-data-var minimum-stake uint u1000000) ;; Minimum stake required to become a governance member (in microSTX)
(define-data-var membership-count uint u0) ;; Total number of governance members

;; Check if caller is contract owner
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

;; Check if caller is project creator
(define-private (is-project-creator (project-id uint))
  (match (map-get? projects { id: project-id })
    project (is-eq tx-sender (get creator project))
    false
  )
)

;; Check if caller is a governance member
(define-private (is-governance-member)
  (match (map-get? governance-members { member: tx-sender })
    member true
    false
  )
)

;; Get member stake
(define-private (get-member-stake (member principal))
  (match (map-get? governance-members { member: member })
    member-data (get stake member-data)
    u0
  )
)

;; Join governance (stake STX to become a member)
(define-public (join-governance (stake-amount uint))
  (let
    (
      (current-block stacks-block-height)
    )
    ;; Validate inputs
    (asserts! (>= stake-amount (var-get minimum-stake)) (err err-insufficient-stake))
    (asserts! (not (is-governance-member)) (err err-already-member))
    
    ;; Transfer STX from member to contract
    (unwrap! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)) (err u100))
    
    ;; Register as governance member
    (map-set governance-members
      { member: tx-sender }
      {
        stake: stake-amount,
        reputation: u0,
        joined-at: current-block,
        vote-count: u0
      }
    )
    
    ;; Increment membership count
    (var-set membership-count (+ (var-get membership-count) u1))
    
    (ok stake-amount)
  )
)

;; Increase governance stake
(define-public (increase-stake (additional-amount uint))
  (let
    (
      (member-data (unwrap! (map-get? governance-members { member: tx-sender }) (err err-not-member)))
      (current-stake (get stake member-data))
    )
    ;; Validate inputs
    (asserts! (> additional-amount u0) (err err-zero-amount))
    
    ;; Transfer additional STX from member to contract
    (try! (stx-transfer? additional-amount tx-sender (as-contract tx-sender)))
    
    ;; Update member stake
    (map-set governance-members
      { member: tx-sender }
      (merge member-data {
        stake: (+ current-stake additional-amount)
      })
    )
    
    (ok (+ current-stake additional-amount))
  )
)

;; Leave governance (withdraw stake)
(define-public (leave-governance)
  (let
    (
      (member-data (unwrap! (map-get? governance-members { member: tx-sender }) (err err-not-member)))
      (stake-amount (get stake member-data))
    )
    ;; Transfer STX back to member
    (try! (as-contract (stx-transfer? stake-amount tx-sender tx-sender)))
    
    ;; Remove from governance members
    (map-delete governance-members { member: tx-sender })
    
    ;; Decrement membership count
    (var-set membership-count (- (var-get membership-count) u1))
    
    (ok stake-amount)
  )
)

;; Submit a new project for verification
(define-public (submit-project (name (string-ascii 64)) (description (string-ascii 256)) (website (string-ascii 128)) (repository (string-ascii 128)))
  (let
    (
      (project-id (+ (var-get project-counter) u1))
      (current-block stacks-block-height)
    )
    ;; Create project in pending status
    (map-set projects
      { id: project-id }
      {
        creator: tx-sender,
        name: name,
        description: description,
        website: website,
        repository: repository,
        status: status-pending,
        votes-for: u0,
        votes-against: u0,
        total-votes: u0,
        voting-start-block: u0,
        voting-end-block: u0,
        created-at: current-block,
        verified-at: none
      }
    )
    
    (var-set project-counter project-id)
    (ok project-id)
  )
)