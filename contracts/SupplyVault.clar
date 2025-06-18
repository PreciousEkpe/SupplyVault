;; SupplyVault: Decentralized Supply Chain Verification & Traceability Protocol
;; Version: 1.0.0
;; A protocol that enables manufacturers to register product batches, stake tokens for quality assurance,
;; and earn verifiable on-chain authenticity certificates upon validation

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-BATCH-NOT-FOUND (err u2))
(define-constant ERR-INVALID-VALUE (err u3))
(define-constant ERR-INVALID-TIMELINE (err u4))
(define-constant ERR-INVALID-IDENTIFIER (err u5))
(define-constant ERR-INVALID-SPECIFICATIONS (err u6))
(define-constant ERR-BATCH-INACTIVE (err u7))
(define-constant ERR-ALREADY-TRACKED (err u8))
(define-constant ERR-NOT-TRACKED (err u9))
(define-constant ERR-INSUFFICIENT-FUNDS (err u10))
(define-constant ERR-BATCH-NOT-VERIFIED (err u11))
(define-constant ERR-ALREADY-AUTHENTICATED (err u12))
(define-constant ERR-INVALID-CATEGORY (err u13))
(define-constant ERR-INVALID-GRADE (err u14))
(define-constant ERR-TRACKING-EXPIRED (err u15))
(define-constant ERR-INVALID-AUDIT (err u16))

;; Constants
(define-constant MIN-VALUE u1000000) ;; 1 STX minimum
(define-constant MAX-VALUE u1000000000000) ;; 1M STX maximum
(define-constant MIN-TIMELINE u86400) ;; 1 day minimum
(define-constant MAX-TIMELINE u31536000) ;; 1 year maximum
(define-constant NETWORK-FEE-PERCENT u5) ;; 5% network fee
(define-constant VERIFICATION-THRESHOLD u80) ;; 80% minimum audit for authentication

;; Data variables
(define-data-var next-batch-id uint u1)
(define-data-var next-tracking-id uint u1)
(define-data-var network-treasury principal tx-sender)
(define-data-var total-network-fees uint u0)

;; Product batch structure
(define-map product-batches
    uint
    {
        manufacturer: principal,
        batch-identifier: (string-utf8 100),
        batch-specifications: (string-utf8 500),
        product-category: (string-utf8 20),
        quality-grade: (string-utf8 10),
        batch-value: uint,
        assurance-bond: uint,
        validity-timeline: uint,
        is-active: bool,
        total-trackers: uint,
        total-authenticated: uint,
        created-at: uint
    }
)

;; Supply tracking structure
(define-map supply-tracking
    uint
    {
        tracker: principal,
        batch-id: uint,
        tracked-at: uint,
        expires-at: uint,
        audit-score: uint,
        is-verified: bool,
        is-authenticated: bool,
        bond-staked: uint
    }
)

;; Tracker batch mapping
(define-map tracker-batch-records
    { tracker: principal, batch-id: uint }
    uint
)

;; Authenticity certificates
(define-map authenticity-certificates
    { tracker: principal, batch-id: uint }
    {
        authenticated-at: uint,
        final-audit: uint,
        certificate-hash: (string-utf8 64)
    }
)

;; Private validation functions
(define-private (validate-category (product-category (string-utf8 20)))
    (or 
        (is-eq product-category u"Electronics")
        (is-eq product-category u"Pharmaceuticals")
        (is-eq product-category u"Food & Beverage")
        (is-eq product-category u"Automotive")
        (is-eq product-category u"Textiles")
        (is-eq product-category u"Chemicals")
        (is-eq product-category u"Luxury Goods")
        (is-eq product-category u"Raw Materials")
    )
)

(define-private (validate-grade (quality-grade (string-utf8 10)))
    (or 
        (is-eq quality-grade u"Standard")
        (is-eq quality-grade u"Premium")
        (is-eq quality-grade u"Enterprise")
        (is-eq quality-grade u"Military")
    )
)

(define-private (validate-text-length (text (string-utf8 500)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    )
)

(define-private (calculate-network-fee (amount uint))
    (/ (* amount NETWORK-FEE-PERCENT) u100)
)

(define-private (calculate-manufacturer-amount (amount uint))
    (- amount (calculate-network-fee amount))
)

(define-private (validate-bond-amount (bond-amount uint))
    (and (>= bond-amount u0) (<= bond-amount u100000000000)) ;; Max 100k STX bond
)

(define-private (validate-certificate-hash (certificate-hash (string-utf8 64)))
    (and (>= (len certificate-hash) u32) (<= (len certificate-hash) u64))
)

;; Public functions

;; Register a new product batch
(define-public (register-product-batch 
    (batch-identifier (string-utf8 100))
    (batch-specifications (string-utf8 500))
    (product-category (string-utf8 20))
    (quality-grade (string-utf8 10))
    (batch-value uint)
    (assurance-bond uint)
    (validity-timeline uint)
)
    (let
        (
            (batch-id (var-get next-batch-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        ;; Validate inputs
        (asserts! (validate-text-length batch-identifier u5 u100) ERR-INVALID-IDENTIFIER)
        (asserts! (validate-text-length batch-specifications u20 u500) ERR-INVALID-SPECIFICATIONS)
        (asserts! (validate-category product-category) ERR-INVALID-CATEGORY)
        (asserts! (validate-grade quality-grade) ERR-INVALID-GRADE)
        (asserts! (and (>= batch-value MIN-VALUE) (<= batch-value MAX-VALUE)) ERR-INVALID-VALUE)
        (asserts! (and (>= validity-timeline MIN-TIMELINE) (<= validity-timeline MAX-TIMELINE)) ERR-INVALID-TIMELINE)
        (asserts! (validate-bond-amount assurance-bond) ERR-INVALID-VALUE)
        
        ;; Create batch
        (map-set product-batches batch-id {
            manufacturer: tx-sender,
            batch-identifier: batch-identifier,
            batch-specifications: batch-specifications,
            product-category: product-category,
            quality-grade: quality-grade,
            batch-value: batch-value,
            assurance-bond: assurance-bond,
            validity-timeline: validity-timeline,
            is-active: true,
            total-trackers: u0,
            total-authenticated: u0,
            created-at: current-time
        })
        
        (var-set next-batch-id (+ batch-id u1))
        (ok batch-id)
    )
)

;; Track product batch with assurance bond
(define-public (track-batch (batch-id uint))
    (let
        (
            (batch (unwrap! (map-get? product-batches batch-id) ERR-BATCH-NOT-FOUND))
            (tracking-id (var-get next-tracking-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (expires-at (+ current-time (get validity-timeline batch)))
            (total-cost (+ (get batch-value batch) (get assurance-bond batch)))
            (network-fee (calculate-network-fee (get batch-value batch)))
            (manufacturer-amount (calculate-manufacturer-amount (get batch-value batch)))
        )
        ;; Validate batch is active
        (asserts! (get is-active batch) ERR-BATCH-INACTIVE)
        
        ;; Check if already tracking
        (asserts! (is-none (map-get? tracker-batch-records { tracker: tx-sender, batch-id: batch-id })) ERR-ALREADY-TRACKED)
        
        ;; Transfer payment to manufacturer and network fee
        (try! (stx-transfer? manufacturer-amount tx-sender (get manufacturer batch)))
        (try! (stx-transfer? network-fee tx-sender (var-get network-treasury)))
        
        ;; Lock assurance bond (simulated by requiring balance)
        (asserts! (>= (stx-get-balance tx-sender) (get assurance-bond batch)) ERR-INSUFFICIENT-FUNDS)
        
        ;; Create tracking record
        (map-set supply-tracking tracking-id {
            tracker: tx-sender,
            batch-id: batch-id,
            tracked-at: current-time,
            expires-at: expires-at,
            audit-score: u0,
            is-verified: false,
            is-authenticated: false,
            bond-staked: (get assurance-bond batch)
        })
        
        ;; Map tracker to record
        (map-set tracker-batch-records { tracker: tx-sender, batch-id: batch-id } tracking-id)
        
        ;; Update batch stats
        (map-set product-batches batch-id (merge batch { total-trackers: (+ (get total-trackers batch) u1) }))
        
        ;; Update network fees
        (var-set total-network-fees (+ (var-get total-network-fees) network-fee))
        (var-set next-tracking-id (+ tracking-id u1))
        
        (ok tracking-id)
    )
)

;; Update audit progress
(define-public (update-audit (batch-id uint) (audit-score uint))
    (let
        (
            (tracking-id (unwrap! (map-get? tracker-batch-records { tracker: tx-sender, batch-id: batch-id }) ERR-NOT-TRACKED))
            (tracking-record (unwrap! (map-get? supply-tracking tracking-id) ERR-NOT-TRACKED))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        ;; Validate tracking is active
        (asserts! (< current-time (get expires-at tracking-record)) ERR-TRACKING-EXPIRED)
        (asserts! (<= audit-score u100) ERR-INVALID-AUDIT)
        (asserts! (>= audit-score (get audit-score tracking-record)) ERR-INVALID-AUDIT)
        
        ;; Update audit
        (map-set supply-tracking tracking-id (merge tracking-record { 
            audit-score: audit-score,
            is-verified: (>= audit-score u100)
        }))
        
        (ok true)
    )
)

;; Issue authenticity certificate
(define-public (issue-authenticity-certificate (batch-id uint) (certificate-hash (string-utf8 64)))
    (let
        (
            (tracking-id (unwrap! (map-get? tracker-batch-records { tracker: tx-sender, batch-id: batch-id }) ERR-NOT-TRACKED))
            (tracking-record (unwrap! (map-get? supply-tracking tracking-id) ERR-NOT-TRACKED))
            (batch (unwrap! (map-get? product-batches batch-id) ERR-BATCH-NOT-FOUND))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (validated-batch-id (get batch-id tracking-record))
            (validated-hash certificate-hash)
        )
        ;; Additional validations
        (asserts! (validate-certificate-hash certificate-hash) ERR-INVALID-SPECIFICATIONS)
        (asserts! (is-eq batch-id validated-batch-id) ERR-BATCH-NOT-FOUND)
        
        ;; Validate verification and audit
        (asserts! (get is-verified tracking-record) ERR-BATCH-NOT-VERIFIED)
        (asserts! (>= (get audit-score tracking-record) VERIFICATION-THRESHOLD) ERR-BATCH-NOT-VERIFIED)
        (asserts! (not (get is-authenticated tracking-record)) ERR-ALREADY-AUTHENTICATED)
        
        ;; Issue certificate
        (map-set authenticity-certificates { tracker: tx-sender, batch-id: validated-batch-id } {
            authenticated-at: current-time,
            final-audit: (get audit-score tracking-record),
            certificate-hash: validated-hash
        })
        
        ;; Update tracking record
        (map-set supply-tracking tracking-id (merge tracking-record { is-authenticated: true }))
        
        ;; Update batch stats
        (map-set product-batches validated-batch-id (merge batch { total-authenticated: (+ (get total-authenticated batch) u1) }))
        
        ;; Return bond to tracker (simulated)
        (ok true)
    )
)

;; Deactivate batch (manufacturer only)
(define-public (deactivate-batch (batch-id uint))
    (let
        (
            (batch (unwrap! (map-get? product-batches batch-id) ERR-BATCH-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get manufacturer batch)) ERR-NOT-AUTHORIZED)
        (map-set product-batches batch-id (merge batch { is-active: false }))
        (ok true)
    )
)

;; Read-only functions

(define-read-only (get-product-batch (batch-id uint))
    (map-get? product-batches batch-id)
)

(define-read-only (get-supply-tracking (tracking-id uint))
    (map-get? supply-tracking tracking-id)
)

(define-read-only (get-tracker-record (tracker principal) (batch-id uint))
    (match (map-get? tracker-batch-records { tracker: tracker, batch-id: batch-id })
        tracking-id (map-get? supply-tracking tracking-id)
        none
    )
)

(define-read-only (get-authenticity-certificate (tracker principal) (batch-id uint))
    (map-get? authenticity-certificates { tracker: tracker, batch-id: batch-id })
)

(define-read-only (is-tracker-authenticated (tracker principal) (batch-id uint))
    (is-some (map-get? authenticity-certificates { tracker: tracker, batch-id: batch-id }))
)

(define-read-only (get-batch-stats (batch-id uint))
    (match (map-get? product-batches batch-id)
        batch {
            total-trackers: (get total-trackers batch),
            total-authenticated: (get total-authenticated batch),
            authentication-rate: (if (> (get total-trackers batch) u0)
                (/ (* (get total-authenticated batch) u100) (get total-trackers batch))
                u0
            )
        }
        { total-trackers: u0, total-authenticated: u0, authentication-rate: u0 }
    )
)

(define-read-only (get-network-stats)
    {
        total-batches: (- (var-get next-batch-id) u1),
        total-tracking-records: (- (var-get next-tracking-id) u1),
        total-network-fees: (var-get total-network-fees),
        network-treasury: (var-get network-treasury)
    }
)

(define-read-only (calculate-batch-cost (batch-id uint))
    (match (map-get? product-batches batch-id)
        batch {
            value: (get batch-value batch),
            bond: (get assurance-bond batch),
            total: (+ (get batch-value batch) (get assurance-bond batch)),
            network-fee: (calculate-network-fee (get batch-value batch)),
            manufacturer-amount: (calculate-manufacturer-amount (get batch-value batch))
        }
        { value: u0, bond: u0, total: u0, network-fee: u0, manufacturer-amount: u0 }
    )
)