;; StxJackpot_Skeleton.clar
;; A distinguished Clarity smart contract skeleton designed for submission to
;; stacks.org/code-for-STX via GitHub. This skeleton provides a secure, auditable
;; and extensible base for a prize/lottery-like STX Jackpot dApp. It focuses on
;; clarity (pun intended), best-practices, and modularity so implementers can
;; add their tokenomics and UI quickly.
;;
;; NOTE: This is only a skeleton - business logic, parameters, and economic
;; safety should be carefully reviewed and audited before deploying.

;; -------------------------
;; License & Metadata
;; -------------------------
;; SPDX-License-Identifier: MIT
;; Author: <your-name-or-github-handle>
;; Project: STX Jackpot (skeleton)
;; Purpose: Base contract to handle deposits, rounds, deterministic/random
;; selection (off-chain oracle or VRF), reward distribution, admin controls.

;; -------------------------
;; Errors
;; -------------------------
(define-constant ERR_NOT_CONTRACT_OWNER (err u100))
(define-constant ERR_PAUSED (err u101))
(define-constant ERR_ZERO_AMOUNT (err u102))
(define-constant ERR_NO_ACTIVE_ROUND (err u103))
(define-constant ERR_UNAUTHORIZED (err u104))
(define-constant ERR_INVALID_ROUND (err u105))
(define-constant ERR_TRANSFER_FAILED (err u106))
(define-constant ERR_ALREADY_INITIALIZED (err u107))
(define-constant ERR_NOT_INITIALIZED (err u108))

;; -------------------------
;; Contract Constants
;; -------------------------
(define-constant CONTRACT_VERSION u1)
(define-constant ADMIN_FEE_BP u200) ;; admin fee in basis points (2.00%)

;; -------------------------
;; Data Maps & Variables
;; -------------------------
;; Owner of the contract (admin)
(define-data-var owner principal tx-sender)

;; Pause flag
(define-data-var paused bool false)

;; Round counter
(define-data-var current-round uint u0)

;; round -> total stx deposited
(define-map round-total {round: uint} {total: uint})

;; round -> list of participants stored as map participant-index -> principal
;; Note: Clarity doesn't have dynamic arrays; we store a map and a length counter
(define-map round-participant {round: uint, index: uint} {participant: principal})
(define-map round-participant-count {round: uint} {count: uint})

;; participant -> (round -> contributed amount)
(define-map contribution {round: uint, participant: principal} {amount: uint})

;; pending rewards for principals (withdraw pattern)
(define-map pending-rewards {participant: principal} {amount: uint})

;; Oracle/VRF result storage (if using on-chain callback)
(define-map round-winner {round: uint} {winner: principal})

;; -------------------------
;; Events (simple logging)
;; -------------------------
(define-read-only (contract-info)
  (tuple (version CONTRACT_VERSION) (owner (var-get owner)) (paused (var-get paused))))

(define-private (emit-event (name (buff 32)))
  ;; Minimal event pattern - replace with a richer event system if desired.
  (ok name))

;; -------------------------
;; Modifiers / Helpers
;; -------------------------
(define-private (assert-owner)
  (if (is-eq tx-sender (var-get owner))
    (ok true)
    ERR_NOT_CONTRACT_OWNER))

(define-private (assert-not-paused)
  (if (not (var-get paused))
    (ok true)
    ERR_PAUSED))

;; Safe STX transfer wrapper - callers should handle the returned result
(define-private (safe-stx-transfer (amount uint) (recipient principal))
  (match (stx-transfer? amount tx-sender recipient)
    success (ok success)
    failure ERR_TRANSFER_FAILED))

;; -------------------------
;; Init / Admin Functions
;; -------------------------
(define-public (initialize (new-owner principal))
  (let ((current-round-val (var-get current-round)))
    (if (is-eq current-round-val u0)
      ;; initialize only if not already initialized (current-round == 0 is allowed)
      (begin
        (asserts! (is-standard new-owner) ERR_NOT_CONTRACT_OWNER)
        (var-set owner new-owner)
        (var-set paused false)
        (ok (tuple (initialized true) (owner new-owner))))
      ;; else return already initialized error
      ERR_ALREADY_INITIALIZED)))

(define-public (set-pause (p bool))
  (begin
    (try! (assert-owner))
    (var-set paused p)
    (ok p)))

(define-public (transfer-ownership (new-owner principal))
  (begin
    (try! (assert-owner))
    (asserts! (is-standard new-owner) ERR_NOT_CONTRACT_OWNER)
    (var-set owner new-owner)
    (ok new-owner)))

;; -------------------------
;; Core Flow: Rounds, Deposits, Participation
;; -------------------------
;; Start a new round (owner only)
(define-public (start-round)
  (begin
    (try! (assert-owner))
    (var-set current-round (+ (var-get current-round) u1))
    (ok (var-get current-round))))

;; Deposit STX into the current round
(define-public (deposit (amount uint))
  (begin
    (try! (assert-not-paused))
    (let ((r (var-get current-round)))
      (if (is-eq r u0)
        ERR_NO_ACTIVE_ROUND
        (if (<= amount u0)
          ERR_ZERO_AMOUNT
          (begin
            (try! (safe-stx-transfer amount (as-contract tx-sender)))
            (let ((old-total (default-to u0 (get total (map-get? round-total {round: r})))))
              (map-set round-total {round: r} {total: (+ old-total amount)}))
            (let ((count-entry (default-to {count: u0} (map-get? round-participant-count {round: r}))))
              (let ((idx (get count count-entry)))
                (map-set round-participant {round: r, index: idx} {participant: tx-sender})
                (map-set round-participant-count {round: r} {count: (+ idx u1)})
                (let ((prev (default-to u0 (get amount (map-get? contribution {round: r, participant: tx-sender})))))
                  (map-set contribution {round: r, participant: tx-sender} {amount: (+ prev amount)})
                  (ok (tuple (round r) (depositor tx-sender) (amount amount) (index idx))))))))))))

;; Read-only: get participant count for a round
(define-read-only (get-participant-count (r uint))
  (get count (default-to {count: u0} (map-get? round-participant-count {round: r}))))

;; -------------------------
;; Winner selection (oracle-assisted)
;; -------------------------
;; This skeleton assumes winner selection is performed off-chain (e.g. secured VRF or oracle)
;; and then the admin calls `set-winner` with the chosen principal. Implementers SHOULD
;; replace/update this with secure randomness and on-chain verification where possible.

(define-public (set-winner (r uint) (winner principal))
  (begin
    (try! (assert-owner))
    (asserts! (is-standard winner) ERR_NOT_CONTRACT_OWNER)
    (let ((w winner))
      (match (map-get? round-total {round: r})
        total-entry
        (begin
          (map-set round-winner {round: r} {winner: w})
          ;; compute payout and set pending reward
          (let ((total (get total total-entry)))
            ;; admin fee
            (let ((fee (/ (* total ADMIN_FEE_BP) u10000)))
              (let ((reward (- total fee)))
                ;; push to pending-winner
                (let ((current-winner-rewards (get amount (default-to {amount: u0} (map-get? pending-rewards {participant: w})))))
                  (map-set pending-rewards {participant: w} {amount: (+ current-winner-rewards reward)}))
                ;; record admin fee under owner pending
                (let ((current-owner-rewards (get amount (default-to {amount: u0} (map-get? pending-rewards {participant: (var-get owner)})))))
                  (map-set pending-rewards {participant: (var-get owner)} {amount: (+ current-owner-rewards fee)}))
                (ok (tuple (round r) (winner w) (reward reward) (fee fee)))))))
        ERR_INVALID_ROUND))))

;; -------------------------
;; Withdraw pattern
;; -------------------------
(define-public (claim-reward)
  (begin
    (let ((entry (default-to {amount: u0} (map-get? pending-rewards {participant: tx-sender}))))
      (let ((amt (get amount entry)))
        (if (<= amt u0)
          ERR_ZERO_AMOUNT
          (begin
            ;; zero out before transfer (checks-effects-interactions)
            (map-set pending-rewards {participant: tx-sender} {amount: u0})
            (match (as-contract (stx-transfer? amt tx-sender tx-sender))
              success (ok amt)
              error (begin
                (map-set pending-rewards {participant: tx-sender} {amount: amt})
                ERR_TRANSFER_FAILED))))))))

;; -------------------------
;; Emergency & Cleanup
;; -------------------------
(define-public (emergency-withdraw (recipient principal) (amount uint))
  (if (is-standard recipient)
    (begin
      (try! (assert-owner))
      ;; Transfer from contract address to recipient
      (try! (safe-stx-transfer amount recipient))
      (ok true))
    ERR_NOT_CONTRACT_OWNER))

;; -------------------------
;; Read-only helpers for UI
;; -------------------------
(define-read-only (get-round-total (r uint))
  (get total (default-to {total: u0} (map-get? round-total {round: r}))))

(define-read-only (get-contribution (r uint) (p principal))
  (get amount (default-to {amount: u0} (map-get? contribution {round: r, participant: p}))))

(define-read-only (get-winner (r uint))
  (match (map-get? round-winner {round: r})
    entry (ok (get winner entry))
    (err ERR_INVALID_ROUND)))

(define-read-only (get-pending-reward (p principal))
  (get amount (default-to {amount: u0} (map-get? pending-rewards {participant: p}))))

;; -------------------------
;; Tests & Development Notes
;; -------------------------
;; - Use Clarinet for local testing.
;; - Replace off-chain winner selection with verified randomness if possible.
;; - Consider re-entrancy patterns and always follow checks-effects-interactions.
;; - Add time-locks, minimum/maximum deposit, anti-sybil mechanics (tickets, KYC),
;;   and fee routing according to your business rules.

;; -------------------------
;; End of skeleton
;; -------------------------
