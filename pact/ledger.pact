(namespace (read-msg 'ns))
(define-keyset 'babena-admin (read-keyset "babena-admin"))
(module babena-ledger GOVERNANCE

  @model
    [
      (defproperty valid-account (account:string)
          (> (length account) 2))
    ]

  (use util.fungible-util)
  (use kip.token-manifest)

  (implements kip.poly-fungible-v2)
  (use kip.poly-fungible-v2 [account-details sender-balance-change receiver-balance-change])
  (use free.babena-pre-sale)

  ;;
  ;; Tables/Schemas
  ;;

  (deftable ledger:{account-details})

  (defschema token-schema
    id:string
    manifest:object{manifest}
    precision:integer
    supply:decimal
    policy:module{kip.token-policy-v1}
  )

  (deftable tokens:{token-schema})

  (defschema collection-schema
    total-supply:integer
    name:string
    symbol:string
    creator:string
    policy:module{kip.token-policy-v1}
    guard:guard
    token:object{kip.token-policy-v1.token-info}
  )

  (deftable collections:{collection-schema})

  (defschema policy-info
    policy:module{kip.token-policy-v1}
    token:object{kip.token-policy-v1.token-info}
  )

  ;;
  ;; Capabilities
  ;;

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'babena-admin)))

  ;;
  ;; poly-fungible-v2 caps
  ;;

  (defcap TRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-unit id amount)
    (enforce (> amount 0.0) "Amount must be positive")
    (compose-capability (DEBIT id sender))
    (compose-capability (CREDIT id receiver))
  )

  (defcap XTRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      target-chain:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce false "cross chain not supported")
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )

  (defcap SUPPLY:bool (id:string supply:decimal)
    @doc " Emitted when supply is updated, if supported."
    @event true
  )

  (defcap TOKEN:bool (id:string precision:integer supply:decimal policy:module{kip.token-policy-v1})
    @event
    true
  )

  (defcap RECONCILE:bool
    ( token-id:string
      amount:decimal
      sender:object{sender-balance-change}
      receiver:object{receiver-balance-change}
    )
    @doc " For accounting via events. \
         \ sender = {account: '', previous: 0.0, current: 0.0} for mint \
         \ receiver = {account: '', previous: 0.0, current: 0.0} for burn"
    @event
    true
  )

  (defcap ACCOUNT_GUARD:bool (id:string account:string guard:guard)
    @doc " Emitted when ACCOUNT guard is updated."
    @event
    true
  )

  ;;
  ;; Implementation caps
  ;;

  (defcap ROTATE (id:string account:string)
    @doc "Autonomously managed capability for guard rotation"
    @managed
    true)

  (defcap DEBIT (id:string sender:string)
    (enforce-guard (account-guard id sender))
  )

  (defun account-guard:guard (id:string account:string)
    (with-read ledger (key id account) { 'guard := guard } guard)
  )

  (defcap CREDIT (id:string receiver:string) true)

  (defcap UPDATE_SUPPLY ()
    "private cap for update-supply"
    true)

  (defcap MINT (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (enforce (< 0.0 amount) "Amount must be positive")
    (compose-capability (CREDIT id account))
    (compose-capability (UPDATE_SUPPLY))
  )

  (defcap MINT-BABENA (account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (compose-capability (PRIVATE))
    (compose-capability (CREDIT-BABENA account))
    (compose-capability (UPDATE_SUPPLY))
  )

  (defcap PRIVATE ()
    true
  )

  (defcap CREDIT-BABENA (receiver:string) true)

  (defcap MINTED (token-id:string account:string)
    @event
    true
  )

  (defcap COLLECTION (collection:object{collection-schema})
    @event
    true
  )

  (defcap BURN (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (enforce (< 0.0 amount) "Amount must be positive")
    (compose-capability (DEBIT id account))
    (compose-capability (UPDATE_SUPPLY))
  )

  (defcap CLAIM-RESERVED (account:string guard:guard)
  (let ( (accounts:[string] (get-accounts)))
    (enforce (contains account accounts) "You did not reserve any tokens in the pre-sale")
  )
    (let ( (reserved-guard:guard (at 'guard (read-reservation account))))
      (enforce-guard reserved-guard)
    )
  )

  (defun ledger-guard:guard ()
    @doc "Ledger module guard for policies to be able to validate access to policy operations."
    (create-module-guard "ledger-guard")
  )

  (defun create-account:bool
    ( id:string
      account:string
      guard:guard
    )
    (enforce-valid-account account)
    (enforce-reserved account guard)
    (insert ledger (key id account)
      { "balance" : 0.0
      , "guard"   : guard
      , "id" : id
      , "account" : account
      })
    (emit-event (ACCOUNT_GUARD id account guard))
  )

  (defun total-supply:decimal (id:string)
    (with-default-read tokens id
      { 'supply : 0.0 }
      { 'supply := s }
      s)
  )

  (defun create-token:bool
    ( id:string
      precision:integer
      manifest:object{manifest}
      policy:module{kip.token-policy-v1}
      collection-name:string
    )
    (enforce-verify-manifest manifest)
    (enforce-collection collection-name)
    (policy::enforce-init
      { 'id: id, 'supply: 0.0, 'precision: precision, 'manifest: manifest })
      (insert tokens id {
        "id": id,
        "precision": precision,
        "manifest": manifest,
        "supply": 0.0,
        "policy": policy
      })
      (emit-event (TOKEN id precision 0.0 policy))
  )

  (defun create-collection:bool (collection:object{collection-schema})
    (insert collections (at 'name collection)
      {
        'name: (at 'name collection),
        'symbol: (at 'symbol collection),
        'total-supply: (at 'total-supply collection),
        'policy: (at 'policy collection),
        'guard: (at 'guard collection),
        'creator: (at 'creator collection),
        'token: (at 'token collection)
      }
    )
    (emit-event (COLLECTION collection))
  )

  (defun truncate:decimal (id:string amount:decimal)
    (floor amount (precision id))
  )

  (defun rotate:bool (id:string account:string new-guard:guard)
    (with-capability (ROTATE id account)
      (enforce-transfer-policy id account account 0.0)
      (with-read ledger (key id account)
        { "guard" := old-guard }

        (enforce-guard old-guard)
        (update ledger (key id account)
          { "guard" : new-guard })
        (emit-event (ACCOUNT_GUARD id account new-guard)))))

  (defun transfer:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision id) amount)
    (with-capability (TRANSFER id sender receiver amount)
      (enforce-transfer-policy id sender receiver amount)
      (with-read ledger (key id receiver)
        { "guard" := g }
        (let
          ( (sender (debit id sender amount))
            (receiver (credit id receiver g amount))
          )
          (emit-event (RECONCILE id amount sender receiver))
        )
      )
    )
  )

  (defun enforce-transfer-policy
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    (bind (get-policy-info id)
      { 'policy := policy:module{kip.token-policy-v1}
      , 'token := token }
      (policy::enforce-transfer token sender (account-guard id sender) receiver amount))
      )

  (defun enforce-collection:bool (name:string)

    (let ((collections:[string] (keys collections)))
      (enforce (contains name collections) "Collection Not found")
    )

    (with-read collections name
      { 'guard:= collection-guard }
      (enforce-guard collection-guard)
    )
  )

  (defun transfer-create:bool
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision id) amount)

    (with-capability (TRANSFER id sender receiver amount)
      (enforce-transfer-policy id sender receiver amount)
      (let
        (
          (sender (debit id sender amount))
          (receiver (credit id receiver receiver-guard amount))
        )
        (emit-event (RECONCILE id amount sender receiver))
      ))
  )

  (defun mint:bool
    ( id:string
      account:string
      guard:guard
      amount:decimal
    )
    (with-capability (MINT id account amount)
      (bind (get-policy-info id)
        { 'policy := policy:module{kip.token-policy-v1}
        , 'token := token }
        (policy::enforce-mint token account guard amount))
      (let
        (
          (receiver (credit id account guard amount))
          (sender:object{sender-balance-change}
            {'account: "", 'previous: 0.0, 'current: 0.0})
        )
        (emit-event (RECONCILE id amount sender receiver))
        (update-supply id amount)
      ))
  )

  (defun mint-babena:bool
    ( account:string
      guard:guard
      amount:decimal
      collection:string
      count:integer
    )
    (require-capability (PRIVATE))
      (let* ( (collection-info:object{collection-schema} (get-collection collection))
              (policy:module{kip.token-policy-v1} (at 'policy collection-info))
              (token:object{kip.token-policy-v1.token-info} (drop ['policy] (at 'token collection-info)))
              (index:string (at 1 (policy::enforce-mint token account guard amount)))
              (id:string (format-token-id collection index))
        )
        (let
          (
            (receiver (credit id account guard amount))
            (sender:object{sender-balance-change}
              {'account: "", 'previous: 0.0, 'current: 0.0})
          )
          (emit-event (RECONCILE id amount sender receiver))
          ; (update-supply id amount)
        )
        )
  )

  (defun mint-bulk-babena
    ( account:string
      guard:guard
      amount:decimal
      collection:string
      count:integer
    )
    (with-capability (MINT-BABENA account amount)
    (map (mint-babena account guard amount collection) (make-list count 1))
    )
  )

  (defun claim-reserved (account:string guard:guard)
    (with-capability (CLAIM-RESERVED account guard)
      (let ( (babena-reserved:integer (get-babena-reserved account)))
        (mint-bulk-babena account guard 1.0 "Babena" babena-reserved)
      )
    )
  )

  (defun burn:bool
    ( id:string
      account:string
      amount:decimal
    )
    (with-capability (BURN id account amount)
      (bind (get-policy-info id)
        { 'policy := policy:module{kip.token-policy-v1}
        , 'token := token }
        (policy::enforce-burn token account amount))
      (let
        (
          (sender (debit id account amount))
          (receiver:object{receiver-balance-change}
            {'account: "", 'previous: 0.0, 'current: 0.0})
        )
        (emit-event (RECONCILE id amount sender receiver))
        (update-supply id (- amount))
      ))
  )

  (defun debit:object{sender-balance-change}
    ( id:string
      account:string
      amount:decimal
    )

    (require-capability (DEBIT id account))

    (enforce-unit id amount)

    (with-read ledger (key id account)
      { "balance" := old-bal }

      (enforce (<= amount old-bal) "Insufficient funds")

      (let ((new-bal (- old-bal amount)))
        (update ledger (key id account)
          { "balance" : new-bal }
          )
        {'account: account, 'previous: old-bal, 'current: new-bal}
      ))
  )

  (defun credit:object{receiver-balance-change}
    ( id:string
      account:string
      guard:guard
      amount:decimal
    )
    @doc "Credit AMOUNT to ACCOUNT balance"

    @model [ (property (> amount 0.0))
             (property (valid-account account))
           ]
    (enforce-valid-account account)
    (enforce-unit id amount)

    (enforce-one "Capabilities not installed."
      [(require-capability (CREDIT id account))
       (require-capability (CREDIT-BABENA account))])

    (with-default-read ledger (key id account)
      { "balance" : -1.0, "guard" : guard }
      { "balance" := old-bal, "guard" := retg }
      (enforce (= retg guard)
        "account guards do not match")

      (let* ((is-new
             (if (= old-bal -1.0)
                 (enforce-reserved account guard)
                 false))
              (new-bal (if is-new amount (+ old-bal amount)))
            )

      (write ledger (key id account)
        { "balance" : new-bal
        , "guard"   : retg
        , "id"   : id
        , "account" : account
        })
        (if is-new (emit-event (ACCOUNT_GUARD id account retg)) true)
        {'account: account, 'previous: (if is-new 0.0 old-bal), 'current: new-bal}
      ))
  )

  (defun credit-account:object{receiver-balance-change}
    ( id:string
      account:string
      amount:decimal
    )
    @doc "Credit AMOUNT to ACCOUNT"
    (credit id account (account-guard id account) amount)
  )

  (defun update-supply:bool (id:string amount:decimal)
    (require-capability (UPDATE_SUPPLY))
    (with-default-read tokens id
      { 'supply: 0.0 }
      { 'supply := s }
      (let ((new-supply (+ s amount)))
      (update tokens id {'supply: new-supply })
      (emit-event (SUPPLY id new-supply))))
  )

  (defun enforce-unit:bool (id:string amount:decimal)
    (let ((p (precision id)))
    (enforce
      (= (floor amount p)
         amount)
      "precision violation"))
  )

  (defun precision:integer (id:string)
    (with-default-read tokens id
      {'precision: 0}
      {'precision:= p}
    p)
  )

  (defpact transfer-crosschain:bool
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step (format "{}" [(enforce false "cross chain not supported")]) false))

  ;;
  ;; sale
  ;;

  (defcap SALE:bool
    (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc "Wrapper cap/event of SALE of token ID by SELLER of AMOUNT until TIMEOUT block height."
    @event
    (enforce (> amount 0.0) "Amount must be positive")
    (compose-capability (OFFER id seller amount timeout))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap OFFER:bool
    (id:string seller:string amount:decimal timeout:integer)
    @doc "Managed cap for SELLER offering AMOUNT of token ID until TIMEOUT."
    @managed
    (enforce (sale-active timeout) "SALE: invalid timeout")
    (compose-capability (DEBIT id seller))
    (compose-capability (CREDIT id (sale-account)))
  )

  (defcap WITHDRAW:bool
    (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc "Withdraws offer SALE from SELLER of AMOUNT of token ID after timeout."
    @event
    (enforce (not (sale-active timeout)) "WITHDRAW: still active")
    (compose-capability (DEBIT id (sale-account)))
    (compose-capability (CREDIT id seller))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap BUY:bool
    (id:string seller:string buyer:string amount:decimal timeout:integer sale-id:string)
    @doc "Completes sale OFFER to BUYER."
    @managed
    (enforce (sale-active timeout) "BUY: expired")
    (compose-capability (DEBIT id (sale-account)))
    (compose-capability (CREDIT id buyer))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap SALE_PRIVATE:bool (sale-id:string) true)

  (defpact sale:bool
    ( id:string
      seller:string
      amount:decimal
      timeout:integer
    )
    (step-with-rollback
      (with-capability (SALE id seller amount timeout (pact-id))
        (offer id seller amount))
      (with-capability (WITHDRAW id seller amount timeout (pact-id))
        (withdraw id seller amount))
    )
    (step
      (let ( (buyer:string (read-msg "buyer"))
             (buyer-guard:guard (read-msg "buyer-guard")) )
        (with-capability (BUY id seller buyer amount timeout (pact-id))
          (buy id seller buyer buyer-guard amount (pact-id)))))
  )

  (defun offer:bool
    ( id:string
      seller:string
      amount:decimal
    )
    @doc "Initiate sale with by SELLER by escrowing AMOUNT of TOKEN until TIMEOUT."
    (require-capability (SALE_PRIVATE (pact-id)))
    (bind (get-policy-info id)
      { 'policy := policy:module{kip.token-policy-v1}
      , 'token := token }
      (policy::enforce-offer token seller amount (pact-id)))
    (let
      (
        (sender (debit id seller amount))
        (receiver (credit id (sale-account) (create-pact-guard "SALE") amount))
      )
      (emit-event (TRANSFER id seller (sale-account) amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )

  (defun withdraw:bool
    ( id:string
      seller:string
      amount:decimal
    )
    @doc "Withdraw offer by SELLER of AMOUNT of TOKEN before TIMEOUT"
    (require-capability (SALE_PRIVATE (pact-id)))
    (let
      (
        (sender (debit id (sale-account) amount))
        (receiver (credit-account id seller amount))
      )
      (emit-event (TRANSFER id (sale-account) seller amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )


  (defun buy:bool
    ( id:string
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string
    )
    @doc "Complete sale with transfer."
    (require-capability (SALE_PRIVATE (pact-id)))
    (bind (get-policy-info id)
      { 'policy := policy:module{kip.token-policy-v1}
      , 'token := token }
      (policy::enforce-buy token seller buyer buyer-guard amount sale-id))
    (let
      (
        (sender (debit id (sale-account) amount))
        (receiver (credit id buyer buyer-guard amount))
      )
      (emit-event (TRANSFER id (sale-account) buyer amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )

  (defun sale-active:bool (timeout:integer)
    @doc "Sale is active until TIMEOUT block height."
    (< (at 'block-height (chain-data)) timeout)
  )

  (defun sale-account:string ()
    (format "sale-{}" [(pact-id)])
  )

  (defun get-ledger-entry (key:string)
    (read ledger key))

  (defun format-token-id:string (collection-name:string index:string)
    (format "{}:{}" [collection-name index])
  )

;;
;; ACCESSORS
;;

(defun key:string ( id:string account:string )
  @doc "DB key for ledger account"
  (format "{}:{}" [id account])
)

(defun get-manifest:object{manifest} (id:string)
  (at 'manifest (read tokens id)))

(defun get-token:object{token-schema} (id:string)
  "Read token"
  (read tokens id)
)

(defun get-collection:object{collection-schema} (name:string)
  "Read Collection"
  (read collections name)
)

(defun get-token-uri:object{kip.token-manifest.mf-uri} (id:string)
  (at 'uri (get-manifest id))
)

(defun get-policy-info:object{policy-info} (id:string)
  (with-read tokens id
    { 'policy := policy:module{kip.token-policy-v1}
    , 'supply := supply
    , 'precision := precision
    , 'manifest := manifest
    }
    { 'policy: policy
    , 'token:
      { 'id: id
      , 'supply: supply
      , 'precision: precision
      , 'manifest: manifest
      } } )
)

(defun get-balance:decimal (id:string account:string)
  (at 'balance (read ledger (key id account)))
)

(defun details:object{account-details}
  ( id:string account:string )
  (read ledger (key id account))
)

)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table ledger)
    (create-table tokens)
    (create-table collections)])
