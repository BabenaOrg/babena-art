(namespace (read-msg 'ns))

(module babena-royalty-policy GOVERNANCE

  @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'babena-admin )))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defcap MINT (account:string)
     (compose-capability (RANDOM))
   )

   (defcap RANDOM ()
     true
   )

   (defcap UPDATE-OWNER (token-id:string new-owner:string)
    true
   )

   (defcap BUY (id:string receiver:string)
    (compose-capability (UPDATE-OWNER id receiver))
   )



  (defschema babena-schema
    provenance-hash:string ;sha256 of combined string
    total-supply:integer ;total supply of tokens that will ever exist
    max-per-user:integer ;maximum NFT a user can mint
    max-per-wh:integer ;maximun NFT a whitelisted user can mint
    royalty-receiver:string ;account which receives the royalty
    tokens-list:[string] ;list of sha256 of the images that will ever exist
    creator:string
    creator-guard:guard
    public-mint-time:time
    royalty-rate:decimal
    price-per-nft:decimal
    whitelist-price:decimal
  )

   (defschema mint-schema
     tokens-list:[string] ;temporary list of sha256 of the images
     current-length:integer ;count of tokens that are not yet minted
     status:string
   )

   (defschema account-schema
     account:string
     minted:integer
   )

  (defschema whitelist-schema
    account:string
    guard:guard
    claimed:integer
  )

  (defschema policy-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    royalty-rate:decimal
    owner:string
  )

  (defschema traits-schema
    job:string
    accessory:string
    haircut:string
    eyes:string
    mouth:string
    shirt:string
    base:string
    background:string
  )

  (defschema babena-metadata
    name:string
    description:string
    image:string
    image-hash:string
    traits:object{traits-schema}
  )

  (deftable policies:{policy-schema})
  (deftable babena-details:{babena-schema})
  (deftable mint-status:{mint-schema})
  (deftable account-details:{account-schema})
  (deftable whitelists:{whitelist-schema})

  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
    )

  (defconst MINT_STATUS "mint-status")
  (defconst BABENA_DETAILS "babena-details")
  (defconst MINT_PAUSED "mint-paused")
  (defconst MINT_STARTED "mint-started")
  (defconst MINT_COMPLETED "mint-completed")

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (deftable quotes:{quote-schema})

  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
  )

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      royalty-payout:decimal
      creator:string
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defun enforce-ledger:bool ()
    (enforce-guard (babena-ledger.ledger-guard))
  )

   (defun enforce-whitelist:bool (account:string guard:guard)

    (let ( (accounts:[string] (keys whitelists))
           (max-per-wh:integer (get-max-per-wh)))
      (enforce (contains account accounts) "You are not whitelisted")
      (with-read whitelists account{
       'guard:= g,
       'claimed:= claimed
      }
       (enforce (= g guard) "Guards doesn't match.")
       (enforce (< claimed  max-per-wh) "You have reached the whitelist claim limit")
     )
    )
   )

  (defun enforce-mint-per-user (account:string)
    (let ( (minted:integer (get-account-minted account))
           (max-per-user:integer (get-max-per-user)))
      (enforce (< minted max-per-user)
       (format "You can Mint only {} tokens" [max-per-user]))
    )
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
     (enforce-ledger)
     (let ( (total-minted:integer (get-total-minted))
           (total-supply:integer (get-total-supply))
          )
       (enforce (< total-minted total-supply) (format "Total supply of {} reached" [total-supply]))
     )
     (with-read mint-status MINT_STATUS {
       'status:= status
       }
       (enforce (= status MINT_STARTED) "PRE-MINT is paused or completed, can't mint now")
     )
     (enforce-mint-per-user account)
     (enforce (= 1.0 amount) "Amount must always be 1.0 for 1 for 1 NFTs")

     (with-capability (MINT account)
       (let* ( (whitelist-enabled:bool (check-whitelist))
               (random:integer (get-random account))
               (current-length:integer (get-current-length))
               (index:integer (mod random current-length))
               (available-tokens:[string] (at 'tokens-list (read mint-status MINT_STATUS)))
               (token-id:string (at index available-tokens))
               (minted:integer (get-account-minted account))
               (babena-details:object{babena-schema} (get-details))
               (creator:string (at 'creator babena-details))
       )
       (if whitelist-enabled (enforce-whitelist account guard) "Pre-mint phase for whitelisted users ended")
        (let* ( (price:decimal (if whitelist-enabled (at 'whitelist-price babena-details) (at 'price-per-nft babena-details))))

              (update mint-status MINT_STATUS {
                'tokens-list: (filter (!= token-id) available-tokens),
                'current-length: (- current-length 1)
              })
              (write account-details account {
                'account: account,
                'minted: (+ minted 1)
              })
              (coin.transfer account creator price)
        )
       token-id
       )
     )
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce false "Burn prohibited")
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (let* ( (spec:object{policy-schema} (read-msg TOKEN_SPEC))
            (fungible:module{fungible-v2} (at 'fungible spec))
            (creator:string (at 'creator spec))
            (creator-guard:guard (at 'creator-guard spec))
            (royalty-rate:decimal (at 'royalty-rate (get-details)))
            (owner:string (at 'owner spec))
            (creator-details:object (fungible::details creator ))
            )
      (fungible::enforce-unit royalty-rate)
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "Invalid royalty rate")
      (insert policies (at 'id token)
        { 'fungible: fungible
        , 'creator: creator
        , 'creator-guard: creator-guard
        , 'owner: owner
        , 'royalty-rate: royalty-rate }))
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
    (with-read mint-status MINT_STATUS {
      'status:= status
      }
      (enforce (= status MINT_COMPLETED) "Pre-mint is not yet completed")
    )
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
       ,'royalty-rate:= royalty-rate:decimal
       ,'creator:= creator:string
      }
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient))
            (sale-price:decimal (* amount price))
            (royalty-payout:decimal
              (floor (* sale-price royalty-rate) (fungible::precision))) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price royalty-payout creator spec)))
      true
  )
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (with-capability (BUY (at 'id token) buyer)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
      , 'creator:= creator:string
      , 'royalty-rate:= royalty-rate:decimal
      }
      (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          { 'price := price:decimal
          , 'recipient := recipient:string
          }
          (let* ((sale-price:decimal (* amount price))
                 (royalty-payout:decimal
                  (floor (* sale-price royalty-rate) (fungible::precision)))
               (payout:decimal (- sale-price royalty-payout)) )
          (if
            (> royalty-payout 0.0)
            (fungible::transfer buyer creator royalty-payout)
            "No royalty")
            (fungible::transfer buyer recipient payout)))
            true
        (update-owner qtoken buyer)
      ))
    )
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun add-whitelist (account:string)
  (if (check-whitelist)
  (with-capability (GOVERNANCE)
    (insert whitelists account{
      "account": account,
      "guard": (at 'guard (coin.details account)),
      "claimed": 0
    })
  )
  "Pre-mint for whitelist users ended, can't add users now")
  )

  (defun update-owner (token-id:string new-owner:string)
    (require-capability (UPDATE-OWNER token-id new-owner))
    (update policies token-id
      {'owner: new-owner}
    )
  )

  (defun check-whitelist:bool ()
    (< (diff-time (at 'block-time (chain-data)) (get-public-mint-time)) 0.0)
  )

  (defun get-current-length:integer ()
    (with-read mint-status MINT_STATUS {
      'current-length:= current-length
    }
    current-length
    )
  )

  (defun get-account-minted:integer (account:string)
    (with-default-read account-details account
      {"minted": 0}
      {"minted":= minted}
    minted
    )
  )

  (defun get-total-minted:integer ()
    (with-read mint-status MINT_STATUS {
      'current-length:= current-length
    }
    (- (at 'total-supply (get-details)) current-length)
    )
  )

  (defun get-mint-status ()
    (read mint-status MINT_STATUS)
  )

  (defun get-account (account:string)
    (read account-details account)
  )

  (defun get-random:integer (account:string)
    (require-capability (RANDOM))
    (let* (
      (prev-block-hash (at "prev-block-hash" (chain-data)))
      (random (str-to-int 64 (hash (+ prev-block-hash (take 20 account)))))
    )
    random
    )
  )

  (defun get-whitelist-info:object{whitelist-schema} (account:string)
    (read whitelists account)
  )

  (defun get-details:object{babena-schema} ()
   (read babena-details BABENA_DETAILS)
  )

  (defun get-total-supply:string ()
   (at 'total-supply (read babena-details BABENA_DETAILS))
  )

  (defun get-max-per-user ()
    (at 'max-per-user (get-details))
  )

  (defun get-max-per-wh ()
    (at 'max-per-wh (get-details))
  )

  (defun get-public-mint-time:time ()
    (with-read babena-details BABENA_DETAILS {
      'public-mint-time:= public-mint-time
      }
      public-mint-time
    )
  )

  (defun pause-mint ()
  (with-capability (GOVERNANCE)
    (update mint-status MINT_STATUS {
      'status: MINT_PAUSED
    })
  )
  )

  (defun resume-mint ()
    (with-capability (GOVERNANCE)
      (update mint-status MINT_STATUS {
        'status: MINT_STARTED
      })
    )
  )

  (defun end-mint ()
    (with-capability (GOVERNANCE)
      (let ( (total-minted:integer (get-total-minted))
            (total-supply:integer (get-total-supply))
           )
        (enforce (= total-minted total-supply) "Tokens not sold completely, can't end mint now")
      )
      (update mint-status MINT_STATUS {
        'status: MINT_COMPLETED
      })
    )
  )

  (defun initialize (
    provenance:string
    tokens-list:[string]
    creator:string
    total-supply:integer
    max-per-user:integer
    max-per-wh:integer
    public-mint-time:time
    royalty-receiver:string
    royalty-rate:decimal
    price-per-nft:decimal
    whitelist-price:decimal)
    (enforce (= (length tokens-list) total-supply) "Total-supply and tokens-list length does not match")
    (write babena-details BABENA_DETAILS {
      "provenance-hash": provenance,
      "max-per-user": max-per-user,
      "max-per-wh": max-per-wh,
      "total-supply": total-supply,
      "tokens-list": tokens-list,
      "creator": creator,
      "creator-guard": (at 'guard (coin.details creator)),
      "public-mint-time": public-mint-time,
      "royalty-receiver": royalty-receiver,
      "royalty-rate": royalty-rate,
      "whitelist-price": whitelist-price,
      "price-per-nft": price-per-nft
    })
    (write mint-status MINT_STATUS {
      "current-length": (length tokens-list),
      "tokens-list": tokens-list,
      "status": MINT_STARTED
    })
  )

)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table quotes)
    (create-table policies)
    (create-table whitelists)
    (create-table babena-details)
    (create-table account-details)
    (create-table mint-status)])
