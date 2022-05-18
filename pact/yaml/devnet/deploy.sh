JSON="Content-Type: application/json"

URL="http://localhost:8080/chainweb/0.0/development/chain/0/pact/api/v1/send"

# pact -u ./ns-kip.yaml | pact add-sig ns-keys.yaml | curl -H "$JSON" -d @- "$URL"
# pact -u ./ns-util.yaml | pact add-sig ns-keys.yaml | curl -H "$JSON" -d @- "$URL"
# pact -u ./manifest.yaml | pact add-sig keys.yaml | curl -H "$JSON" -d @- "$URL"
# pact -u ./account-protocols-v1.yaml | pact add-sig keys.yaml | curl -H "$JSON" -d @- "$URL"
# pact -u ./token-policy-v1.yaml | pact add-sig keys.yaml | curl -H "$JSON" -d @- "$URL"
# pact -u ./pf-v2.yaml | pact add-sig keys.yaml | curl -H "$JSON" -d @- "$URL"

# pact -u ./fungible-util.yaml | pact add-sig keys.yaml | curl -H "$JSON" -d @- "$URL"

pact -u ./ledger.yaml | pact add-sig keys.yaml | curl -H "$JSON" -d @- "$URL"
# pact -u ./policy.yaml | pact add-sig keys.yaml | curl -H "$JSON" -d @- "$URL"
