(define-keyset 'util-ns-user)
(define-keyset 'util-ns-admin)
(ns.write-registry (read-msg 'ns) (keyset-ref-guard 'util-ns-admin) true)
(define-namespace
  (read-msg 'ns)
  (keyset-ref-guard 'util-ns-user)
  (keyset-ref-guard 'util-ns-admin)
)