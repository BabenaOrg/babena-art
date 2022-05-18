(enforce-pact-version "3.7")
(namespace "free")
(interface metadata

  (defschema traits-schema
    trait-type:string
    trait-value:string
  )

  (defschema token-metadata
    name:string
    description:string
    image:string
    image-hash:string
    traits:object{traits-schema}
  )
)
