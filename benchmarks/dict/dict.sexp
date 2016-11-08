(
    (max_terms 2)
    (variables
        (
            (dict (x y z))
            (posint (a b))
            (bool (p q))
        )
    )
    (signature
        (
            (Symbol dict_haskey (dict posint bool))
            (Symbol dict_update (dict posint posint dict))
            (Symbol dict_get (dict posint posint))
            (Symbol dict_clear (dict dict))
            (Symbol dict_len (dict posint))
        )
    )
)
