(
    (max_terms 2)
    (variables
        (
            (set (x y z))
            (int (a b))
            (bool (p q))
        )
    )
    (signature
        (
            (Symbol set_contains (set int bool))
            (Symbol set_add (set int set))
            (Symbol set_discard (set int set))
            (Symbol set_clear (set set))
            (Symbol set_difference (set set set))
            (Symbol set_union (set set set))
            (Symbol set_issubset (set set bool))
            (Symbol set_issuperset (set set bool))
            (Symbol set_length (set int))
        )
    )
)
