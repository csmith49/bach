(
    (max_terms 4)
    (variables
        (
            (string (x y z))
            (int (a b))
            (bool (p q))
        )
    )
    (signature
        (
            (Symbol concat (string string string))
			(Symbol reverse (string string))
            (Symbol is_prefix (string string bool))
            (Symbol length (string int))
            (Symbol to_caps (string string))
            (Symbol to_lower (string string))
            (Symbol left_strip (string string))
            (Symbol right_strip (string string))
            (Symbol all_strip (string string))
        )
    )
)
