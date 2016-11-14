(
    (max_terms 3)
    (variables
        (
            (rect (x y z))
            (int (a b))
            (rad (r e))
            (bool (n))
            (point (t s))
        )
    )
    (signature
        (
            (Symbol geometry_encloses (rect rect bool))
            (Symbol geometry_area (rect int))
            (Symbol geometry_encloses_point (rect point bool))
            (Symbol geometry_rotate (rect rad rect))
            (Symbol geometry_scale (rect int rect))
            (Symbol geometry_translate (rect int rect))
        )
    )
)
