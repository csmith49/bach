(
    (max_terms 3)
    (variables
        (
            (rect (x y z))
            (int (a b))
            (posint (a b))
            (rad (r e))
            (bool (n))
            (point (t s))
        )
    )
    (signature
        (
            (Symbol geometry_encloses (rect rect bool))
            (Symbol geometry_encloses_point (rect point bool))
            (Symbol geometry_area (rect int))
            (Symbol geometry_translate (rect int rect))
            (Symbol geometry_scale (rect posint rect))
            (Symbol geometry_rotate (rect rad rect))
        )
    )
)
