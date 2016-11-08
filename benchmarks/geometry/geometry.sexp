(
    (max_terms 2)
    (variables
        (
            (rect (x y z))
            (int (a b))
            (posint (p q))
            (bool (m n))
            (point (t s))
        )
    )
    (signature
        (
            (Symbol geometry_encloses_point (rect point bool))
            (Symbol geometry_encloses (rect rect bool))
            (Symbol geometry_rotate (rect int rect))
            (Symbol geometry_scale (rect posint rect))
            (Symbol geometry_translate (rect int rect))
            (Symbol geometry_area (rect int))
        )
    )
)
