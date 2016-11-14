(
    (max_terms 2)
    (variables
        (
            (tensor (x y z))
            (int (a b))
            (bool (p q))
        )
    )
    (signature
        (
            (Symbol matrix_mult (tensor tensor tensor))
            (Symbol matrix_inv (tensor tensor))
            (Symbol matrix_is_symmetric (tensor bool))
            (Symbol matrix_det (tensor int))
            (Symbol matrix_transpose (tensor tensor))
            (Symbol matrix_is_lower (tensor bool))
            (Symbol matrix_is_upper (tensor bool))
        )
    )
)
