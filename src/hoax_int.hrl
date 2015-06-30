-type error_class() :: error | exit | throw.
-type action()      :: default | {return, term()} | {error_class(), term()}.
-type m_f_a()       :: { Module::atom(), Function::atom(), Arity::non_neg_integer() }.

-record(expectation, {
        key              :: m_f_a(),
        expected_args    :: [term()],
        action = default :: action(),
        call_count = 0   :: integer(),
        expected_count   :: integer() | undefined
    }).
