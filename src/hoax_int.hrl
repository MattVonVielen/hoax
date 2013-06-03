-type error_class() :: error | exit | throw.
-type action()      :: default | {return, term()} | {error_class(), term()}.
-type m_f_args()    :: { Module::atom(), Function::atom(), Args::[term()] }.

-record(expectation, {
        key            :: m_f_args(),
        action         :: action(),
        call_count = 0 :: integer()
    }).
