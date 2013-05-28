-type error_class() :: error | exit | throw.
-type action()      :: default | {return, term()} | {error_class(), term()}.

-record(expectation, {
        module   :: atom(),
        function :: atom(),
        arity    :: non_neg_integer(),
        args     :: [term()],
        action   :: action()
    }).
