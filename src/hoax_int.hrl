-record(expectation, {
    key              :: { Module::atom(), Function::atom(), Arity::non_neg_integer() },
    desc             :: string(),
    line_num         :: non_neg_integer(),
    args             :: [term()],
    action = default :: default | fun(() -> any()),
    call_count = 0   :: integer(),
    expected_count   :: integer() | undefined
}).
