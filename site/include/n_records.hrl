
-record(nnote, { 
    id       =  n_utils:create_id(),
    user_id  :: list(),
    date     =  calendar:universal_time(),   
    type     :: list() | undefined,
    event    :: list() | undefined,
    source   :: list() | undefined,
    topic    :: list() | undefined,
    question :: list() | undefined,
    tags     :: list() | undefined,
    note     :: list() | undefined
   }).

-record(naccounts, {
    email,
    date = calendar:universal_time(),
    pwhash
   }).



