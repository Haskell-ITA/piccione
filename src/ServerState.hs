module ServerState where

-- Datatype representing the connection state

data ServerState = NotAuthenticated
                 | Authenticated
                 | Selected

