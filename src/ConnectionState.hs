module ConnectionState where

-- Datatype representing the connection state

data ConnectionState = NotAuthenticated
                     | Authenticated
                     | Selected
                     | Logout

