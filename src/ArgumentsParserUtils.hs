module ArgumentsParserUtils where

import qualified Text.Megaparsec as M

noArguments :: M.MonadParsec e s m => m ()
noArguments = M.eof
