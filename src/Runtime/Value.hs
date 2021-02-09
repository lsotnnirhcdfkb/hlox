module Runtime.Value where

data LoxValue = LoxNil
              | LoxBool Bool
              | LoxNumber Double
              | LoxString String
              deriving (Show, Eq)

stringifyType :: LoxValue -> String
stringifyType LoxNil = "nil"
stringifyType (LoxBool _) = "bool"
stringifyType (LoxNumber _) = "number"
stringifyType (LoxString _) = "string"

stringifyValue :: LoxValue -> String
stringifyValue LoxNil = "nil"
stringifyValue (LoxBool b) = if b then "true" else "false"
stringifyValue (LoxNumber n)
    | n == (fromIntegral $ (floor n :: Integer)) = show $ (floor n :: Integer)
    | otherwise = show n
stringifyValue (LoxString s) = s
