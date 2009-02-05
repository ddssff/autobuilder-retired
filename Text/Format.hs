module Text.Format where

class Show a => Format a where
    format :: a -> String
    format = show
