module Config (imageUrl) where

imageUrl : String -> String
imageUrl img =
    "img/" ++ img
