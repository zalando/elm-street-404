module Config (imageUrl) where

port imagesUrl : String

imageUrl : String -> String
imageUrl img =
    imagesUrl ++ img
