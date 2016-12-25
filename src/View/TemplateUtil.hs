module View.TemplateUtil where

-- |Given x, return the string "template/hamlet/x.hamlet
hamFile name = "template/hamlet/" ++ name ++ ".hamlet"
-- |Given x, return the string "template/lucius/x.lucius
cssFile name = "template/lucius/" ++ name ++ ".lucius"

-- |Given x, return the string "template/lucius/x.lucius
jsFile name = "template/julius/" ++ name ++ ".julius"
