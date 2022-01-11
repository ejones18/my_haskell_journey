{-
Simple web scraper using Scalpel.

Author: Ethan Jones
-}

{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Applicative

print_azure_updates :: IO (Maybe [String])
print_azure_updates = scrapeURL "https://azure.microsoft.com/en-gb/updates/" fetch_updates
    where
        fetch_updates :: Scraper String [String]
        fetch_updates = chroots ("h3" @: [hasClass "text-body2"]) isolate_update
        
        isolate_update :: Scraper String String
        isolate_update = update
        
        update :: Scraper String String
        update = do 
            header <- text $ "a"
            return $ header
