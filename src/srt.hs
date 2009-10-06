module SRT where

import Text.Regex.Posix
import Text.ParserCombinators.Parsec
import Utils
import Control.Monad

import Timing
import Subtitle

		
readSubtitlesOrError :: String -> Either String [Subtitle]
readSubtitlesOrError input = case parse p_srt_file "" input of
    Left err -> Left $ show err
    Right subs -> Right subs

readSubtitles :: String -> Maybe [Subtitle]
readSubtitles input = case parse p_srt_file "" input of
            Left err -> Nothing
            Right subs -> Just subs


renderSrtFile :: [Subtitle] -> String
renderSrtFile subs = join (map show subs) 	

p_srt_file :: CharParser () [Subtitle]
p_srt_file = p_subtitle `manyTill` eof

p_subtitle :: CharParser () Subtitle
p_subtitle = do
    index <- p_int <?> "subtitle index"
    p_eol <?> "new line after subtitle index"
    beginning <- p_timing <?> "beginning timing"
    p_time_separator <?> "-->"
    end <- p_timing <?> "end timing"
    text <- p_subtitle_text <?> "subtitle text"
    return $ Subtitle index beginning end text
    

p_timing :: CharParser () Timing
p_timing = do
    hours <- p_int <?> "hours"
    char ':' <?> "hours"
    minutes <- p_int <?> "minutes"
    char ':' <?> "minutes"
    seconds <- p_int <?> "seconds"
    char ',' <?> "seconds"
    milliseconds <- p_int <?> "milliseconds"
    return $ Timing hours minutes seconds milliseconds

p_subtitle_text :: CharParser () String
p_subtitle_text = manyTill anyChar ((try $ p_eol >> skipMany1 p_eol) <|> eof)

p_eol :: CharParser () ()
p_eol = do
    char '\n' <?> "end of line"
    (char '\r' >> return ()) <|> return ()

p_time_separator :: CharParser () String
p_time_separator = do
    many space
    string "-->"
    many space

p_int :: CharParser () Int
p_int = (liftM read) $ many1 digit






		

	

