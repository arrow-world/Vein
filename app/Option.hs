module Option where

import Options.Applicative

data Options = Options
    { verbose        :: Bool
    , outputFile     :: FilePath
    , inputFiles     :: [FilePath]
    } deriving Show

verboseP :: Parser Bool
verboseP = switch $ short 'v' <> long "verbose" <> help "verbose mode"

outputFileP :: Parser String
outputFileP = strOption $ mconcat
    [ short 'o', long "output"
    , help "output file"
    , metavar "FILE"
    , value "a.out"
    , showDefaultWith id
    ]

inputFileP :: Parser FilePath
inputFileP = strArgument $ mconcat
    [ help "input files"
    , metavar "FILES"
    , action "file"
    ]

inputFilesP :: Parser [FilePath]
inputFilesP = some inputFileP

optionsP :: Parser Options
optionsP = (<*>) helper $ Options <$> verboseP <*> outputFileP <*> inputFilesP

myParserInfo :: ParserInfo Options
myParserInfo = info optionsP $ mconcat 
    [ fullDesc
    , progDesc "Vein Compiler"
    , header ""
    , footer ""
    , progDesc ""
    ]

parse :: IO Options
parse = customExecParser (prefs showHelpOnError) myParserInfo