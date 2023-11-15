module Poseidon.CLI.Jannocoalesce where

data JannoCoalesceOptions = JannoCoalesceOptions
    { _jannocoalesceSource           :: FilePath
    , _jannocoalesceTarget           :: FilePath
    , _jannocoalesceOutSpec          :: Maybe FilePath -- Nothing means "in place"
    , _jannocoalesceFillColumns      :: [String] -- empty list means All
    , _jannocoalesceOverwriteColumns :: Bool
    }

runJannocoalesce :: JannoCoalesceOptions -> PoseidonIO ()
runJannocoalesce (JannoCoalesceOptions source target outSpec fields overwrite) = do
    JannoRows sourceRows <- readJannoFile source
    JannoRows targetRows <- readJannoFile target
    newJanno <- forM targetRows $ \targetRow -> do
        let posId = jPoseidonID targetRow
            sourceRowCandidates = filter (\r -> jPoseidonId r == posId) sourceRows
        case sourceRowCandidates of
            [] -> return targetRow
            [keyRow] -> return $ mergeRows targetRow keyRow fields overwrite
            _ -> throwM $ PoseidonGenericException $ "source file contains multiple rows with key " ++ posId
    let outPath = maybe target id outSpec 
    liftIO $ writeJannoFile outPath newJanno


mergeRow :: JannoRow -> JannoRow -> [String] -> Bool -> JannoRow
mergeRow targetRow sourceRow fields overwrite = do
    let targetRowRecord = toNamedRecord targetRow
        sourceRowRecord = toNamedRecord sourceRow
    fromNamedRecord $ HM.unionWith mergeIfMissing targetRowRecord sourceRowRecord
  where
    mergeIfMissing targetVal sourceVal = if targetVal `elem` ["n/a", ""] || overwrite then sourceVal else targetVal
