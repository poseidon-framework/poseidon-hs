module Poseidon.CLI.Jannocoalesce where

data JannoCoalesceOptions = JannoCoalesceOptions
    { _jannocoalesceSource           :: FilePath
    , _jannocoalesceTarget           :: FilePath
    , _jannocoalesceOutSpec          :: Maybe FilePath -- Nothing means "in place"
    , _jannocoalesceFillColumns      :: [String] -- empty list means All
    , _jannocoalesceOverwriteColumns :: Bool
    }

runJannocoalesce :: JannoCoalesceOptions -> PoseidonIO ()
runJannocoalesce (JannoCoalesceOptions source target outSpec fillColumns overwrite) = do
    sourceJanno <- readJannoFile source
    targetJanno <- readJannoFile target
    let newJanno = do -- list monad
            sourceRow <- sourceJanno -- loop over source rows
            targetRow <- lookupRowById targetJanno sourceRow
            

lookupRowById :: (MonadThrow m) => JannoRows -> JannoRow -> m JannoRow
lookupRowById = do
    undefined
