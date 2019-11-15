let list =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v11.1.0/Prelude/List/package.dhall

let RunSpec =
      { name : Optional Text
      , working_directory : Optional Text
      , command : Text
      , background : Bool
      , shell : Optional Text
      }

let RestoreCacheSpec = { key : Optional Text, keys : Optional (List Text) }

let SaveCacheSpec = { paths : List Text, key : Text }

let PersistToWorkspaceSpec = { root : Text, paths : List Text }

let Step
    : Type
    = < Checkout : Text
      | Run : { run : RunSpec }
      | RestoreCache : { restore_cache : RestoreCacheSpec }
      | SaveCache : { save_cache : SaveCacheSpec }
      | PersistToWorkspace : { persist_to_workspace : PersistToWorkspaceSpec }
      >

let checkout = Step.Checkout "checkout"

let saveCache
    : SaveCacheSpec → Step
    = λ(spec : SaveCacheSpec) → Step.SaveCache { save_cache = spec }

let run
    : RunSpec → Step
    = λ(spec : RunSpec) → Step.Run { run = spec }

let runCommandSpec
    : Text → RunSpec
    =   λ(command : Text)
      → { command = command
        , name = None Text
        , working_directory = None Text
        , background = False
        , shell = None Text
        }

let runCommand
    : Text → Step
    =   λ(command : Text)
      → run
          { command = command
          , name = None Text
          , working_directory = None Text
          , background = False
          , shell = None Text
          }

let restoreCacheKeys
    : List Text → Step
    =   λ(keys : List Text)
      → Step.RestoreCache
          { restore_cache = { key = None Text, keys = Some keys } }

let restoreCacheKey
    : Text → Step
    =   λ(key : Text)
      → Step.RestoreCache
          { restore_cache = { key = Some key, keys = None (List Text) } }

let persistToWorkspace
    : PersistToWorkspaceSpec → Step
    =   λ(spec : PersistToWorkspaceSpec)
      → Step.PersistToWorkspace { persist_to_workspace = spec }

let saveSpecToRestore
    : SaveCacheSpec → Step
    = λ(spec : SaveCacheSpec) → restoreCacheKey spec.key

let withRestoreAndCache
    : List SaveCacheSpec → List Step → List Step
    =   λ(specs : List SaveCacheSpec)
      → λ(steps : List Step)
      →   list.map SaveCacheSpec Step saveSpecToRestore specs
        # steps
        # list.map SaveCacheSpec Step saveCache specs

in  { run = run
    , runCommand = runCommand
    , runCommandSpec = runCommandSpec
    , checkout = checkout
    , saveCache = saveCache
    , restoreCacheKey = restoreCacheKey
    , restoreCacheKeys = restoreCacheKeys
    , withRestoreAndCache = withRestoreAndCache
    , persistToWorkspace = persistToWorkspace
    , Step = Step
    }
