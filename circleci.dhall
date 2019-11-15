let Map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v11.1.0/Prelude/Map/package.dhall

let list =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v11.1.0/Prelude/List/package.dhall

let steps = ./steps.dhall

let Step = steps.Step

let Env = Map.Type Text Text

let DockerImage =
      { image : Text, name : Optional Text, environment : Optional Env }

let MacOs = { xcode : Text }

let Executor
    : Type
    = < Docker : List DockerImage | MacOs : Text >

let Job
    : Type
    = { environment : Env
      , executor : Executor
      , working_directory : Optional Text
      , steps : List Step
      }

let JobJSON
    : Type
    = { environment : Env
      , docker : Optional (List DockerImage)
      , macos : Optional MacOs
      , working_directory : Optional Text
      , steps : List Step
      }

let jobToJson
    : Job → JobJSON
    =   λ(job : Job)
      → { environment = job.environment
        , working_directory = job.working_directory
        , steps = job.steps
        , docker =
            merge
              { Docker = λ(images : List DockerImage) → Some images
              , MacOs = λ(version : Text) → None (List DockerImage)
              }
              job.executor
        , macos =
            merge
              { Docker = λ(images : List DockerImage) → None MacOs
              , MacOs = λ(version : Text) → Some { xcode = version }
              }
              job.executor
        }

let Orb
    : Type
    = Map.Type Text Text

let CircleCi
    : Type
    = { version : Double, orbs : Optional Orb, jobs : Map.Type Text Job }

let UnsafeCircleCi
    : Type
    = { version : Double, orbs : Optional Orb, jobs : Map.Type Text JobJSON }

let toCircleCi
    : CircleCi → UnsafeCircleCi
    =   λ(spec : CircleCi)
      → { version = spec.version
        , orbs = spec.orbs
        , jobs = Map.map Text Job JobJSON jobToJson spec.jobs
        }

in  { run = steps.run
    , runCommand = steps.runCommand
    , runCommandSpec = steps.runCommandSpec
    , checkout = steps.checkout
    , saveCache = steps.saveCache
    , restoreCacheKey = steps.restoreCacheKey
    , restoreCacheKeys = steps.restoreCacheKeys
    , withRestoreAndCache = steps.withRestoreAndCache
    , persistToWorkspace = steps.persistToWorkspace
    , Step = steps.Step
    , Env = Env
    , Job = Job
    , Orb = Orb
    , Executor = Executor
    , toCircleCi = toCircleCi
    }
