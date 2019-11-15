let JSON =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v11.1.0/Prelude/JSON/package.dhall

let list =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v11.1.0/Prelude/List/package.dhall

let Map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v11.1.0/Prelude/Map/package.dhall

let optional =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v11.1.0/Prelude/Optional/package.dhall

let natural =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v11.1.0/Prelude/Natural/package.dhall

let Refs = < Only : List Text | Ignore : List Text >

let Filters = { branches : Optional Refs, tags : Optional Refs }

let JobSpec =
      { name : Text
      , type : Optional Text
      , context : Optional Text
      , requires : List Text
      , filters : Filters
      }

let Job = < Name : Text | WithSpec : JobSpec >

let Workflow = { jobs : List Job }

let Workflows = { version : Natural, workflows : Map.Type Text Workflow }

let refsToJson
    : Refs → JSON.Type
    =   λ(refs : Refs)
      → JSON.object
          [ merge
              { Only =
                    λ(names : List Text)
                  → Map.keyValue
                      JSON.Type
                      "only"
                      (JSON.array (list.map Text JSON.Type JSON.string names))
              , Ignore =
                    λ(names : List Text)
                  → Map.keyValue
                      JSON.Type
                      "ignore"
                      (JSON.array (list.map Text JSON.Type JSON.string names))
              }
              refs
          ]

let filtersToJson
    : Filters → JSON.Type
    =   λ(filters : Filters)
      → JSON.object
          ( toMap
              { branches =
                  optional.fold
                    Refs
                    filters.branches
                    JSON.Type
                    refsToJson
                    JSON.null
              , tags =
                  optional.fold Refs filters.tags JSON.Type refsToJson JSON.null
              }
          )

let toNullableText
    : Optional Text → JSON.Type
    =   λ(o : Optional Text)
      → optional.fold Text o JSON.Type JSON.string JSON.null

let jobSpecToJson
    : JobSpec → JSON.Type
    =   λ(spec : JobSpec)
      → JSON.object
          [ Map.keyValue JSON.Type "type" (toNullableText spec.type)
          , Map.keyValue JSON.Type "context" (toNullableText spec.context)
          , Map.keyValue
              JSON.Type
              "requires"
              (JSON.array (list.map Text JSON.Type JSON.string spec.requires))
          , Map.keyValue JSON.Type "filters" (filtersToJson spec.filters)
          ]

let jobToJson
    : Job → JSON.Type
    =   λ(job : Job)
      → merge
          { Name = λ(name : Text) → JSON.string name
          , WithSpec =
                λ(spec : JobSpec)
              → JSON.object
                  [ JSON.keyValue JSON.Type spec.name (jobSpecToJson spec) ]
          }
          job

let workflowToJson
    : Workflow → JSON.Type
    =   λ(workflow : Workflow)
      → JSON.object
          ( toMap
              { jobs =
                  JSON.array (list.map Job JSON.Type jobToJson workflow.jobs)
              }
          )

let toWorkflows
    : Workflows → JSON.Type
    =   λ(workflows : Workflows)
      → let map =
                toMap
                  { version = JSON.number (natural.toDouble workflows.version) }
              # Map.map
                  Text
                  Workflow
                  JSON.Type
                  workflowToJson
                  workflows.workflows
        
        in  JSON.object map

in  { toWorkflows = toWorkflows, Job = Job, Workflows = Workflows, Refs = Refs }
