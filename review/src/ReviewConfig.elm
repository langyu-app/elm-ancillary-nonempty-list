module ReviewConfig exposing (config)

import Documentation.ReadmeLinksPointToCurrentVersion
import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDuplicatePorts
import NoEmptyText
import NoExposingEverything
import NoFloatIds
import NoImportingEverything
import NoInconsistentAliases
import NoInvalidRGBValues
import NoLeftPizza
import NoLongImportLines
import NoMissingDocumentation
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoRecursiveUpdate
import NoRedundantConcat
import NoRedundantCons
import NoRegex
import NoTypeAliasConstructorCall
import NoUnsafePorts
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnusedPorts
import NoUselessSubscriptions
import Review.Rule exposing (Rule)
import UseCamelCase


config : List Rule
config =
    [ Documentation.ReadmeLinksPointToCurrentVersion.rule
    , NoBooleanCase.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoDuplicatePorts.rule
    , NoEmptyText.rule
    , NoExposingEverything.rule
    , NoFloatIds.rule
    , NoImportingEverything.rule []
    , NoInconsistentAliases.config
        [ ( "Array.Extra", "ArrayX" )
        , ( "Html.Attributes", "Attr" )
        , ( "Html.Extra", "HtmlX" )
        , ( "Json.Decode", "Decode" )
        , ( "Json.Decode.Ancillary", "DecodeA" )
        , ( "Json.Decode.Extra", "DecodeX" )
        , ( "Json.Encode", "Encode" )
        , ( "Json.Encode.Extra", "EncodeX" )
        , ( "List.Extra", "ListX" )
        , ( "List.Nonempty", "NE" )
        , ( "List.Nonempty.Ancillary", "NEA" )
        , ( "Maybe.Extra", "MaybeX" )
        , ( "Random.Extra", "RandomX" )
        , ( "Result.Extra", "ResultX" )
        , ( "Svg.Attributes", "SvgAttr" )
        ]
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule
    , NoInvalidRGBValues.rule
    , NoLeftPizza.rule NoLeftPizza.Redundant
    , NoLongImportLines.rule
    , NoMissingDocumentation.rule
    , NoMissingSubscriptionsCall.rule
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoModuleOnExposedNames.rule
    , NoRecursiveUpdate.rule
    , NoRedundantConcat.rule
    , NoRedundantCons.rule
    , NoRegex.rule
    , NoTypeAliasConstructorCall.rule
    , NoUnsafePorts.rule NoUnsafePorts.any
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoUnusedPorts.rule
    , NoUselessSubscriptions.rule
    , UseCamelCase.rule UseCamelCase.default
    ]
