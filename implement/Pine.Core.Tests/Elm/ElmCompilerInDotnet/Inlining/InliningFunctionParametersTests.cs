using AwesomeAssertions;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Inlining = Core.Elm.ElmCompilerInDotnet.Inlining;

public class InliningFunctionParametersTests
{
    private static string InlineAndRenderSingleModule(
        string elmModuleText,
        IReadOnlyList<string> moduleName,
        Inlining.Config config)
    {
        var appModule =
            InliningTestHelper.CanonicalizeAndInlineAndGetSingleModule(
                [
                elmModuleText,
                ],
                moduleName,
                config);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(appModule);

        return rendered;
    }

    [Fact]
    public void Single_choice_tag_single_primitive_argument_directly_annotated_parameter_pattern()
    {
        // apply f x = f x
        // result = apply (\y -> y * 2) 5
        // After inlining apply and beta-reducing: result = y * 2 [y := 5] = int_multiply [5, 2]

        var elmModuleText =
            """"
            module App exposing (..)


            type MyType
                = MyConstructor Int


            alfa : Int -> MyType -> Int
            alfa factor (MyConstructor x) =
                Pine_builtin.int_multiply [ factor, x ]


            root : Int -> Int
            root factor =
                alfa factor (MyConstructor 5)

            """";

        var expectedElmModuleText =
            """"
            module App exposing (..)
            
            
            type MyType
                = MyConstructor Int
            

            alfa : Int -> App.MyType -> Int
            alfa factor (App.MyConstructor x) =
                Pine_builtin.int_multiply
                    [ factor, x ]


            root : Int -> Int
            root factor =
                alfa__specialized__1
                    factor
                    5


            alfa__specialized__1 factor x =
                Pine_builtin.int_multiply
                    [ factor, x ]

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }

    [Fact]
    public void Single_choice_tag_single_primitive_argument_directly_annotated_case_of_pattern()
    {
        // apply f x = f x
        // result = apply (\y -> y * 2) 5
        // After inlining apply and beta-reducing: result = y * 2 [y := 5] = int_multiply [5, 2]

        var elmModuleText =
            """"
            module App exposing (..)


            type MyType
                = MyConstructor Int


            alfa : Int -> MyType -> Int
            alfa factor myValue =
                case myValue of
                    MyConstructor x ->
                        Pine_builtin.int_multiply [ factor, x ]


            root : Int -> Int
            root factor =
                alfa factor (MyConstructor 5)

            """";

        var expectedElmModuleText =
            """"
            module App exposing (..)
            
            
            type MyType
                = MyConstructor Int
            
            
            alfa : Int -> App.MyType -> Int
            alfa factor myValue =
                case myValue of
                    App.MyConstructor x ->
                        Pine_builtin.int_multiply
                            [ factor, x ]


            root : Int -> Int
            root factor =
                alfa__specialized__1
                    factor
                    5


            alfa__specialized__1 factor myValue__field__1 =
                Pine_builtin.int_multiply
                    [ factor, myValue__field__1 ]

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }

    [Fact]
    public void Single_choice_tag_nested_single_primitive_argument_directly_annotated_case_of_pattern()
    {
        var elmModuleText =
            """"
            module App exposing (..)


            type Inner
                = Inner Int


            type Outer
                = Outer Inner


            alfa : Int -> Outer -> Int
            alfa factor myValue =
                case myValue of
                    Outer innerValue ->
                        case innerValue of
                            Inner x ->
                                Pine_builtin.int_multiply [ factor, x ]


            root : Int -> Int
            root factor =
                alfa factor (Outer (Inner 5))

            """";

        var expectedElmModuleText =
            """"
            module App exposing (..)
            
            
            type Inner
                = Inner Int
            
            
            type Outer
                = Outer App.Inner
            
            
            alfa : Int -> App.Outer -> Int
            alfa factor myValue =
                case myValue of
                    App.Outer innerValue ->
                        case innerValue of
                            App.Inner x ->
                                Pine_builtin.int_multiply
                                    [ factor, x ]


            root : Int -> Int
            root factor =
                alfa__specialized__1
                    factor
                    5


            alfa__specialized__1 factor myValue__field__1__field__1 =
                Pine_builtin.int_multiply
                    [ factor, myValue__field__1__field__1 ]

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }

    [Fact]
    public void Single_choice_tag_two_arguments_one_generic_case_of_pattern()
    {
        // Modeled after ParserWithComments.WithComments from elm-syntax:
        // type WithComments res = WithComments Comments res
        // The constructor has two arguments; the second is a generic type parameter.
        // When a call site passes a known constructor application, both fields should be
        // extracted and the wrapping/unwrapping should be eliminated.

        var elmModuleText =
            """"
            module App exposing (..)


            type alias Comments =
                List String


            type WithComments res
                = WithComments Comments res


            merge : Comments -> WithComments a -> WithComments a
            merge earlier withComments =
                case withComments of
                    WithComments laterComments res ->
                        WithComments (Pine_builtin.concat [ earlier, laterComments ]) res


            root : List String -> List String -> Int -> WithComments Int
            root earlier later value =
                merge earlier (WithComments later value)

            """";

        var expectedElmModuleText =
            """"
            module App exposing (..)


            type alias Comments =
                List.List String


            type WithComments res
                = WithComments App.Comments res


            merge : App.Comments -> App.WithComments a -> App.WithComments a
            merge earlier withComments =
                case withComments of
                    App.WithComments laterComments res ->
                        App.WithComments
                            (Pine_builtin.concat
                                [ earlier, laterComments ]
                            )
                            res


            root : List.List String -> List.List String -> Int -> App.WithComments Int
            root earlier later value =
                merge__specialized__1
                    earlier
                    ( later, value )


            merge__specialized__1 earlier ( withComments__field__1, withComments__field__2 ) =
                App.WithComments
                    (Pine_builtin.concat
                        [ earlier, withComments__field__1 ]
                    )
                    withComments__field__2

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }

    [Fact]
    public void Single_choice_tag_two_arguments_one_generic_parameter_pattern()
    {
        // Same as above but using parameter-level destructuring instead of case-of
        var elmModuleText =
            """"
            module App exposing (..)


            type alias Comments =
                List String


            type WithComments res
                = WithComments Comments res


            merge : Comments -> WithComments a -> WithComments a
            merge earlier (WithComments laterComments res) =
                WithComments (Pine_builtin.concat [ earlier, laterComments ]) res


            root : List String -> List String -> Int -> WithComments Int
            root earlier later value =
                merge earlier (WithComments later value)

            """";

        var expectedElmModuleText =
            """"
            module App exposing (..)


            type alias Comments =
                List.List String


            type WithComments res
                = WithComments App.Comments res


            merge : App.Comments -> App.WithComments a -> App.WithComments a
            merge earlier (App.WithComments laterComments res) =
                App.WithComments
                    (Pine_builtin.concat
                        [ earlier, laterComments ]
                    )
                    res


            root : List.List String -> List.List String -> Int -> App.WithComments Int
            root earlier later value =
                merge__specialized__1
                    earlier
                    ( later, value )


            merge__specialized__1 earlier ( laterComments, res ) =
                App.WithComments
                    (Pine_builtin.concat
                        [ earlier, laterComments ]
                    )
                    res

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }

    [Fact]
    public void Single_choice_tag_single_function_argument_directly_annotated()
    {
        var elmModuleText =
            """"
            module App exposing (..)


            type TaggedFunc a b
                = TaggedFunc (a -> b)


            listMapTagged : TaggedFunc a b -> List a -> List b
            listMapTagged taggedFunc list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        case taggedFunc of
                            TaggedFunc f ->
                                f first :: listMapTagged taggedFunc rest


            root : (Int -> Int) -> List Int -> List Int
            root f list =
                listMapTagged (TaggedFunc f) list

            """";

        var expectedElmModuleText =
            """"
            module App exposing (..)


            type TaggedFunc a b
                = TaggedFunc (a -> b)


            listMapTagged : App.TaggedFunc a b -> List.List a -> List.List b
            listMapTagged taggedFunc list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        case taggedFunc of
                            App.TaggedFunc f ->
                                List.cons
                                    (f
                                        first
                                    )
                                    (App.listMapTagged
                                        taggedFunc
                                        rest
                                    )


            root : (Int -> Int) -> List.List Int -> List.List Int
            root f list =
                listMapTagged__specialized__1
                    f
                    list


            listMapTagged__specialized__1 taggedFunc__field__1 list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        List.cons
                            (taggedFunc__field__1
                                first
                            )
                            (listMapTagged__specialized__1
                                taggedFunc__field__1
                                rest
                            )

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }
}
