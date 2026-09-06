module Input exposing (..)

type PathDescription a
    = DescribePathNode a (PathDescription a)
    | DescribePathEnd a ,
