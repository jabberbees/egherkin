# egherkin ![CI Status](https://github.com/jabberbees/egherkin/workflows/CI/badge.svg)
egherkin is an Erlang library for parsing Gherkin documents.

## Goals
egherkin aims at providing a *complete* Gherkin parser in a *small* code base.

For efficiency egherkin parses Gherkin documents loaded as binaries and produces a binary based memory model for processing.
  
egherkin is *clean* and *well tested* Erlang code.

## How to build
egherkin uses erlang.mk as build tool so you only need a recent version of *make*:

    git clone https://github.com/jabberbees/egherkin
    cd egherkin
    make

To run tests:

    make ct

## How to use

    egherkin:parse(Document :: binary()) -> ParseResult

    egherkin:parse_file(Filename :: string()) -> ParseResult

    ParseResult = Feature | {failed, Line :: integer(), ErrorMessage :: string()}

## Parsed model

    Feature = {Headers, Tags, Name, Description, Background, Scenarios}
    Headers = [{Line :: integer(), Header :: binary()}]
    Tags = [{Line :: integer(), Name :: binary()}]
    Description = [DescriptionLine :: binary()]
    Background = {Line :: integer(), Steps}
    Scenarios = [Scenario]
    Scenario = {Line :: integer(), Name :: binary(), Tags, Steps}
        | {Line :: integer(), Name :: binary(), Tags, Steps, Examples}
    Steps = [Step]
    Step = {Line :: integer(), GWT, StepParts}
    Examples = DataTable
    GWT = given_keyword | when_keyword | then_keyword | and_keyword | but_keyword
    StepParts = [StepPart]
    StepPart = binary() | DocString | DataTable
    DocString = {docstring, [Line :: binary()]}
    DataTable = {datatable,
                 [RowName :: binary()],
                 [DataTableRow]}
    DataTableRow = [Value :: binary()]

## Compatibility
egherkin is tested against Erlang OTP version 19, 20, 21, 22, 23, 24.
