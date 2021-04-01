%% Copyright (c) 2018, Jabberbees SAS

%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.

%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @author Emmanuel Boutin <emmanuel.boutin@jabberbees.com>

-module(egherkin_lib_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("assert.hrl").

all() -> [
    format_gwt_works,

    format_step_parts_only_binaries,
    format_step_parts_docstring,
    format_step_parts_datatable,

    datatable_to_iolist_2_no_keys_no_rows,
    datatable_to_iolist_2_no_keys,
    datatable_to_iolist_2_no_rows,
    datatable_to_iolist_2_1_row,

    datatable_to_iolist_3_no_highlight,
    datatable_to_iolist_3_1_keys_highlighted,
    datatable_to_iolist_3_1_row_highlighted,
    datatable_to_iolist_3_1_cell_highlighted,

    format_table_line_2_returns_empty,
    format_table_line_2_handles_incorrect_widths,
    format_table_line_2_works,

    format_table_line_3_returns_empty,
    format_table_line_3_handles_incorrect_widths,
    format_table_line_3_works,
    format_table_line_3_highlights_first_column,
    format_table_line_3_highlights_middle_column,
    format_table_line_3_highlights_last_column
].

%%region format_gwt/1

format_gwt_works(_) ->
	?assertEqual(<<"Given">>, egherkin_lib:format_gwt(given_keyword)),
	?assertEqual(<<"When">>, egherkin_lib:format_gwt(when_keyword)),
	?assertEqual(<<"Then">>, egherkin_lib:format_gwt(then_keyword)),
	?assertEqual(<<"And">>, egherkin_lib:format_gwt(and_keyword)),
	?assertEqual(<<"But">>, egherkin_lib:format_gwt(but_keyword)),
    ok.

%%endregion

%%region format_step_parts/1

format_step_parts_only_binaries(_) ->
    Parts = [<<"I">>, <<"love">>, <<"wood">>],
	?assertEqual(<<
        "I love wood"
    >>, iolist_to_binary(egherkin_lib:format_step_parts(Parts))).

format_step_parts_docstring(_) ->
    Parts = [
        <<"I">>, <<"love">>, <<"this:">>,
        {docstring, [<<"wood">>]}
    ],
	?assertEqual(<<
        "I love this:\n"
        "\"\"\"\n"
        "wood\n"
        "\"\"\""
    >>, iolist_to_binary(egherkin_lib:format_step_parts(Parts))).

format_step_parts_datatable(_) ->
    Parts = [
        <<"I">>, <<"love">>, <<"this:">>,
        egherkin_datatable:new([<<"a">>, <<"b">>, <<"c">>],
            [[<<"foo">>, <<"bar">>, <<"foobar">>]])
    ],
	?assertEqual(<<
        "I love this:\n"
        "| a   | b   | c      |\n"
        "| foo | bar | foobar |"
    >>, iolist_to_binary(egherkin_lib:format_step_parts(Parts))).

%%endregion

%%region datatable_to_iolist/1

datatable_to_iolist_2_no_keys_no_rows(_) ->
    Table = egherkin_datatable:new([], []),
	?assertEqual(<<
        "| <empty> |"
    >>, iolist_to_binary(datatable_to_iolist(Table))).

datatable_to_iolist_2_no_keys(_) ->
    Table = egherkin_datatable:new([], [
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ]),
	?assertEqual(<<
        "| <empty> |"
    >>, iolist_to_binary(datatable_to_iolist(Table))).

datatable_to_iolist_2_no_rows(_) ->
    Table = egherkin_datatable:new([<<"a">>, <<"b">>, <<"c">>], []),
	?assertEqual(<<
        "| a | b | c |"
    >>, iolist_to_binary(datatable_to_iolist(Table))).

datatable_to_iolist_2_1_row(_) ->
    Table = egherkin_datatable:new([<<"a">>, <<"b">>, <<"c">>], [
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ]),
	?assertEqual(<<
        "| a   | b   | c      |\n"
        "| foo | bar | foobar |"
    >>, iolist_to_binary(datatable_to_iolist(Table))).

%%endregion

%%region datatable_to_iolist/3

datatable_to_iolist_3_no_highlight(_) ->
    Table = egherkin_datatable:new([<<"a">>, <<"b">>, <<"c">>], [
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ]),
	?assertEqual(<<
        "| a   | b   | c      |\n"
        "| foo | bar | foobar |"
    >>, iolist_to_binary(datatable_to_iolist(Table))).

datatable_to_iolist_3_1_keys_highlighted(_) ->
    Table = egherkin_datatable:new([<<"a">>, <<"b">>, <<"c">>], [
        [<<"foo">>, <<"bar">>, <<"foobar">>],
        [<<"foo">>, <<"bar">>, <<"foobar">>],
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ]),
	?assertEqual(<<
        "> a   | b   | c      <\n"
        "| foo | bar | foobar |\n"
        "| foo | bar | foobar |\n"
        "| foo | bar | foobar |"
    >>, iolist_to_binary(datatable_to_iolist(Table, keys))).

datatable_to_iolist_3_1_row_highlighted(_) ->
    Table = egherkin_datatable:new([<<"a">>, <<"b">>, <<"c">>], [
        [<<"foo">>, <<"bar">>, <<"foobar">>],
        [<<"foo">>, <<"bar">>, <<"foobar">>],
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ]),
	?assertEqual(<<
        "| a   | b   | c      |\n"
        "| foo | bar | foobar |\n"
        "> foo | bar | foobar <\n"
        "| foo | bar | foobar |"
    >>, iolist_to_binary(datatable_to_iolist(Table, {row, 2}))).

datatable_to_iolist_3_1_cell_highlighted(_) ->
    Table = egherkin_datatable:new([<<"a">>, <<"b">>, <<"c">>], [
        [<<"foo">>, <<"bar">>, <<"foobar">>],
        [<<"foo">>, <<"bar">>, <<"foobar">>],
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ]),
	?assertEqual(<<
        "| a   | b   | c      |\n"
        "| foo | bar | foobar |\n"
        "> foo < bar | foobar |\n"
        "| foo | bar | foobar |"
    >>, iolist_to_binary(datatable_to_iolist(Table, {row, 2, <<"a">>}))).

%%endregion

%%region format_table_line/2

format_table_line_2_returns_empty(_) ->
    Values = [],
    Widths = [],
    Opts = egherkin_lib:default_opts(),
	?assertEqual(<<"">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths, Opts))).

format_table_line_2_handles_incorrect_widths(_) ->
    Values = [<<"abcd">>, <<"abcd">>, <<"abcd">>],
    Widths = [0, 0, 0],
    Opts = egherkin_lib:default_opts(),
	?assertEqual(<<"| abcd | abcd | abcd |">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths, Opts))).

format_table_line_2_works(_) ->
    Values = [<<"abcd">>, <<"abcd">>, <<"abcd">>],
    Widths = [4, 5, 6],
    Opts = egherkin_lib:default_opts(),
	?assertEqual(<<"| abcd | abcd  | abcd   |">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths, Opts))).

%%endregion

%%region format_table_line/3

format_table_line_3_returns_empty(_) ->
    Values = [],
    Widths = [],
    Highlights = [none, none, none],
    Opts = egherkin_lib:default_opts(),
	?assertEqual(<<"">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths, Highlights, Opts))).

format_table_line_3_handles_incorrect_widths(_) ->
    Values = [<<"abcd">>, <<"abcd">>, <<"abcd">>],
    Widths = [0, 0, 0],
    Highlights = [none, none, none],
    Opts = egherkin_lib:default_opts(),
	?assertEqual(<<"| abcd | abcd | abcd |">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths, Highlights, Opts))).

format_table_line_3_works(_) ->
    Values = [<<"abcd">>, <<"abcd">>, <<"abcd">>],
    Widths = [4, 5, 6],
    Highlights = [none, none, none],
    Opts = egherkin_lib:default_opts(),
	?assertEqual(<<"| abcd | abcd  | abcd   |">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths, Highlights, Opts))).

format_table_line_3_highlights_first_column(_) ->
    Values = [<<"abcd">>, <<"abcd">>, <<"abcd">>],
    Widths = [4, 5, 6],
    Highlights = [highlight, none, none],
    Opts = egherkin_lib:default_opts(),
	?assertEqual(<<"> abcd < abcd  | abcd   |">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths, Highlights, Opts))).

format_table_line_3_highlights_middle_column(_) ->
    Values = [<<"abcd">>, <<"abcd">>, <<"abcd">>],
    Widths = [4, 5, 6],
    Highlights = [none, highlight, none],
    Opts = egherkin_lib:default_opts(),
	?assertEqual(<<"| abcd > abcd  < abcd   |">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths, Highlights, Opts))).

format_table_line_3_highlights_last_column(_) ->
    Values = [<<"abcd">>, <<"abcd">>, <<"abcd">>],
    Widths = [4, 5, 6],
    Highlights = [none, none, highlight],
    Opts = egherkin_lib:default_opts(),
	?assertEqual(<<"| abcd | abcd  > abcd   <">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths, Highlights, Opts))).

%%endregion

%%region helpers

datatable_to_iolist(Table) ->
    datatable_to_iolist(Table, none).

datatable_to_iolist(Table, Highlight) ->
    Opts = egherkin_lib:default_opts(),
    egherkin_lib:datatable_to_iolist(Table, Highlight, Opts).

%%endregion
