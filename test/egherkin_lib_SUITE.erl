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

-include_lib("common_test/include/ct.hrl").
-include_lib("assert.hrl").

all() -> [
    datatable_to_iolist_no_keys_no_rows,
    datatable_to_iolist_no_keys,
    datatable_to_iolist_no_rows,
    datatable_to_iolist_1_row,

    format_table_line_1,
    format_table_line_2,
    format_table_line_3
].

%%region datatable_to_iolist

datatable_to_iolist_no_keys_no_rows(_) ->
    Table = egherkin_datatable:new([], []),
	?assertEqual(<<
        "| <empty> |\n"
    >>, iolist_to_binary(egherkin_lib:datatable_to_iolist(Table))).

datatable_to_iolist_no_keys(_) ->
    Table = egherkin_datatable:new([], [
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ]),
	?assertEqual(<<
        "| <empty> |\n"
    >>, iolist_to_binary(egherkin_lib:datatable_to_iolist(Table))).

datatable_to_iolist_no_rows(_) ->
    Table = egherkin_datatable:new([<<"a">>, <<"b">>, <<"c">>], []),
	?assertEqual(<<
        "| a | b | c |\n"
    >>, iolist_to_binary(egherkin_lib:datatable_to_iolist(Table))).

datatable_to_iolist_1_row(_) ->
    Table = egherkin_datatable:new([<<"a">>, <<"b">>, <<"c">>], [
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ]),
	?assertEqual(<<
        "| a   | b   | c      |\n"
        "| foo | bar | foobar |\n"
    >>, iolist_to_binary(egherkin_lib:datatable_to_iolist(Table))).

%%endregion

%%region format_table_line

format_table_line_1(_) ->
    Values = [],
    Widths = [],
	?assertEqual(<<"">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths))).

format_table_line_2(_) ->
    Values = [<<"abcd">>, <<"abcd">>, <<"abcd">>],
    Widths = [0, 0, 0],
	?assertEqual(<<"| abcd | abcd | abcd |">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths))).

format_table_line_3(_) ->
    Values = [<<"abcd">>, <<"abcd">>, <<"abcd">>],
    Widths = [4, 5, 6],
	?assertEqual(<<"| abcd | abcd  | abcd   |">>,
        iolist_to_binary(egherkin_lib:format_table_line(Values, Widths))).

%%endregion
