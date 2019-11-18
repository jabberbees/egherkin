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

-module(egherkin_datatable_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("assert.hrl").

all() -> [
    new_works,

    keys_works,
    
    rows_works,

    rows_as_valuesproplists,

    rows_map_1_works,
    rows_map_2_works,

    are_equal_returns_true,
    are_equal_returns_false,

    are_equal_unordered_returns_true,
    are_equal_unordered_returns_false,

    matches_4_returns_true_projection_fun_1,
    matches_4_returns_true_projection_fun_2,
    matches_4_returns_true_comparison_fun_2,
    matches_4_returns_true_json_list
].

%%region new

new_works(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ],
	?assertEqual({datatable, undefined, Keys, Rows},
        egherkin_datatable:new(Keys, Rows)).

%%endregion

%%region keys

keys_works(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ],
    DataTable = egherkin_datatable:new(Keys, Rows),
	?assertEqual(Keys,
        egherkin_datatable:keys(DataTable)).

%%endregion

%%region rows

rows_works(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ],
    DataTable = egherkin_datatable:new(Keys, Rows),
	?assertEqual(Rows,
        egherkin_datatable:rows(DataTable)).

%%endregion

%%region rows_as_proplists

rows_as_valuesproplists(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Row = [<<"foo">>, <<"bar">>, <<"foobar">>],
    Rows = [Row, Row, Row],
    PropList = lists:zip(Keys, Row),
    DataTable = egherkin_datatable:new(Keys, Rows),
	?assertEqual([PropList, PropList, PropList],
        egherkin_datatable:rows_as_proplists(DataTable)).

%%endregion

%%region rows_map

rows_map_1_works(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Row = [<<"foo">>, <<"bar">>, <<"foobar">>],
    Rows = [Row, Row, Row],
    DataTable = egherkin_datatable:new(Keys, Rows),
    Fun = fun(R) -> length(R) end,
	?assertEqual([3, 3, 3],
        egherkin_datatable:rows_map(Fun, DataTable)).

rows_map_2_works(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Row = [<<"foo">>, <<"bar">>, <<"foobar">>],
    Rows = [Row, Row, Row],
    DataTable = egherkin_datatable:new(Keys, Rows),
    Fun = fun(K, R) -> lists:zip(K, R) end,
    PropList = lists:zip(Keys, Row),
	?assertEqual([PropList, PropList, PropList],
        egherkin_datatable:rows_map(Fun, DataTable)).

%%endregion

%%region are_equal

are_equal_returns_true(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows1 = [
        [<<"foo1">>, <<"bar1">>, <<"foobar1">>],
        [<<"foo2">>, <<"bar2">>, <<"foobar2">>],
        [<<"foo3">>, <<"bar3">>, <<"foobar3">>]
    ],
    Rows2 = [
        [<<"foo1">>, <<"bar1">>, <<"foobar1">>],
        [<<"foo2">>, <<"bar2">>, <<"foobar2">>],
        [<<"foo3">>, <<"bar3">>, <<"foobar3">>]
    ],
    DT1 = egherkin_datatable:new(Keys, Rows1),
    DT2 = egherkin_datatable:new(Keys, Rows2),
	?assertEqual(true, egherkin_datatable:are_equal(DT1, DT2)).

are_equal_returns_false(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows1 = [
        [<<"foo1">>, <<"bar1">>, <<"foobar1">>],
        [<<"foo2">>, <<"bar2">>, <<"foobar2">>],
        [<<"foo3">>, <<"bar3">>, <<"foobar3">>]
    ],
    Rows2 = [
        [<<"foo1x">>, <<"bar1">>, <<"foobar1">>],
        [<<"foo2">>,  <<"bar2">>, <<"foobar2">>],
        [<<"foo3">>,  <<"bar3">>, <<"foobar3">>]
    ],
    DT1 = egherkin_datatable:new(Keys, Rows1),
    DT2 = egherkin_datatable:new(Keys, Rows2),
	?assertEqual(false, egherkin_datatable:are_equal(DT1, DT2)).

%%endregion

%%region are_equal_unordered

are_equal_unordered_returns_true(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows1 = [
        [<<"foo1">>, <<"bar1">>, <<"foobar1">>],
        [<<"foo2">>, <<"bar2">>, <<"foobar2">>],
        [<<"foo3">>, <<"bar3">>, <<"foobar3">>]
    ],
    Rows2 = [
        [<<"foo2">>, <<"bar2">>, <<"foobar2">>],
        [<<"foo1">>, <<"bar1">>, <<"foobar1">>],
        [<<"foo3">>, <<"bar3">>, <<"foobar3">>]
    ],
    DT1 = egherkin_datatable:new(Keys, Rows1),
    DT2 = egherkin_datatable:new(Keys, Rows2),
	?assertEqual(true, egherkin_datatable:are_equal_unordered(DT1, DT2)).

are_equal_unordered_returns_false(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows1 = [
        [<<"foo1">>, <<"bar1">>, <<"foobar1">>],
        [<<"foo2">>, <<"bar2">>, <<"foobar2">>],
        [<<"foo3">>, <<"bar3">>, <<"foobar3">>]
    ],
    Rows2 = [
        [<<"foo1x">>, <<"bar1">>, <<"foobar1">>],
        [<<"foo2">>, <<"bar2">>, <<"foobar2">>],
        [<<"foo3">>, <<"bar3">>, <<"foobar3">>]
    ],
    DT1 = egherkin_datatable:new(Keys, Rows1),
    DT2 = egherkin_datatable:new(Keys, Rows2),
	?assertEqual(false, egherkin_datatable:are_equal_unordered(DT1, DT2)).

%%endregion

%%region matches/4

matches_4_returns_true_projection_fun_1(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DataTable = egherkin_datatable:new(Keys, Rows),
    Data = [
        [<<"true">>, <<"foobar">>, <<"1">>],
        [<<"true">>, <<"foobar">>, <<"2">>],
        [<<"true">>, <<"foobar">>, <<"3">>]
    ],
    Projection = fun([B, C, A]) -> [A, B, C] end,
    Comparison = fun(RowA, RowB) -> RowA =:= RowB end,
	?assertEqual(true,
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

matches_4_returns_true_projection_fun_2(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DataTable = egherkin_datatable:new(Keys, Rows),
    Data = [
        [{<<"a">>, <<"1">>}, {<<"b">>, <<"true">>}, {<<"c">>, <<"foobar">>}],
        [{<<"a">>, <<"2">>}, {<<"b">>, <<"true">>}, {<<"c">>, <<"foobar">>}],
        [{<<"a">>, <<"3">>}, {<<"b">>, <<"true">>}, {<<"c">>, <<"foobar">>}]
    ],
    Projection = fun(K, R) -> [proplists:get_value(I, R) || I <- K] end,
    Comparison = fun(RowA, RowB) -> RowA =:= RowB end,
	?assertEqual(true,
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

matches_4_returns_true_comparison_fun_2(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DataTable = egherkin_datatable:new(Keys, Rows),
    Data = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    Projection = fun(Row) -> Row end,
    Comparison = fun(RowA, RowB) -> RowA =:= RowB end,
	?assertEqual(true,
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

matches_4_returns_true_json_list(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DataTable = egherkin_datatable:new(Keys, Rows),
    Data = [
        [{<<"a">>, 1}, {<<"b">>, true}, {<<"c">>, <<"foobar">>}],
        [{<<"a">>, 2}, {<<"b">>, true}, {<<"c">>, <<"foobar">>}],
        [{<<"a">>, 3}, {<<"b">>, true}, {<<"c">>, <<"foobar">>}]
    ],
    Projection = lists:duplicate(3, fun proplists:get_value/2),
    Comparison = lists:duplicate(3, fun match_json_value/2),
	?assertEqual(true,
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

%%endregion

%%region helpers

match_json_value(DV, TV) when is_integer(DV) ->
    binary_to_integer(TV) == DV; 
match_json_value(DV, TV) when is_float(DV) ->
    binary_to_float(TV) == DV; 
match_json_value(DV, TV) when is_atom(DV) ->
    TV == list_to_binary(atom_to_list(DV));
match_json_value(DV, TV) when is_list(DV) ->
    TV == iolist_to_binary(DV);
match_json_value(DV, TV) ->
    TV == DV.

%%endregion
