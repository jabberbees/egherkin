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
    new_2_works,

    keys_works,
    
    rows_works,

    rows_as_valuesproplists,

    rows_map_1_works,
    rows_map_2_works,

    replace_rows_2_works,

    replace_rows_3_projection_fun_1,
    replace_rows_3_projection_fun_2,

    are_equal_returns_true,
    are_equal_returns_false,

    are_equal_unordered_returns_true,
    are_equal_unordered_returns_false,

    matches_3_returns_nomatch_a_shorter_than_b,
    matches_3_returns_nomatch_a_longer_than_b,
    matches_3_returns_match_comparison_fun_2,
    matches_3_returns_nomatch_comparison_fun_2,
    matches_3_returns_match_comparison_fun_3,
    matches_3_returns_nomatch_comparison_fun_3,
    matches_3_returns_nomatch_with_key_comparison_fun_3,

    matches_4_returns_match_projection_fun_1,
    matches_4_returns_match_projection_fun_2,
    matches_4_returns_match_comparison_fun_2,
    matches_4_returns_nomatch_comparison_fun_2,
    matches_4_returns_match_comparison_fun_3,
    matches_4_returns_nomatch_comparison_fun_3,
    matches_4_returns_nomatch_with_key_comparison_fun_3,
    matches_4_returns_match_json_list,
    matches_4_returns_nomatch_with_key_json_list
].

%%region new/2

new_2_works(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"foo">>, <<"bar">>, <<"foobar">>]
    ],
	?assertEqual({datatable, Keys, Rows},
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

%%region replace_rows/2

replace_rows_2_works(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DataTable = egherkin_datatable:new(Keys, Rows),
    NewRows = [
        [<<"true">>, <<"foobar">>, <<"1">>],
        [<<"true">>, <<"foobar">>, <<"2">>],
        [<<"true">>, <<"foobar">>, <<"3">>]
    ],
    Expected = egherkin_datatable:new(Keys, NewRows),
    Actual = egherkin_datatable:replace_rows(NewRows, DataTable),
	?assertEqual(true, egherkin_datatable:are_equal(Expected, Actual)).

%%endregion

%%region replace_rows/3

replace_rows_3_projection_fun_1(_) ->
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
    Expected = egherkin_datatable:new(Keys, Rows),
    Actual = egherkin_datatable:replace_rows(Data, Projection, DataTable),
	?assertEqual(true, egherkin_datatable:are_equal(Expected, Actual)).

replace_rows_3_projection_fun_2(_) ->
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
    Expected = egherkin_datatable:new(Keys, Rows),
    Actual = egherkin_datatable:replace_rows(Data, Projection, DataTable),
	?assertEqual(true, egherkin_datatable:are_equal(Expected, Actual)).

%%endregion

%%region matches/3

matches_3_returns_nomatch_a_shorter_than_b(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    RowsA = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    RowsB = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>],
        [<<"4">>, <<"true">>, <<"foobar">>]
    ],
    DTA = egherkin_datatable:new(Keys, RowsA),
    DTB = egherkin_datatable:new(Keys, RowsB),
    Comparison = fun(RowA, RowB) -> RowA =:= RowB end,
	?assertEqual({nomatch, match, {row, 4}},
        egherkin_datatable:matches(DTA, Comparison, DTB)).

matches_3_returns_nomatch_a_longer_than_b(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    RowsA = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>],
        [<<"4">>, <<"true">>, <<"foobar">>]
    ],
    RowsB = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DTA = egherkin_datatable:new(Keys, RowsA),
    DTB = egherkin_datatable:new(Keys, RowsB),
    Comparison = fun(RowA, RowB) -> RowA =:= RowB end,
	?assertEqual({nomatch, {row, 4}, match},
        egherkin_datatable:matches(DTA, Comparison, DTB)).

matches_3_returns_match_comparison_fun_2(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    RowsA = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    RowsB = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DTA = egherkin_datatable:new(Keys, RowsA),
    DTB = egherkin_datatable:new(Keys, RowsB),
    Comparison = fun(RowA, RowB) -> RowA =:= RowB end,
	?assertEqual(match,
        egherkin_datatable:matches(DTA, Comparison, DTB)).

matches_3_returns_nomatch_comparison_fun_2(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    RowsA = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    RowsB = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"false">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DTA = egherkin_datatable:new(Keys, RowsA),
    DTB = egherkin_datatable:new(Keys, RowsB),
    Comparison = fun(RowA, RowB) -> RowA =:= RowB end,
	?assertEqual({nomatch, {row, 2}},
        egherkin_datatable:matches(DTA, Comparison, DTB)).

matches_3_returns_match_comparison_fun_3(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DTA = egherkin_datatable:new(Keys, Rows),
    DTB = egherkin_datatable:new(Keys, Rows),
    Comparison = fun(K, RowA, RowB) -> K =:= Keys andalso RowA =:= RowB end,
	?assertEqual(match,
        egherkin_datatable:matches(DTA, Comparison, DTB)).

matches_3_returns_nomatch_comparison_fun_3(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    RowsA = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    RowsB = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"false">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DTA = egherkin_datatable:new(Keys, RowsA),
    DTB = egherkin_datatable:new(Keys, RowsB),
    Comparison = fun
    (K, R, R) when K =:= Keys -> match;
    (_, _, _) -> nomatch
    end,
	?assertEqual({nomatch, {row, 2}},
        egherkin_datatable:matches(DTA, Comparison, DTB)).

matches_3_returns_nomatch_with_key_comparison_fun_3(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    RowsA = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    RowsB = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"false">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DTA = egherkin_datatable:new(Keys, RowsA),
    DTB = egherkin_datatable:new(Keys, RowsB),
    Comparison = fun
    (K, R, R) when K =:= Keys -> match;
    (_, [<<"2">>, <<"true">>, <<"foobar">>], [<<"2">>, <<"false">>, <<"foobar">>]) -> {nomatch, <<"b">>};
    (_, _, _) -> nomatch
    end,
	?assertEqual({nomatch, {row, 2, <<"b">>}},
        egherkin_datatable:matches(DTA, Comparison, DTB)).

%%endregion

%%region matches/4

matches_4_returns_match_projection_fun_1(_) ->
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
	?assertEqual(match,
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

matches_4_returns_match_projection_fun_2(_) ->
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
	?assertEqual(match,
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

matches_4_returns_match_comparison_fun_2(_) ->
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
	?assertEqual(match,
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

matches_4_returns_nomatch_comparison_fun_2(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DataTable = egherkin_datatable:new(Keys, Rows),
    Data = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"false">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    Projection = fun(Row) -> Row end,
    Comparison = fun(RowA, RowB) -> RowA =:= RowB end,
	?assertEqual({nomatch, {row, 2}},
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

matches_4_returns_match_comparison_fun_3(_) ->
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
    Comparison = fun(K, RowA, RowB) -> K =:= Keys andalso RowA =:= RowB end,
	?assertEqual(match,
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

matches_4_returns_nomatch_comparison_fun_3(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DataTable = egherkin_datatable:new(Keys, Rows),
    Data = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"false">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    Projection = fun(Row) -> Row end,
    Comparison = fun
    (_, R, R) -> match;
    (_, _, _) -> nomatch
    end,
	?assertEqual({nomatch, {row, 2}},
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

matches_4_returns_nomatch_with_key_comparison_fun_3(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DataTable = egherkin_datatable:new(Keys, Rows),
    Data = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"false">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    Projection = fun(Row) -> Row end,
    Comparison = fun
    (_, R, R) -> match;
    (_, _, _) -> {nomatch, <<"b">>}
    end,
	?assertEqual({nomatch, {row, 2, <<"b">>}},
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

matches_4_returns_match_json_list(_) ->
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
	?assertEqual(match,
        egherkin_datatable:matches(Data, Projection, Comparison, DataTable)).

matches_4_returns_nomatch_with_key_json_list(_) ->
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    Rows = [
        [<<"1">>, <<"true">>, <<"foobar">>],
        [<<"2">>, <<"true">>, <<"foobar">>],
        [<<"3">>, <<"true">>, <<"foobar">>]
    ],
    DataTable = egherkin_datatable:new(Keys, Rows),
    Data = [
        [{<<"a">>, 1}, {<<"b">>, true}, {<<"c">>, <<"foobar">>}],
        [{<<"a">>, 2}, {<<"b">>, false}, {<<"c">>, <<"foobar">>}],
        [{<<"a">>, 3}, {<<"b">>, true}, {<<"c">>, <<"foobar">>}]
    ],
    Projection = lists:duplicate(3, fun proplists:get_value/2),
    Comparison = lists:duplicate(3, fun match_json_value/2),
	?assertEqual({nomatch, {row, 2, <<"b">>}},
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
