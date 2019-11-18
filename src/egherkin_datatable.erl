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

-module(egherkin_datatable).

-export([
    new/2,
    new/3,
    line/1,
    keys/1,
    rows/1,
    rows_as_proplists/1,
    rows_map/2,
    are_equal/2,
    are_equal_unordered/2,
    matches/4
]).

-record(datatable, {
    line,
    keys,
    rows
}).

new(Keys, Rows) ->
    #datatable{keys = Keys, rows = Rows}.

new(Line, Keys, Rows) ->
    #datatable{line = Line, keys = Keys, rows = Rows}.

line(#datatable{line = Line}) ->
	Line.

keys(#datatable{keys = Keys}) ->
	Keys.

rows(#datatable{rows = Rows}) ->
	Rows.

rows_as_proplists(#datatable{keys = Keys, rows = Rows}) ->
	[lists:zip(Keys, Row) || Row <- Rows].

rows_map(Fun, DataTable) when is_function(Fun, 1) ->
    lists:map(Fun, rows(DataTable));
rows_map(Fun, DataTable) when is_function(Fun, 2) ->
    Keys = keys(DataTable),
    [Fun(Keys, Row) || Row <- rows(DataTable)].

are_equal(DT1, DT2) ->
    K1 = keys(DT1),
    K2 = keys(DT2),
    R1 = rows(DT1),
    R2 = rows(DT2),
    K1 =:= K2 andalso R1 =:= R2.

are_equal_unordered(DT1, DT2) ->
    K1 = keys(DT1),
    K2 = keys(DT2),
    R1 = lists:sort(rows(DT1)),
    R2 = lists:sort(rows(DT2)),
    K1 =:= K2 andalso R1 =:= R2.

matches(Data, Projection, Comparison, #datatable{keys = Keys} = DataTable) ->
    Rows1 = project_data(Keys, Data, Projection),
    Rows2 = rows(DataTable),
    Pred = fun({A, B}) -> row_compare(A, B, Comparison) end,
    lists:all(Pred, lists:zip(Rows1, Rows2)).

project_data(_Keys, Data, Fun) when is_function(Fun, 1) ->
    lists:map(Fun, Data);
project_data(Keys, Data, Fun) when is_function(Fun, 2) ->
    [Fun(Keys, Row) || Row <- Data];
project_data(Keys, Data, Projection) when is_list(Projection) ->
    [project_data_row(Keys, Row, Projection) || Row <- Data].

project_data_row(Keys, Row, Projection) ->
    [Proj(Key, Row) || {Key, Proj} <- lists:zip(Keys, Projection)].

row_compare(Row1, Row2, Comparison) when is_function(Comparison, 2) ->
    Comparison(Row1, Row2);
row_compare(Row1, Row2, Comparison) when is_list(Comparison) ->
    lists:foldl(fun
    (_, false) -> false;
    ({V1, V2, Fun}, _) -> Fun(V1, V2)
    end, true, lists:zip3(Row1, Row2, Comparison)).
