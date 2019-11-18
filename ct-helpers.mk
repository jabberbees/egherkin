# Common Test make helpers

#CT_OPTS = -erl_args -config $(TEST_DIR)/ct.config

CT_ERL_OPTS = -noinput \
	-pa $(CURDIR)/ebin $(DEPS_DIR)/*/ebin $(APPS_DIR)/*/ebin $(TEST_DIR) \
	$(CT_EXTRA) \
	$(CT_OPTS)

define ctdbg.erl
	ParseBreakpoint = fun(S) ->
		case string:tokens(S, ":") of
		[M] -> list_to_atom(M);
		[M, L] -> {list_to_atom(M), list_to_integer(L)}
		end
	end,

	ModuleSource = fun(Module) ->
		ModuleInfo = Module:module_info(),
		case lists:keyfind(compile, 1, ModuleInfo) of 
		{compile, Compile} ->
			case lists:keyfind(source, 1, Compile) of 
			{source, Source} ->
				Source;
			_ -> 
				unavailable
			end;
		_ -> 
			unavailable
		end
	end,
	BreakOn = fun(Flags) ->
		i:iaa(Flags)
	end,
	GetIaa = fun() ->
		case int:auto_attach() of
		false -> [];
		{Flags, _} -> Flags
		end
	end,
	AddBreakpoint = fun
		(Module) when is_atom(Module) ->
			io:format("Adding breakpoint: module ~s~n", [Module]),
			i:im(),
			Src = ModuleSource(Module),
			case i:ii(Src) of
			{module, _} -> 
				BreakOn(GetIaa() ++ [init]),
				ok;
			error -> error
			end;
		({Module, LineOrLines}) when is_atom(Module) ->
			io:format("Adding breakpoint: module ~s, line ~p~n", [Module, LineOrLines]),
			i:im(),
			Src = ModuleSource(Module),
			case i:ii(Src) of
			{module, _} ->
				case LineOrLines of
				Line when is_integer(Line) -> i:ib(Module, Line);
				Lines when is_list(Lines) -> [i:ib(Module, Line) || Line <- Lines]
				end,
				BreakOn(GetIaa() ++ [break]),
				ok;
			error ->
				error
			end
	end,

	[ModuleL, TestCaseL | _] = string:tokens($1, ":"),
 
	{Module, Suite} = case lists:suffix("_SUITE", ModuleL) of
	true ->
		{
			list_to_atom(string:substr(ModuleL, 1, length(ModuleL)-6)),
			list_to_atom(ModuleL)
		};
	false ->
		{
			list_to_atom(ModuleL),
			list_to_atom(ModuleL ++ "_SUITE")
		}
    end,

	TestCase = list_to_atom(TestCaseL),

	Breakpoints = [ParseBreakpoint(I) || I <- string:tokens($2, ",; ")],

	Breakpoints2 = case Breakpoints of
	[] -> [Module];
	_ -> Breakpoints
	end,

	[AddBreakpoint(Breakpoint) || Breakpoint <- Breakpoints2],

	CTOpts = [
		{auto_compile, false},
		{dir, "test"},
		{logdir, "logs"},
		{suite, Suite},
		{testcase, TestCase}
	],

	case ct:run_test(CTOpts) of
	{Ok, Failed, {UserSkipped, AutoSkipped}} ->
		io:format("~p ok, ~p failed, ~p user skipped, ~p auto skipped~n", [Ok, Failed, UserSkipped, AutoSkipped]);
	{error, Reason} ->
		io:format("error: ~p~n", [Reason])
	end,
	halt()
endef

ctdbg: test-build
	$(gen_verbose) $(call erlang,$(call ctdbg.erl,"$(t)","$(b)"),$(CT_ERL_OPTS))
