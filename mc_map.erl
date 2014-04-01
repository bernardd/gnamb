-module(mc_map).

-compile(export_all).

init() ->
	ets:new(chunks, [named_table, public, {key_pos, 2}]).
