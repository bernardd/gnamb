-record(
	disconnect, {
		reason
	}).
-record(
	encryption_request, {
		server_id,
		public_key,
		verify_token
	}).

-record(
	login_success, {
		uuid,
		username
	}).

-record(
	keep_alive, {
		id
	}).

-record(
	join_game, {
		entity_id,
		game_mode,
		dimension,
		difficult,
		max_players,
		level_type
	}).

-record(
	chat_message, {
		message
	}).

-record(
	time_update, {
		world_age,
		time_of_day
	}).

-record(
	entity_equipment, {
		entity_id,
		slot,
		item
	}).

-record(
	spawn_pos, {
		x, y, z
	}).
