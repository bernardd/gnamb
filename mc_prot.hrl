%% Login state

%0
-record(
	disconnect, {
		reason
	}).

%1
-record(
	encryption_request, {
		server_id,
		public_key,
		verify_token
	}).

%2
-record(
	login_success, {
		uuid,
		username
	}).

%% Running state

%0
-record(
	keep_alive, {
		id
	}).

%1
-record(
	join_game, {
		entity_id,
		game_mode,
		dimension,
		difficult,
		max_players,
		level_type
	}).

%2
-record(
	chat_message, {
		message
	}).

%3
-record(
	time_update, {
		world_age,
		time_of_day
	}).

%4
-record(
	entity_equipment, {
		entity_id,
		slot,
		item
	}).

%5
-record(
	spawn_pos, {
		x, y, z
	}).

%8
-record(
	player_position_and_look, {
		x, y, z,
		yaw, pitch, on_ground
	}).

%9
-record(
	held_item_change, {
		slot
	}).

%37
-record(
	statistics, {
		statistics
	}).

%38
-record(
	player_list_item, {
		name,
		online,
		ping
	}).

%39
-record(
	player_abilities, {
		flags,
		flying_speed,
		walking_speed
	}).

%3F
-record(
	plugin_message, {
		channel,
		data
	}).
