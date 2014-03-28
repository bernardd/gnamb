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
