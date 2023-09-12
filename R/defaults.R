parquet_default = function() getOption("mlr3oml.parquet", FALSE)
test_server_default = function() getOption("mlr3oml.test_server", FALSE)
limit_default = function() getOption("mlr3oml.limit", 5000L)
api_key_default = function(test_serve) get_api_key(get_server(test_serve))

