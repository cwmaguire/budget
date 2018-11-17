PROJECT = budget
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1
DEPS = xxhash epgsql

dep_xxhash = git https://github.com/pierreis/erlang-xxhash.git
dep_epgsql = git https://github.com/epgsql/epgsql.git

include erlang.mk
