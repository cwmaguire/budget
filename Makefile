PROJECT = budget
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1
DEPS = xxhash epgsql jsx cowboy

dep_xxhash = git https://github.com/pierreis/erlang-xxhash.git
dep_epgsql = git https://github.com/epgsql/epgsql.git
dep_jsx = git https://github.com/talentdeficit/jsx.git develop
dep_cowboy = git https://github.com/ninenines/cowboy.git 2.5.0

include erlang.mk
