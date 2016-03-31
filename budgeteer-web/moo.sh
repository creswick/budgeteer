#!/bin/bash

dbname=budgeteer
username=budgeteer
dbhost=localhost
#DBM_DATABASE="user=${username} password=${password} host=${dbhost} dbname=${dbname}"
export DBM_DATABASE="user=${username} host=${dbhost} dbname=${dbname}"
export DBM_DATABASE_TYPE=postgresql
export DBM_MIGRATION_STORE=migrations


stack exec moo -- "$@"
