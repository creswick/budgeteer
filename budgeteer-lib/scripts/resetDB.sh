#!/bin/bash
set -e


DBNAME=budgeteer

HERE=$(cd `dirname $0`; pwd)
ROOT=$HERE/..

MOO=$ROOT/.cabal-sandbox/bin/moo

# drop it if it exists, to reset things:
dropdb --if-exists $DBNAME

# create the db:
createdb $DBNAME

# set up the schema:
$MOO upgrade

