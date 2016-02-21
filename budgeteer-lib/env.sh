
if [ ! -f "budgeteer.cabal" ]
then
    echo "Error: cabal file not found; this script must be sourced from the budgeteer source tree."
else
    export ROOT=`pwd`

    # Path to moo:
    export PATH=$PATH:.cabal-sandbox/bin

    # Set up the dbmigrations environment variables.
    export DBM_DATABASE_TYPE=postgresql
    export DBM_DATABASE=postgresql:///budgeteer
    export DBM_MIGRATION_STORE=$ROOT/migrations
fi
