# FPSurvey
FPSurvey is a small web-survey written with Haskell and Yesod.

[![Build Status](https://travis-ci.org/ForestPhoenix/FPSurvey.svg?branch=master)](https://travis-ci.org/ForestPhoenix/FPSurvey)

Requirements:
- The haskell stack from http://docs.haskellstack.org/en/stable/README/
- PostgreSQL from http://www.postgresql.org/download/

Building the project & running the Unit-tests:
- run `stack build` to inside the root of your checkout to build the project
- run `stack test` to run the tests

Setting up the database:
- The website assumes you can authenticate without submitting a password:
  - On Windows: https://wiki.postgresql.org/wiki/Configuring_for_single_sign-on_using_SSPI_on_Windows
  - On Linux: Create a database user with the same username as your username on the system and configure the `peer` authentication in pg_hba.conf (the `postgres` user in your pg_hba.conf can serve as an example) (http://www.postgresql.org/docs/9.4/static/auth-methods.html)
- the Website uses the database "letsseesurvey" with the schema and questions inside
  - `psql -c "CREATE DATABASE letsseesurvey"` to create the database
  - `psql -d letsseesurvey -f database/recreate.sql` to create the schema
  - `psql -d letsseesurvey -f database/insert.sql` to insert the survey
  - run `SELECT make_participant();` from psql or pgAdmin to create a new User token in the `participants` table

Running the Project:
- execute `yesod devel` in the project directory to start the yesod development server with FPSurvey
