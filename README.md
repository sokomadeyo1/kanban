# kanban

Kanban board written in haskell with yesod.

## Installing and running

1. Install haskell and stack build tool, e.g. by using
   [ghcup](https://www.haskell.org/ghcup/) or executing stack installation
   script:

   ```shell
   curl -sSL https://get.haskellstack.org/ | sh
   ```

2. Install sqlite3 using your packet manager.

3. Clone and build this project, installing dependencies:

   ```shell
   git clone https://github.com/sokomadeyo1/kanban.git && cd kanban
   stack build
   ```

4. Setup the database by executing `scripts/setup_db.sh`

5. Run the application with `stack run` or `stack exec kanban-exe`

6. View the site at <https://localhost:3000/>

## Project structure

### Backend

- `Domain` modules describe the data model of the application and tie it to the
  data model in DB.
- `Persistence.Sqlite` module contains all the database interactions used in
  the project.
- `Usecase` modules contain all the project logic, such as checking if moving
  between certain columns is restricted when trying to move entries.

### API/Frontend

- `config/routes.yesodroutes` contains specification of the supported HTTP
  methods.
- `Foundation` module declares the web-application's routes and types used for
  HTTP response construction.
- `Handler` modules make up the controller, processing user interactions and
  use hamlet and lucius templates from `templates/` to build user interface.
- `Application` module provides a function for launching the app and sets up
  route dispatching, which requires all handler functions to be defined.

## Roadmap

For further development, the following problems can be addressed:

- [ ] Rewriting the `Persistence.Sqlite` module using
  [`yesodweb/persistent`](https://github.com/yesodweb/persistent) library that
  uses an EDSL which ensures more type safety and aims to catch every possible
  error at compile time. It also features automatic database migrations.
- [ ] Adding database indexes.
- [ ] Adding functionality for working with several boards.
- [ ] Adding multi-user support.
- [ ] Updating frontend.
