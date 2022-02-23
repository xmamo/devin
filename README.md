# Devin

## macOS installation instructions

1. Install [Homebrew](https://brew.sh/);

2. Install GTK+ 3, GtkSourceView 3 and GObject Introspection:

   ```sh
   brew install gtk+3
   brew install gtksourceview3
   brew install gobject-introspection
   ```

3. Install GHC and Cabal:

   ```sh
   brew install ghc
   brew install cabal-install
   ```

4. Update the list known of Cabal packages:

   ```sh
   cabal update
   ```

5. Build the program:

   ```sh
   XDG_DATA_DIRS="${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/:}$(brew --prefix)/share/" cabal build all
   ```

   If the command above fails, retry with `sudo`[^1]. If you do, execute the following commands as
   well:

   ```sh
   test -e ./dist && sudo chown -R "$(whoami)" ./dist
   test -e ./dist-newstyle && sudo chown -R "$(whoami)" ./dist-newstyle
   sudo chown -R "$(whoami)" ~/.cabal
   ```

   Building the application the first time will take a lot of time: this is normal!

6. Run the program:

   ```sh
   cabal run devin
   ```


## Debian installation instructions

1. Install the GTK+ 3 and GtkSourceView 3 libraries:

   ```sh
   sudo apt install libgtk-3-dev
   sudo apt install libgtksourceview-3.0-dev
   ```

2. Install the GObject introspection data handling library:

   ```sh
   sudo apt install libgirepository1.0-dev
   ```

3. Install GHC and Cabal:

   ```sh
   sudo apt install ghc
   sudo apt install cabal-install
   ```

4. Update the list known of Cabal packages:

   ```sh
   cabal update
   ```

5. Build the program:

   ```sh
   cabal build all
   ```

   Building the application the first time will take a lot of time: this is normal!

6. Run the program:

   ```sh
   cabal run devin
   ```


## Windows installation instructions

TBD


[^1]: On macOS Monterey, repeatedly executing the command yields different errors every time. If you
      try long enough, you’ll find that one of the possible reasons the command fails is because it
      can’t run `touch`. Clearly, `touch` is a trivial command: there has to be some kind of problem
      regarding permissions.
