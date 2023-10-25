# Devin

This repository contains the Haskell source code for the Devin software
developed and discussed in my thesis, and the LaTeX source of the thesis itself.

## Running Devin on your machine

Precompiled Devin executables can be found in the
[latest release](https://github.com/xmamo/devin/releases/latest).

If you wish to run Devin, GtkSourceView 3 needs to be installed on your system.

* On Debian, you can install it with:

  ```Shell
  sudo apt install libgtksourceview-3.0-1
  ```

* On macOS, you can install it with:

  ```Shell
  brew install gtksourceview3
  ```

* On Windows, you need to install [MSYS2](https://www.msys2.org). Open a MSYS2
  shell and run:

  ```Shell
  pacman -S mingw-w64-x86_64-gtksourceview3
  ```
