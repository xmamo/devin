# Devin

This repository contains the Haskell source code for the Devin software, which
is discussed in my thesis. It also includes the LaTeX source of the thesis
itself.

## Running Devin on your machine

Precompiled Devin executables can be found in the
[latest release](https://github.com/xmamo/devin/releases/latest).

To run Devin, GtkSourceView 3 and its dependencies need to be installed on your
system.

* On Debian derivatives, you can install them with:

  ```Shell
  sudo apt install libgtksourceview-3.0-1
  ```

  Before launching Devin, ensure you have granted execution permissions:

  ```Shell
  chmod +x devin-x86_64-linux
  ```

* On macOS, you can install them with [Homebrew](https://brew.sh/):

  ```Shell
  brew install gtksourceview3
  ```

  Before launching Devin, ensure you have granted execution permissions:

  ```Shell
  chmod +x devin-aarch64-osx
  ```

* On Windows, you can install them with [MSYS2](https://www.msys2.org/).

  If you don’t have MSYS2 already installed, run:

  ```Batchfile
  winget install --id MSYS2.MSYS2 --interactive
  ```

  In a MSYS2 shell, execute the following command:

  ```Shell
  pacman -S mingw-w64-x86_64-gtksourceview3
  ```

  You may launch Devin from MSYS2 using:

  ```Shell
  ./devin-x86_64-windows.exe
  ```
