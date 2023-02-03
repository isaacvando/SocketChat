# SocketChat
## Build & Run
Install Haskell using the command [here](https://www.haskell.org/ghcup/) and choose all the default options. 

In this directory execute the following.
```bash
cabal build
```
This will take a while to download and install the required packages.

Then to start the server do
```bash
cabal run Server
```
And in a separate terminal to start the client do
```bash
cabal run Client
```