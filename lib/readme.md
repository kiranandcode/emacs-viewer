# Lib

Backend server for the project implemented in Dream.

The high level project structure is as follows:

- *server.ml* - Main entrypoint for backend.

- *decoder.ml* - Implements a robust failure-tolerant decoder to extract org-mode data into our internal representation

- *emacs.ml* - encapsulates any remote process calls via emacs-client used to retrieve the state of emacs.

- *state.ml* - Key data structure used by the backend to cache requests to Emacs

