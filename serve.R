library(httpuv)
runServer(host = "127.0.0.1", port = 8080,
          list(
            staticPaths = list(
              "/" = staticPath(
                "./docs",
                headers = list(
                  "Content-type" = "text/html; charset=utf-8",
                  "Cross-Origin-Opener-Policy" = "same-origin",
                  "Cross-Origin-Embedder-Policy" = "require-corp"
                )
              )
            )
          )
)
