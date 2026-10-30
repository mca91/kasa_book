# webR browser check

The book uses webR 0.4.2 (R 4.4.1). Keep package repositories compatible with
that runtime. The old R-universe overrides caused `readRDS(dest): unknown input
format` after installing `evaluate`, aborting all widgets during `knitr` startup.
The standard webR repository supplies the chapter packages, including `nloptr`.

Render the book, serve `docs` over HTTP, and check Synthetic Control:

```sh
quarto render --to live-html
python3 -m http.server 8000 --directory docs
```

In another terminal, with Node and Playwright installed:

```sh
node scripts/check-webr.mjs http://localhost:8000/SyntheticControl.html
```

To keep test dependencies outside the book checkout:

```sh
npm install --prefix /tmp/kausalmlr-browser-check playwright
/tmp/kausalmlr-browser-check/node_modules/.bin/playwright install chromium
PLAYWRIGHT_MODULE=/tmp/kausalmlr-browser-check/node_modules/playwright/index.mjs \
  node scripts/check-webr.mjs http://localhost:8000/SyntheticControl.html
```

The same check accepts the deployed chapter URL. It catches package startup
errors, verifies that the hidden setup loaded the Brexit dataset, and edits and
runs a calculation in a visible widget. Initial package downloads require network
access and can take a few minutes.
