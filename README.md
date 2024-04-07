# Blaze Generator

Welcome to the Blaze Generator repository! This tool is designed for Haskell developers who want to seamlessly integrate HTML templates into their Haskell code using the powerful [blaze-html](https://jaspervdj.be/blaze/) library. `blaze-html` is a combinator library that allows for embedding HTML templates directly within Haskell code, providing a type-safe way to generate dynamic HTML content.

## Overview

The Blaze Generator takes your static HTML files and converts them into Haskell code that uses `blaze-html` syntax. This conversion process allows you to work more efficiently by writing your web UI in HTML and then automatically converting it into Haskell code, ready to be integrated into your web applications.

## Example

Consider you have the following HTML code for a login page:

```html
<html lang="en">
<head>
  <title>ScottyCrud - Login Page</title>
</head>
<body>
  <div class="main">
    <nav class="header-bar navbar navbar-expand-lg navbar-dark">
      <form class="d-flex ms-auto my-lg-0">
        <input class="form-control me-2" type="search" placeholder="Search" aria-label="Search" />
        <button class="btn btn-outline-light" type="submit">Search</button>
      </form>
      <button class="btn btn-outline-light ms-2" type="button">Login</button>
    </nav>
    <div class="footer-bar">
      <p>Footer here</p>
    </div>
  </div>
</body>
</html>
```

To convert this HTML file into `blaze-html` syntax, you would use the Blaze Generator like so:

```bash
cabal exec blaze-generator "/path/to/your/html/file/login.html"
```

The output will be Haskell code in `blaze-html` syntax:

```haskell
html ! lang "en" $ do
  head $ do
    title $ "ScottyCrud - Login Page"
  body $ do
    div ! class "main" $ do
      nav ! class "header-bar navbar navbar-expand-lg navbar-dark" $ do
        form ! class "d-flex ms-auto my-lg-0" $ do
          input ! aria-label "Search" ! class "form-control me-2" ! placeholder "Search" ! type "search"
          button ! class "btn btn-outline-light" ! type "submit" $ "Search"
        button ! class "btn btn-outline-light ms-2" ! type "button" $ "Login"
      div ! class "footer-bar" $ do
        p $ "Footer here"
```

## Getting Started

To get started with Blaze Generator, clone this repository and build the project using Cabal or Stack.

```bash
cabal build
```

```bash
cabal exec blaze-generator "path/to/html/file"
```

## Future Improvements

- **HTML Comments**: Currently, the Blaze Generator does not support converting HTML comments into Haskell comments. We plan to add this feature in a future release to ensure that comments in your HTML templates are preserved in the generated Haskell code.

Stay tuned for more updates and improvements to the Blaze Generator. We're constantly looking to enhance this tool to better serve the Haskell web development community.