# Documentation

The documentation you are currently reading can be found on the branch [book](https://github.com/CharlyCst/zephyr/tree/book). This documentation is built using rust's [mdbook](https://github.com/rust-lang/mdBook) project. You can find instructions to install it on the project's homepage.

The documentation can be compiled using our makefile. It will populate the `docs/*` folder.
```sh
make book
```

If you want to work on the documentation and preview your changes in real time, you can use the command `mdbook serve` from within the `book/*` directory. This will provide you with a local website reloading on sources change.
