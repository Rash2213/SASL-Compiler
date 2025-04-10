# Demo Compiler

This is a demo implementation of SASL. You can use it as follows:

```bash
cat prelude.sasl myfile.sasl | ./<sasl-for-my-machine>
```

We've compiled this implementation for 64 bit machines running Windows, mac OS, or Linux. Be sure to use the correct version for your machine.

> [!WARNING]
> This compiler has a few quirks, so beware:
> - Mixed case identifiers like `pAnd` cause errors, use `pand` instead.
> - The operator precedence (esp for `where`) is not always correct, just use a superfluous amount of parentheses to be extra sure.

> [!CAUTION]
> This implementation was written by Dan Piponi (aka. `sigfpe`). If you find the source code out the wild, it won't be of much use to you for this project, as it functions differently from the implementation you are supposed to write.
