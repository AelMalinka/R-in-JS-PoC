A small Proof of Concept for getting R outputs into node

# Requires
- R installed and accessible in path (specifically `Rscript` and `R` executables)

# Build
This is required on first run and anytime `package.json` `build` is modified to add R packages

```
npm run build
```

# Run
Runs the contents of `anthro.R` and ouputs the last object as json

```
npm start
```