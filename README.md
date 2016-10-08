# Bad Values

This R-package offers functions to facilitate the handling of bad values.

# Introducing the bad value concept
- I choose the term "bad values" for cells in a data frame that should be used under reserve or even removed from the data.  
- The bad value concept should enhance the traceability: Instead of simply removing those values after detection they can be marked for later removal. 
- Therefore it is straightforward to keep the whole data frame 'as-is' while removing the "bad values" only when needed.
