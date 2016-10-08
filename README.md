# Bad Values

This R-package offers functions to facilitate the handling of bad values.

# Introducing the bad value concept
- I choose the term "bad values" for cells in a data frame that should be used under reserve or even removed from the data.  
- The bad value concept should enhance the traceability: Instead of simply removing those values after detection they can be marked for later removal. 
- Therefore it is straightforward to keep the whole data frame 'as-is' while removing the "bad values" only when needed.

# Step-by-step explanation
1. Add a column for indices of bad values to your data frame (e.g. mydata$BADVAL <- NA)

2. Use 
```
badval_add()
``` 
to add bad value indices to your badval_column. Check output and update the column of your dataframe.  

3. Clean the bad values of your data. It is possible to clean all bad values at once or specify a pattern or an exact string for specific column names: 
```
badval_clean_data()
```

4. Remove the bad values from the badval_column if you do not need them anymore: 
```
badval_rm_index()
```
