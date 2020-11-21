# KashCal - A spreadsheet

## Yet to do

- Expression Language
  - Explore supporting expressions referring to other expression values
  - Explore a performant architecture for updating cells and recalculating expressions
    - [How to recalculate a spreadsheet](https://lord.io/blog/2020/spreadsheets/)
    - [Write your own excel in 100 lines of F#](http://tomasp.net/blog/2018/write-your-own-excel/)
  - Add BODMAS based expression evaluation
  - Allow whitespace in expressions
  - Add support for range based formulas
    - SUM, AVG
- UI
  - Add formatting support to cells 
    - bold, underscore, italic
    - font size, font color
    - background color / highlighting
  - Adjust dimensions of rows, columns and cells
  - Dynamically add more rows and columns
  - Multi select cells, entire rows and columns
  - Multiple sheet/tab support
  - Pinning/freezing rows
  - Delete rows and columns
  - Hide columns
  - Reposition rows and columns (adjust their index)
- Persistence
  - Explore how to persist a spreadsheet (locally first)
