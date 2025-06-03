# HasNotes

This is a haskell markdown parser. The app is designed for users to create their own notes in markdown and parse them to AST which can be converted to terminal freindly output. 

# Requirements
As previously mentioned this app is serves as a markdown parser. I would recommend writing your .md files in your favourite text editor (vim for the win) and then using this app to see how the output will look on your computer.

This app is coded in haskell, and can be run with ```stack run```.

# App flow
- User creates/edits markdown note
   
- Store raw markdown in hasnotes directory
   
- When displaying, parse markdown to AST
   
- Convert AST to formatted terminal output
   
- Display with colors/formatting in terminal
