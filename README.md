# img2orgtable 
Make a screenshot of a table, and get detected table right to your org notes.  

Works not perfect, but at least you get a structure to start

[Demo](assets/demo.mp4)

## Installation
*Prerequisites*:  
+ Python 3.7  
+ Tesseract OCR - [Installation Guide](https://tesseract-ocr.github.io/tessdoc/Installation.html) 

*Installation steps*:
1. Copy the repository 
2. Install deps 
```bash
make install
```
3. In your org file, add the following lines to enable the extension:
```emacs-lisp
(add-to-list 'load-path "/path/to/img2orgtable")
(require 'img2orgtable)
```

