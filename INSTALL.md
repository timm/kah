# INSTALL

This whole system is one file [src/kah.lua](src/kah.lua)
|Essential      | Optional  | Install   | Make directory  |
|--------------:|-----------|:----------:|:--------------:|
|               | ~/tmp     |            |  y             |
|               | ~/.vim    |            |  y             |
|               | ~/.vim/bundle |        |  y             |
| lua > 5.4     |           | y ||
| make          |           | y ||
|               | gawk > 5.1| y ||
|               | vim > 9.0 | y ||
|               | git       | y || 
|               | pycco     | y ||
|               | a2ps      | y ||
|               | ps2pdf    | y ||

## If you like vim...

```
mkdir -p ~/.vim
if [[ ! -d ~/.vim/bundle ]]; \
then git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim; \
fi
vim +'PluginInstall --sync' +qa
```


