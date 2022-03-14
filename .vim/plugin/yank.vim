function Osc52Yank()
    let buffer=system('base64 | tr -d "\n"', @0)
    let buffer='\033]52;c;'.buffer.'\033\'
    silent exe "!echo -ne ".shellescape(buffer)." > ".shellescape(g:tty)
endfunction

augroup Yank
    autocmd!
    autocmd TextYankPost * if v:event.operator ==# 'y' | call Osc52Yank() | endif
augroup END
