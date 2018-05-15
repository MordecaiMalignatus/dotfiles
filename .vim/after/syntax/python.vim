syntax clear pythonOperator

syntax match pyNiceOperator "\<=\>" conceal cchar=←
syntax match pyNiceOperator "\<in\>" conceal cchar=∈
syntax match pyNiceOperator "\<or\>" conceal cchar=∨
syntax match pyNiceOperator "\<and\>" conceal cchar=∧
syntax match pyNiceOperator "\%(is \)\@<!\<not\%( \|\>\)" conceal cchar=¬
syntax match pyNiceOperator "\<not in\>" conceal cchar=∉
syntax match pyNiceOperator "<=" conceal cchar=≤
syntax match pyNiceOperator ">=" conceal cchar=≥

syntax keyword pyNiceOperator sum conceal cchar=∑
syntax match pyNiceKeyword "\<for\>" conceal cchar=∀
syntax match pyNiceKeyword "\<return\>" conceal cchar=◀
syntax match pyNiceKeyword "\<def\>" conceal cchar=※
syntax match pyNiceKeyword "\<class\>" conceal cchar=§
syntax keyword pyNiceStatement lambda conceal cchar=λ
syntax keyword pyNiceStatement None conceal cchar=∅

hi link pyNiceOperator Operator
hi link pyNiceStatement Statement
hi link pyNiceKeyword Keyword
hi! link Conceal Operator

setlocal conceallevel=1
function! ToggleConcealLevel()
    if &conceallevel == 0
        setlocal conceallevel=2
    else
        setlocal conceallevel=0
    endif
endfunction

nnoremap <silent> <C-c><C-y> :call ToggleConcealLevel()<CR>
