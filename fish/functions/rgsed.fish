function rgsed --description 'Replace every occurence in PWD found by ripgrep with another one.' --argument old new
        rg $old --files-with-matches | xargs gsed -i "s/$old/$new/g"
end
