function cfmt
	cargo fmt
    find ./src -type f -name \*.bk | xargs rm
end
