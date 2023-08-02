function proto-blogpost
       pushd ~/Sync/Perceptron
       fd --regex '[a-z0-9]{8}.org' | shuf | head -n1 | bb -i '(run! println (reverse (map (fn [s] (str s ".org")) (take 7 (iterate (fn [n] (subs n 0 (- (count n) 1)))(-> (first *input*) (subs 0 8)))))))' | xargs cat | sed '/\#\+/ d' 
       popd
end
