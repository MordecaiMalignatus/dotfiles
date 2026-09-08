function run-puppet --description 'run a noop run on given host' --argument-names host additional_args
        set -l puppetArgs ""
        if not test -z $additional_args
                set -a puppetArgs "$argv[2..]"
        end

        printf "running 'puppet agent -t $puppetArgs'\n"

        ssh $host "sudo /opt/puppetlabs/bin/puppet agent -t $puppetArgs" 
end
