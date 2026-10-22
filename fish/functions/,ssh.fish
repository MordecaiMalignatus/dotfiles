function ,ssh --argument-names profile
        set -l region "eu-central-1"
        if test -z $profile
                set profile "htg-main"
        end
        set -l ec2output (aws ec2 describe-instances --profile $profile --region $region | jq '.Reservations[].Instances[]')
        echo $ec2output | jq -r '.PrivateIpAddress' > /tmp/ssh-ip-addresses
        echo $ec2output | jq -r '.InstanceId' > /tmp/ssh-instance-ids
        echo $ec2output | jq -r '.Tags[] | select ( .Key == "Name" ) | .Value' > /tmp/ssh-instance-names

        set -l target (paste /tmp/ssh-ip-addresses /tmp/ssh-instance-ids /tmp/ssh-instance-names | fzf )
        set -l target_ip (echo $target | awk '{print $1}')

        if test -z $target_ip
                return 1
        end

        ssh ubuntu@$target_ip
end
