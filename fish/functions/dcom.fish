function dcom --wraps=docker-compose --description 'alias dcom=docker-compose'
  docker-compose $argv; 
end
