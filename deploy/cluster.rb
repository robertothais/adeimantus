# TODO
# - Fix hot code loading

require 'benchmark'
require 'yaml'
require 'colored'
require 'capistrano/recipes/deploy/scm'
require 'capistrano/recipes/deploy/strategy'
require 'support/cluster'
require 'support/helpers'
require 'support/ejabberd'
require 'uri'
require 'cgi'

# Use the HOSTS environment variable to override
role(:app) { Cluster.instances.map { |i| i.dns_name } }

depend :remote, :command, 'git'
depend :remote, :command, 'erl'
depend :remote, :command, 'ejabberdctl'

namespace :cluster do

  desc <<-DESC
    Launches a new instance, bootstraps it and adds it to the load balancer.
  DESC
  task :launch_instance do
    instance = Cluster.launch_instance do |instance|
      with_env('HOSTS', instance.dns_name) { bootstrap_instance }
    end    
  end

  desc <<-DESC
    Boostraps a running instance. This includes adding it to the ejabberd
    cluster, if there is one. Requires using the HOSTS environment
    variable to indicate in what host to run this task.
  DESC
  task :bootstrap_instance do
    ensure_hosts
    logger.info 'Boostrapping the node'
    logger.info 'Checking out the repository for the first time'
    setup
    update
    run_script 'bootstrap.sh'
    if Cluster.empty?
      ENV['USERNAME'], ENV['PASSWORD'] = Ejabberd.admin.username, Ejabberd.admin.password
      ejabberd.register 
    else
      join_cluster
    end
  end

  desc <<-DESC
    Adds the specified instance to the ejabberd cluster. Requires using
    the HOSTS environment variable to indicate in what host to run this task.
  DESC
  task :join_cluster do
    ensure_hosts
    instances = Cluster.instances.map { |i| "ejabberd@#{i.private_dns_name}" }
    unless instances.empty?
      run_script 'join_cluster.sh', env: { 'EJABBERD_NODES' => instances.join(" ") }
    else
      raise Capistrano::LocalArgumentError, 'No cluster to join'
    end
  end

  desc <<-DESC
    Removes an instance from the load balancer, shuts it down, and prunes
    the ejabberd cluster. If no host is specified via HOSTS, it shut downs
    an instance at random.
  DESC
  task :decomission_instance do
    if Cluster.size == 1
      prompt = "<%= color('This will shut down the last instance in the cluster. All EBS volume data will be lost.', RED) %> Proceed? "
      exit unless agree(prompt) { |q| q.default = 'n' }
    end
    instance_id = ENV['INSTANCE'] || Cluster.random_instance.id
    Cluster.decomission_instance(instance_id)
    prune unless Cluster.empty?
  end

  desc <<-DESC
    Prunes the ejabberd cluster. If a host is specified via HOSTS, it runs
    the prune script in that host. Otherwise it picks an instance at random_instance
    and runs it there.
  DESC
  task :prune do
    logger.info 'Pruning ejabberd cluster'
    host = ENV['HOSTS'] || Cluster.random_instance.dns_name
    run_script 'prune_cluster.sh', hosts: host
  end

  desc 'Describes the running instances attached to the load balancer.'
  task :status do
    in_service =  Cluster.instances
    out_of_service = Cluster.instances_out_of_service
    if in_service.any?
      puts 'In Service:'.green
      puts in_service.inspect
    end
    if out_of_service.any?
      print 'Out of Service: '.red
      puts out_of_service.map { |i| i.id }.join(", ")
    end
    puts "There are #{in_service.length} healthy nodes currently in service".green
    if out_of_service.any?
      puts "There are #{out_of_service.length} unhealthy nodes currently out of service".red
    end
  end

  desc <<-DESC
    Loads the code in the current release into the ejabberd process. This includes 
    modules and configuration files.
  DESC
  task :reload do
    compile_modules
    hot_code_update
  end

  desc <<-DESC
    Compiles all modules in the instance/erlang/src directory of the current
    release and moves the outfiles to ejabberd's module directory.
  DESC
  task :compile_modules do
    run_script 'compile_modules.sh'
  end

  desc <<-DESC
    Runs a hot code update on the ejabberd server.
  DESC
  task :hot_code_update do
    run_script 'hot_code_update.sh'
  end
  
  desc <<-DESC
    Updates the authorized_keys file with the keys/adeimantus.pub file in the local
    machine's working copy.
  DESC
  task :update_keys do
    top.upload("./instance/keys/adeimantus.pub", "#{current_path}/instance/keys/adeimantus.pub")
    run_script 'update_keys.sh'
  end

  desc 'Opens the ejabberd web admin interface.'
  task :admin do
    if Cluster.empty?
      raise Capistrano::LocalArgumentError, 'No cluster to admin'
    end
    url = URI.parse("https://chat.adeimantus.com")
    url.path = '/admin'
    url.user = CGI.escape('admin@chat.adeimantus.com')
    url.password = CGI.escape('scars69:crucible')
    run_locally("open #{url.to_s}")
  end

  desc 'Lauches a local Ruby console with the Cluster class loaded'
  task :console do
    Kernel.exec('irb --prompt simple -r ./deploy/support/cluster.rb -r ./deploy/support/ejabberd.rb')
  end

  desc <<-DESC
    SSH\'s into an instance of the cluster. If no host is specified via
    HOSTS, it picks a one at random.
  DESC
  task :ssh do
    host = ENV['HOSTS'] || Cluster.random_instance.dns_name
    Kernel.exec "ssh #{fetch(:user)}@#{host}"
  end

  namespace :logs do
    desc <<-DESC
      Opens an aggregate stream of the ejabberd log files accross the cluster.
    DESC
    task :default do
      stream 'tail -f /var/log/ejabberd/ejabberd.log'
    end

    desc <<-DESC
      Same as cluster:logs
    DESC
    task :ejabberd do
      default
    end

    desc <<-DESC
      Opens an aggregate stream of the erlang log files accross the cluster.
    DESC
    task :erlang do
      stream 'tail -f /var/log/ejabberd/erlang.log'
    end
  end
end

namespace :ejabberd do
  desc <<-DESC
    Restarts the ejabbed process
  DESC
  task :restart do 
    run "#{sudo} /etc/init.d/ejabberd restart"
  end

  desc <<-DESC
    Starts the ejabberd process
  DESC
  task :start do 
    hosts = ENV['HOSTS'] || Cluster.all_instances.map { |i| i.dns_name }
    run "#{sudo} /etc/init.d/ejabberd start", hosts: hosts
  end

  desc <<-DESC
    Stops the ejabberd process
  DESC
  task :stop do
    run "#{sudo} /etc/init.d/ejabberd stop"
  end
  
  desc <<-DESC
    Copies the ejabberd config files in the instance/config directory of the
    current release to ejabberd's configuration directory.
  DESC
  task :update_config do
    run_script 'update_ejabberd_config.sh'
    restart
  end

  desc 'Registers a user' 
  task :register do
    if ENV['USERNAME'] && ENV['PASSWORD']
      Ejabberd.domains.each do |domain|
        options = {} 
        options[:hosts] = [ Cluster.random_instance.dns_name ] unless Cluster.empty?
        sudo "ejabberdctl register #{ENV['USERNAME']} #{domain} #{ENV['PASSWORD']}",
          options
      end          
    else
      logger.important 'Must supply USERNAME and PASSWORD'
    end
  end

  desc 'Unregisters a user' 
  task :unregister do
    if ENV['USERNAME']
      Ejabberd.domains.each do |domain|
        options = {} 
        options[:hosts] = [ Cluster.random_instance ] unless Cluster.empty?
        sudo "ejabberdctl unregister #{ENV['USERNAME']} #{domain}",
          options
      end
    else
      logger.important 'Must supply USERNAME'
    end
  end
end

desc <<-DESC
  Describes all servers in the Adeimantus EC2 infrastructure. Pass the NAME variable 
  to filter by name
DESC
task :servers do
  servers = Cluster.connection.servers.all
  if name = ENV['NAME']
    servers.reject! { |n| n.tags['Name'].empty? || n.tags['Name'] != name }
  end
  puts servers.inspect
end

desc <<-DESC
  Launches a fog shell. This is a lower level interface to the adeimantus EC2 infrastructure.
  For a higher level console, run:

    $ cap cluster:console
DESC
task :fog do
  Kernel.exec('env FOG_RC=./.fog fog')
end