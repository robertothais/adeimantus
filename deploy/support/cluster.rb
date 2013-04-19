require 'fog'
require 'colored'
require 'erb'

module Cluster

  def launch_instance
    log 'Starting up instance... '
    instance = connection.servers.create({
      image_id:           config['ami'],
      groups:             config['security_group'],
      flavor_id:          config['instance_type'],
      key_name:           config['key_name'],
      availability_zone:  config['availability_zone'],
      user_data:          user_data,
      monitoring:         true,
      tags:               { 'Name' => 'adeimantus' },
      block_device_mapping:  
        [{
          'Ebs.VolumeSize' => config['root_partition_size'],
          'DeviceName' => "/dev/sda1",
          'Ebs.DeleteOnTermination' => true
        }]
    })
    log :ok
    log 'Waiting for instance to become available... '
    instance.wait_for { sshable? }
    log :ok
    yield(instance) if block_given?
    log 'Registering new instance with load balancer... '
    load_balancer.register_instances(instance.id)
    log :done
  end

  def user_data
    base_path = File.join(File.dirname(__FILE__), '..', '..', 'instance')
    template = File.open(File.join(base_path, 'scripts', 'user_data.sh.erb')) { |f| f.read }
    public_keys = File.open(File.join(base_path, 'keys', 'adeimantus.pub')) { |f| f.read }
    # Create the ejabberd node names using dns names
    ejabberd_nodes = nodes.map { |n| n.private_dns_name }
    ERB.new(template).result(binding)
  end

  def instances_in_service
    ids = load_balancer.instances_in_service
    unless ids.empty?
      connection.servers.all('instance-id' => ids)    
    else 
      []
    end
  end
  alias :nodes     :instances_in_service
  alias :instances :instances_in_service

  def size
    instances.length
  end

  def empty?
    size == 0
  end

  def instances_out_of_service
    ids = load_balancer.instances_out_of_service
    unless ids.empty?
      connection.servers.all('instance-id' => ids)    
    else 
      []
    end
  end

  def all_instances
    instances + instances_out_of_service
  end

  def random_instance
    instances.to_a.sample(1).first
  end

  def decomission_instance(instance_id)
    instance = connection.servers.get(instance_id)
    log "Decomissioning instance #{instance.id} with hostname #{instance.dns_name}\n"
    log 'Deregistering instance from load balancer... '
    load_balancer.deregister_instances(instance_id)
    log :ok
    log 'Shutting down instance... '
    instance.destroy
    log :done
    Cluster.reload
  end

  def config
    @config ||= YAML.load(File.open(File.join(File.dirname(__FILE__), "cluster.yml")))
  end

  def connection 
    @connection ||= Fog::Compute.new({
      provider: 'AWS',
      aws_secret_access_key: config['secret_access_key'],
      aws_access_key_id:     config['secret_access_key_id']
    })
  end

  def load_balancer
    return @load_balancer if @load_balancer
    elb = Fog::AWS::ELB.new({
      aws_secret_access_key: config['secret_access_key'],
      aws_access_key_id:     config['secret_access_key_id']
    })
    @load_balancer = elb.load_balancers.get(config['load_balancer_name'])    
  end

  def reload
    @connection = nil
    @load_balancer = nil
  end

  def log(message)
    case message
    when :ok then puts "[ #{'ok'.green} ]"
    when :done then puts "[ #{'done'.green} ]"
    when :error then puts "[ #{'error'.green} ]"
    else print message
    end
  end

  extend self
end