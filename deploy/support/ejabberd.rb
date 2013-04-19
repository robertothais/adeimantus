module Ejabberd

  module_function

  def config
    @config ||= YAML.load(File.open(File.join(File.dirname(__FILE__), "ejabberd.yml")))
  end

  def admin
    @admin ||= Struct.new(:username, :password).new(config['admin']['username'], config['admin']['password'])
  end

  def domains
    config['domains']
  end

end