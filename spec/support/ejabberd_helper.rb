# Sets up ejabberd for testing
# Needs access to an ejabberd.yml file in the same directory as this file
# It should have this structure, with values appropriate to the local system
#
# ejabberd:
#   config:
#     host: localhost
#     port: 5222
#     include_dir: /usr/local/lib/ejabberd/include
#     ebin_dir: /usr/local/lib/ejabberd/ebin
#     config_dir: /usr/local/etc/ejabberd
#   commands:
#     start: 'ejabberdctl start'
#     stop:  'ejabberdctl stop'

require 'socket'
require 'colored'
require 'fileutils'
require 'tempfile'
require 'yaml'

module Ejabberd

  CONFIG = YAML.load_file(File.join(File.dirname(__FILE__), 'ejabberd.yml'))

  class << self  
    CONFIG['ejabberd']['config'].each do |key, val|
      define_method(key) { val }
    end

    CONFIG['ejabberd']['commands'].each do |key, val|
      define_method(key) { Kernel.system(val) }
    end
  end 

  module_function  

  def is_running?
    begin
      socket = TCPSocket.open(host, port)
      socket.close
      true
    rescue Errno::ECONNREFUSED
      false
    end
  end

  def compile
    log = Tempfile.new('ejabberd-compile')
    errors = 0
    base_dir = File.join(File.dirname(__FILE__), '..', '..', 'instance', 'erlang')
    Dir.glob(File.join(base_dir, 'src', '**/*.erl')) do |f|
      Kernel.system("erlc -I #{include_dir} -I #{File.join(base_dir, 'include')} -o #{File.dirname(f)} #{f} >> #{log.path}")
      out = File.join(File.dirname(f), File.basename(f, '.erl') + '.beam')
      success = File.exists?(out)
      unless success
        errors += 1
        next
      end
      FileUtils.chmod 0644, out 
      FileUtils.mv out, ebin_dir
    end
    output = log.read
    log.close
    log.unlink
    { errors: errors, output: output }
  end

  def copy_config
    FileUtils.cp(File.join(File.dirname(__FILE__), '..', '..', 'instance', 'config', 'ejabberd.cfg'), config_dir)
  end

  def wait_for
    until yield 
      sleep(1.5)
    end
  end

  def prepare
    print 'Preparing ejabberd...'
    stop if is_running? 
    wait_for { !is_running? }
    copy_config
    result = compile
    if result[:errors] == 0
      start
      wait_for { is_running? }
      puts "[ #{'done'.green} ]"
      true
    else
      puts "[ #{'compilation errors'.red} ]"
      puts result[:output]
      false
    end
  end

end