require 'highline/import'

set :application, 'adeimantus'
set :repository,  'git@github.com:rthais/adeimantus.git'
set :user, 'ubuntu'
set :deploy_to, "/home/ubuntu/#{application}"
set :use_sudo, false

set :deploy_via, :checkout
set :scm, :git
set :git_shallow_clone, 1

set :ssh_options, forward_agent: true

if ENV['DEPLOY_FROM_LOCAL']
  exit unless agree("<%= color('If deploying, this will push the code in your local machine\\'s working copy.', YELLOW) %> Proceed? ") { |q| q.default = 'n' }
  set :scm, :none
  set :repository, "."
  set :deploy_via, :copy
end

set(:source)            { Capistrano::Deploy::SCM.new(scm, self) }
set(:revision)          { source.head }
set(:real_revision)     { source.local.query_revision(revision) { |cmd| with_env("LC_ALL", "C") { run_locally(cmd) } } }

set(:strategy)          { Capistrano::Deploy::Strategy.new(deploy_via, self) }

set(:release_name)      { set :deploy_timestamped, true; Time.now.utc.strftime("%Y%m%d%H%M%S") }

set :version_dir,       "releases"
set :shared_dir,        "shared"
set :shared_children,   %w()
set :current_dir,       "current"

set(:releases_path)     { File.join(deploy_to, version_dir) }
set(:shared_path)       { File.join(deploy_to, shared_dir) }
set(:current_path)      { File.join(deploy_to, current_dir) }
set(:release_path)      { File.join(releases_path, release_name) }

set(:releases)          { capture("ls -x #{releases_path}", :except => { :no_release => true }).split.sort }
set(:current_release)   { releases.length > 0 ? File.join(releases_path, releases.last) : nil }
set(:previous_release)  { releases.length > 1 ? File.join(releases_path, releases[-2]) : nil }

set(:current_revision)  { capture("cat #{current_path}/REVISION",     :except => { :no_release => true }).chomp }
set(:latest_revision)   { capture("cat #{current_release}/REVISION",  :except => { :no_release => true }).chomp }
set(:previous_revision) { capture("cat #{previous_release}/REVISION", :except => { :no_release => true }).chomp if previous_release }

set(:run_method)        { fetch(:use_sudo, true) ? :sudo : :run }

# some tasks, like symlink, need to always point at the latest release, but
# they can also (occassionally) be called standalone. In the standalone case,
# the timestamped release_path will be inaccurate, since the directory won't
# actually exist. This variable lets tasks like symlink work either in the
# standalone case, or during deployment.
set(:latest_release) { exists?(:deploy_timestamped) ? release_path : current_release }