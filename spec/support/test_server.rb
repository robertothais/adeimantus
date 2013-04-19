require 'rack/handler/thin'
require 'timeout'

class TestServer

  DEFAULT_TIMEOUT = 2

  def start(port)
   @listener_thread = Thread.new do
      Rack::Handler::Thin.run(Serve(), :Port => port) do |thin| 
        @thin = thin 
        @thin.silent = true 
      end
    end
  end

  def stop
    @thin.stop!
    @listener_thread.exit
  end

  def on_request(&blk)
    sleep 2
    @handler = blk
  end

  def handle(request)
    if @handler      
      begin
        @handler.call(request)
      rescue => e
        @test_thread.raise(e)
      end
    end
    @test_thread.run
  end

  def wait_for(timeout = nil, &blk)
    timeout ||= DEFAULT_TIMEOUT
    begin
      Timeout::timeout(timeout) do 
        @test_thread = Thread.current
        blk.call
        Thread.stop
      end
    ensure
      @handler = nil
      @test_thread = nil
    end
  end

  def Serve
    server = self
    Class.new do
      @server = server
      def self.call(env)
        req = Rack::Request.new(env)
        @server.handle(req)
        [200, {}, []] 
      end
    end
  end
end